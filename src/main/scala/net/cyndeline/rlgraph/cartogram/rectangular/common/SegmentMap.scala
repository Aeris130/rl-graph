package net.cyndeline.rlgraph.cartogram.rectangular.common

import archery.{Box, Entry, Geom, RTree}
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.Rectangle

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Parses the segments that bounds a rectangle in a rectangular dual into maximal segments, and stores the start/stop
 * value for each rectangle along each axis.
 *
 * Example, if two rectangles share their top segment along the x axis from (3, 4)[start R1] to
 * (6, 4)[stop R1, start R2] to (8, 4)[stop R2] then the single x segment [(3, 4), (8,4)] will be present in the map.
 *
 * @param initialLayout Rectangles and gates used to construct the initial segment values from. These area object
 *                      will be used to retrieve segments after each optimization, despite that their initial
 *                      coordinates may not correspond to the segments any longer.
 */
class SegmentMap[V, E[X] <: UnDiEdge[X]](initialLayout: RectangularLayout[V, E]) {
  private val segmentMap: mutable.HashMap[Rectangle, SegmentsOfRectangle] = computeSegments

  /** Every segment that bounds a rectangle from the left or right in a fixed order based on their variable index. The lowest index is 0. */
  val verticalSegments: Vector[Segment] = segmentMap.values
    .toVector.flatMap(sor => Vector(sor.left, sor.right))
    .distinct
    .sortBy(_.variableIndex)

  /** Every segment that bounds a rectangle from the above or below in a fixed order based on their variable index. The lowest index is 0. */
  val horizontalSegments: Vector[Segment] = segmentMap.values
    .toVector.flatMap(sor => Vector(sor.top, sor.bottom))
    .distinct
    .sortBy(_.variableIndex)

  /** The number of segments in the map. */
  val size: Int = verticalSegments.size + horizontalSegments.size

  /**
   * @param r A rectangular area to retrieve segment data for.
   * @return In bounding order: Left(x), Right(x), Top(y), Bottom(y).
   */
  def segmentOfArea(r: Rectangle): SegmentsOfRectangle = {
    segmentMap(r)
  }

  /**
   * @return A rectangular layout based on the input layout of this segment map, and the new coordinates of its segments.
   */
  def constructNewDrawingFromSegments: RectangularLayout[V, E] = {
    val rectangleEntries = new ListBuffer[(V, (Int, Int), (Int, Int))]()
    val gateEntries = new ListBuffer[((V, V), (Int, Int), (Int, Int))]()
    val dummyEntries = new ListBuffer[((Int, Int), (Int, Int))]()

    for (r <- initialLayout.rectangles) {
      val s = this.segmentOfArea(r._2)
      rectangleEntries += ((r._1, (s.left.value, s.top.value), (s.right.value, s.bottom.value)))
    }

    for (g <- initialLayout.gates) {
      val s = this.segmentOfArea(g)
      gateEntries += (((g.from, g.to), (s.left.value, s.top.value), (s.right.value, s.bottom.value)))
    }

    for (d <- initialLayout.dummies) {
      val s = this.segmentOfArea(d)
      dummyEntries += (((s.left.value, s.top.value), (s.right.value, s.bottom.value)))
    }

    new RectangularLayout(rectangleEntries.toVector, gateEntries.toVector, dummyEntries.toVector, initialLayout.graph)
  }

  /**
   * Setup phase: The maximum segments bounding each area in the initial layout is computed, and each rectangle
   * is mapped to its segment entry containing the x and y coordinates of its position in the layout.
   */
  private def computeSegments: mutable.HashMap[Rectangle, SegmentsOfRectangle] = {
    val segments = new mutable.HashMap[Rectangle, SegmentsOfRectangle]()

    /* RTrees containing non-intersecting coordinate intervals, i.e the maximal intervals, storing the rectangles
     * bounded by them, and the direction of the bound.
     */
    val maximalXSegmentTree = mergeSegments(computeSeparateSegments(X_Axis), X_Axis)
    val maximalYSegmentTree = mergeSegments(computeSeparateSegments(Y_Axis), Y_Axis)

    val leftBound = new mutable.HashMap[Rectangle, Segment]()
    val rightBound = new mutable.HashMap[Rectangle, Segment]()
    val topBound = new mutable.HashMap[Rectangle, Segment]()
    val bottomBound = new mutable.HashMap[Rectangle, Segment]()

    mapRectanglesToSegments(maximalXSegmentTree, 0, leftBound, rightBound, topBound, bottomBound, X_Axis)
    mapRectanglesToSegments(maximalYSegmentTree, 0, leftBound, rightBound, topBound, bottomBound, Y_Axis)

    for (area <- initialLayout.allAreas) {
      segments.put(area, SegmentsOfRectangle(area, leftBound(area), rightBound(area), topBound(area), bottomBound(area)))
    }

    segments
  }

  /**
   * Takes each segment in an RTree and maps its rectangles to a new common segment, split between maps representing
   * the directions the rectangle can be bound from.
   * @return The next segment id assignable.
   */
  private def mapRectanglesToSegments(tree: RTree[Vector[RectangleEntry]],
                                      nextId: Int,
                                      left: mutable.HashMap[Rectangle, Segment],
                                      right: mutable.HashMap[Rectangle, Segment],
                                      top: mutable.HashMap[Rectangle, Segment],
                                      bottom: mutable.HashMap[Rectangle, Segment],
                                      axis: Axis): Unit = {

    val treeBoxToSegment = new mutable.HashMap[Entry[Vector[RectangleEntry]], Segment]()
    var nextAvailableSegmentId = nextId

    for (treeEntry <- tree.entries) {
      val segment = treeBoxToSegment.getOrElse(treeEntry, {
        val axisValue = if (treeEntry.geom.x == treeEntry.geom.x2) treeEntry.geom.x
        else if (treeEntry.geom.y == treeEntry.geom.y2) treeEntry.geom.y
        else throw new Error("Axis segments required to line up on one axis (found " + treeEntry.geom + ").")

        val newSegment = new Segment(nextAvailableSegmentId, axisValue.toInt, axis)
        nextAvailableSegmentId += 1
        treeBoxToSegment.put(treeEntry, newSegment)
        newSegment
      })

      for (rectangleEntry <- treeEntry.value) {
        rectangleEntry.side match {
          case Left => left.put(rectangleEntry.r, segment)
          case Right => right.put(rectangleEntry.r, segment)
          case Top => top.put(rectangleEntry.r, segment)
          case Bottom => bottom.put(rectangleEntry.r, segment)
        }
      }
    }
  }

  /**
   * Computes an RTree with box areas having width or height == 1, depending on weather they represents the x or y
   * segments bounding the rectangles in the initial layout. Every box entry will contain a reference to the rectangle
   * it represents. Multiple rectangles sharing a single size will contain separate entries with separate boxes.
   */
  private def computeSeparateSegments(axis: Axis): RTree[RectangleEntry] = {
    val treeEntries: Vector[Vector[Entry[RectangleEntry]]] = for (area <- initialLayout.allAreas) yield axis match {
      case X_Axis => Vector(leftEntry(area), rightEntry(area))
      case Y_Axis => Vector(topEntry(area), bottomEntry(area))
    }

    RTree(treeEntries.flatten:_*)
  }

  /**
   * Computes maximal segments by merging every segment that shares coordinates. Every rectangle that is merged in this
   * way keeps its original rectangle entry in the new segment.
   */
  private def mergeSegments(separateTree: RTree[RectangleEntry], axis: Axis): RTree[Vector[RectangleEntry]] = {

    /* Begin by assigning every segment in the input tree its own entry in the new tree. Otherwise segments that
     * doesn't intersect anything else won't get added.
     */
    val newEntries = (for (entry <- separateTree.entries) yield Entry(entry.geom, Vector(entry.value))).toVector
    var mergedTree = RTree(newEntries:_*)
    val mergedSegments = new mutable.HashSet[Entry[Vector[RectangleEntry]]]()

    for (entry <- mergedTree.entries if !mergedSegments.contains(entry)) {
      var intersectingSegments = mergedTree.searchIntersection(entry.geom.asInstanceOf[Box], _ != entry)
      var currentSegment = entry

      /* If one or more segments intersect the current one, they'll get merged. In doing so, a new segment is
       * produced that may intersect yet more segments. This process is repeated until a maximal segment is found
       * (one that has no more intersections along this RTree's axis).
       */
      while (intersectingSegments.nonEmpty) {
        mergedSegments.add(currentSegment)
        mergedSegments ++= intersectingSegments
        mergedTree = mergedTree.remove(currentSegment)
        mergedTree = mergedTree.removeAll(intersectingSegments)

        val mergedCoordinateBox = mergeSegments(currentSegment.geom +: intersectingSegments.map(_.geom), axis)
        currentSegment = Entry(mergedCoordinateBox, currentSegment.value ++ intersectingSegments.flatMap(_.value))
        mergedTree = mergedTree.insert(currentSegment)
        intersectingSegments = mergedTree.searchIntersection(currentSegment.geom.asInstanceOf[Box], _ != currentSegment)
      }
    }

    mergedTree
  }

  private def mergeSegments(segments: Seq[Geom], axis: Axis): Box = axis match {
    case X_Axis =>
      val x = segments.head.x
      val yStart = segments.minBy(_.y).y
      val yStop = segments.maxBy(_.y2).y2
      Box(x, yStart, x, yStop)
    case Y_Axis =>
      val y = segments.head.y
      val xStart = segments.minBy(_.x).x
      val xStop = segments.maxBy(_.x2).x2
      Box(xStart, y, xStop, y)
  }

  private def leftEntry(r: Rectangle): Entry[RectangleEntry] = Entry(Box(r.startX, r.startY, r.startX, r.stopY), RectangleEntry(r, Left))
  private def rightEntry(r: Rectangle): Entry[RectangleEntry] = Entry(Box(r.stopX, r.startY, r.stopX, r.stopY), RectangleEntry(r, Right))
  private def topEntry(r: Rectangle): Entry[RectangleEntry] = Entry(Box(r.startX, r.startY, r.stopX, r.startY), RectangleEntry(r, Top))
  private def bottomEntry(r: Rectangle): Entry[RectangleEntry] = Entry(Box(r.startX, r.stopY, r.stopX, r.stopY), RectangleEntry(r, Bottom))

  /** Stored in an RTree to specify which side an interval bounds a rectangle on. */
  private case class RectangleEntry(r: Rectangle, side: Side)

}

case class SegmentsOfRectangle(rectangle: Rectangle, left: Segment, right: Segment, top: Segment, bottom: Segment)
