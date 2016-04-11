package net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic

import net.cyndeline.rlgraph.cartogram.rectangular.common._
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help.MoveDir._
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help.{MovementChange, RectangleEntry, SegmentEntry, ValidMovement}
import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.Rectangle

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge

/**
 * A heuristic that optimizes the size and aspect ratio of rectangles a rectangular dual.
 *
 * Unlike the bilinear programming method this heuristic doesn't guarantee an optimal solution, but it still
 * produces nicely looking cartograms. A short description of the algorithm plus an example layout (and how it
 * compares to the LIP solution) can be found in "A Linear Programming Approach to Rectangular Cartograms" by Bettina
 * Speckmann, Marc van Kreveld and Sander Florisson.
 *
 * The heuristic works by computing the maximal segments of the initial layout. It then iterates over all such
 * segments and attempts to move them in one of the two available directions on the segments axis if doing so will
 * cause the sum of cartographic errors of its adjacent rectangles to decrease. The cartographic error is defined as
 * |A - As(R)| / As(R), where A is the current area of the rectangle and As(R) is the optimal area of the rectangle
 * targeted by the user. This means that users may only specify area sizes, and not specific widths and heights.
 *
 * Other than area, users may also specify a maximum aspect ratio. Segments will not be moved if doing so causes
 * one of the segments adjacent rectangles to exceed the maximum ratio anymore than it currently does. A larger
 * maximum ratio gives the algorithm greater margins to optimize within, but may result in visually suboptimal
 * drawings.
 *
 * Other than error and ratio, the algorithm respects planarity and adjacency of all rectangles as follows:
 *
 *  - The lower segment (left/top) of each rectangle may never exceed the upper segment (right/bottom), doing so
 *  would cause the rectangles area to be inverted, breaking planarity.
 *  - Every adjacent neighbor of a rectangle in the layout must remain adjacent in the final drawing.
 *
 * The algorithm allows the user to specify a set of vertices to not be optimized, i.e areas that doesn't
 * need to be constrained by size or aspect ratios (only planarity and adjacencies). These are known as Sea rectangles.
 * The name stems from their use in the above mentioned article by Speckmann et. al, where the areas on a map that
 * represents water regions surrounding a continent doesn't necessarily need to be as strictly optimized as the
 * actual landmasses inside them. By allowing the algorithm to disregard these areas, the remaining areas have more
 * space available to reach the desired size.
 *
 * Finally, the algorithm does not apply size or aspect ratio constraints to gates in the layout, as the user has not
 * specified such data for them.
 *
 * @param maxAspectRatio The maximum amount of width/height and height/width that each rectangles proportions allows.
 *                       Example: Setting this to 2.5 allows rectangles to be 2.5 times wider than high, or 2.5 times
 *                       higher than wide.
 */
class SegmentHeuristic[VType <: MapArea, EType[X] <: UnDiEdge[X]](maxAspectRatio: Double) {
  private val gateTargetSize = -1 // Gates shouldn't add anything to the cartographic error

  /**
   * @param layout A rectangular layout to optimize.
   * @return The layout that results from the optimization..
   */
  def applyToLayout(layout: RectangularLayout[VType, EType]): RectangularLayout[VType, EType] =
    applyToLayoutWithExceptions(layout, Set())

  /**
   * @param layout A rectangular layout to optimize.
   * @param seaRectangles Vertices in the initial layout that shouldn't have their target size or aspect ratio taken
   *                      into consideration when optimizing.
   * @return The layout that results from the optimization.
   */
  def applyToLayoutWithExceptions(layout: RectangularLayout[VType, EType], seaRectangles: Set[VType]): RectangularLayout[VType, EType] = {
    val moveDirectionsFinder = new ValidMovement()

    /* To avoid giving the right/bottom rectangles more space to expand in, every coordinate in the layout
     * is adjusted by 100 before beginning.
     */
    val adjustedLayout = layout.adjustCoordinates(100, 100)

    val segmentMap = new SegmentMap(adjustedLayout)
    val verticalSegments = createSegmentEntries(X_Axis, adjustedLayout, segmentMap, seaRectangles)
    val horizontalSegments = createSegmentEntries(Y_Axis, adjustedLayout, segmentMap, seaRectangles)

    /* To avoid skewing the rectangles in one direction by optimizing every segment on one axis before moving to the
     * next, the axises are alternated between as long as they both contain non-processed segments.
     */
    val zipped: Vector[SegmentEntry] = zippedSegments(verticalSegments, horizontalSegments)

    var optimizationPossible = true

    do {
      val segments = zipped.toIterator
      optimizationPossible = false

      while (segments.hasNext) {
        val segmentEntry: SegmentEntry = segments.next()

        /* First check which directions the segment is currently allowed to move into. */
        val dirs = moveDirectionsFinder.validMovementDirections(segmentEntry)
        var currentErrorReduction: Option[(Side, Double)] = None

        /* When deciding which direction to move the segment in, the decrease in error for rectangles on both sides
         * of the segment must be taken into account for both directions, otherwise it could result in an infinite loop.
         */
        for (move <- dirs) {
          val direction = segmentEntry.axis match {
            case X_Axis => if (move == Down) Left else Right
            case Y_Axis => if (move == Down) Top else Bottom
          }

          val movementResults = segmentEntry.rectangles.map(_.canMove(direction))

          if (movementAllowed(movementResults)) {
            val errorReduction = errorChange(movementResults)

            if (currentErrorReduction.isEmpty || currentErrorReduction.get._2 > errorReduction) {
              currentErrorReduction = Some((direction, errorReduction))
            }
          }
        }

        if (currentErrorReduction.isDefined) {
          optimizationPossible = true // If at least one segment moved, it opens up the possibility to move other segments on subsequent runs
          moveSegment(segmentEntry.segment, currentErrorReduction.get._1)
        }

      }
    } while (optimizationPossible)

    segmentMap.constructNewDrawingFromSegments.adjustToZero
  }

  private def moveSegment(s: Segment, d: Side): Unit = {
    d match {
      case Left | Top => s.value = s.value - 1
      case Right | Bottom => s.value = s.value + 1
    }
  }

  private def errorChange(movements: Vector[MovementChange]): Double = movements.map(m => m.newError - m.oldError).sum

  private def movementAllowed(movements: Vector[MovementChange]): Boolean = {

    /* Breaking aspect ratio is fine, but going from a valid ratio to an invalid might not be. */
    val breaksRatio = movements.exists(m => m.newRatio > m.oldRatio && m.newRatio > maxAspectRatio)
    val error = errorChange(movements)

    // No movement allowed unless it leads to a total cartographic error decrease
    if (error < 0) {

      /* If one or more rectangles goes from a valid aspect ratio to an invalid ratio, movement should only
       * be allowed if the total change in aspect ratio still leads to an overall decrease in ratio discrepancy.
       */
      if (breaksRatio) {
        val ratioIncrease = movements
          .filter(m => m.newRatio > m.oldRatio)
          .map(m => m.newRatio - m.oldRatio)
          .sum

        val ratioDecrease = movements
          .filter(m => m.newRatio < m.oldRatio)
          .map(m => m.newRatio - m.oldRatio)
          .sum

        if (Math.abs(ratioDecrease) > ratioIncrease)
          true
        else
          false

      } else {
        true // No rectangle exceeds the target aspect ratio, movement is A-OK.
      }

    } else {
      false // No error decrease, no movement
    }
  }

  private def zippedSegments(vertical: Vector[SegmentEntry], horizontal: Vector[SegmentEntry]): Vector[SegmentEntry] = {
    val vSize = vertical.size
    val hSize = horizontal.size
    if (vSize == hSize) {
      (vertical zip horizontal).map(t => Vector(t._1, t._2)).flatten

    } else if (vSize > hSize) {
      val vFirst: Vector[SegmentEntry] = vertical.dropRight(vSize - hSize)
      val last = vertical diff vFirst
      (vFirst zip horizontal).map(t => Vector(t._1, t._2)).flatten ++ last

    } else {
      val hFirst: Vector[SegmentEntry] = horizontal.dropRight(hSize - vSize)
      val last = horizontal diff hFirst
      (hFirst zip vertical).map(t => Vector(t._1, t._2)).flatten ++ last
    }
  }

  /**
   * @return Every segment on the specified axis, containing references to all segments it must be lower or higher than.
   */
  private def createSegmentEntries(axis: Axis,
                                   layout: RectangularLayout[VType, EType],
                                   segmentMap: SegmentMap[VType, EType],
                                   ignoredVertices: Set[VType]): Vector[SegmentEntry] = {
    val sMap: Map[Segment, Vector[(Rectangle, Side)]] = segmentToRectangles(axis, layout.allAreas, segmentMap)
    val vMap: Map[Rectangle, VType] = layout.rectangles.map(_.swap)
    val rToVMap: Map[Rectangle, VType] = layout.rectangles.map(_.swap)

    val segments = axis match {
      case X_Axis => segmentMap.verticalSegments
      case Y_Axis => segmentMap.horizontalSegments
    }

    val constraintsOfSegment = new mutable.HashMap[Segment, ArrayBuffer[Constraint]]()
    for (s <- segments)
      constraintsOfSegment += s -> new ArrayBuffer[Constraint]()

    // Add adjacency constraint with segments belonging to other rectangles
    for (constraint <- adjacencyConstraints(axis, layout, segmentMap))
      constraintsOfSegment(constraint.segment) += constraint

    // For each segment on the axis in a rectangle, add the opposite segment as a constraint
    for (r <- layout.allAreas) {
      val s = segmentMap.segmentOfArea(r)
      val segments = axis match {
        case X_Axis => (s.left, s.right)
        case Y_Axis => (s.top, s.bottom)
      }
      val planarConstr = new PlanarConstraints(segments._1, segments._2)
      constraintsOfSegment(segments._1) += planarConstr.lowerConstraint
      constraintsOfSegment(segments._2) += planarConstr.higherConstraint
    }

    // Construct the final entries
    val finalSegmentEntries = new ArrayBuffer[SegmentEntry]()
    for (s <- segments) {
      val cs = constraintsOfSegment(s).toVector
      val adjacentRectangles = sMap(s)

      val rectangleEntries = new ArrayBuffer[RectangleEntry]()
      for (r <- adjacentRectangles) {
        val targetSize = if (vMap.contains(r._1)) vMap(r._1).targetArea else gateTargetSize

        /* Rectangle should not be be optimized if it is a gate (no corresponding vertex) or if it is a regular rectangle
         * but also is present in the exceptions set.
         */
        val shouldNotBeOptimized = (rToVMap.contains(r._1) && ignoredVertices.contains(rToVMap(r._1))) || !rToVMap.contains(r._1)

        rectangleEntries += new RectangleEntry(segmentMap.segmentOfArea(r._1), targetSize, r._2, maxAspectRatio, shouldNotBeOptimized)
      }

      finalSegmentEntries += SegmentEntry(s, cs.filter(_.relation == GreaterThan), cs.filter(_.relation == LessThan), rectangleEntries.toVector, axis)
    }

    finalSegmentEntries.toVector
  }

  /**
   * Maps every segment to the rectangles that are bound by it, and which side of the segment that the rectangle appears on.
   */
  private def segmentToRectangles(axis: Axis, rectangles: Vector[Rectangle], segmentMap: SegmentMap[VType, EType]): Map[Segment, Vector[(Rectangle, Side)]] = {
    val sMap = new mutable.HashMap[Segment, ArrayBuffer[(Rectangle, Side)]]()
    for (s <- if (axis == X_Axis) segmentMap.verticalSegments else segmentMap.horizontalSegments)
      sMap += s -> new ArrayBuffer[(Rectangle, Side)]()

    for (r <- rectangles) {
      val segments = segmentMap.segmentOfArea(r)
      axis match {
        case X_Axis => sMap(segments.left) += ((r, Right)); sMap(segments.right) += ((r, Left))
        case Y_Axis => sMap(segments.top) += ((r, Bottom)); sMap(segments.bottom) += ((r, Top))
      }
    }

    sMap.toMap.map(kv => kv._1 -> kv._2.toVector)
  }

  /**
   * Every rectangle in the original layout must remain adjacent to its neighbors that doesn't share one of its two
   * edges along the axis being computed. See the AdjacencyConstraints class for more information.
   */
  private def adjacencyConstraints(axis: Axis,
                                   layout: RectangularLayout[VType, EType],
                                   segments: SegmentMap[VType, EType]): Vector[Constraint] = {
    (for {
      rectangle <- layout.allAreas
      if layout.adjacentAreas contains rectangle
      neighbors = layout.adjacentAreas(rectangle)
      rectSegments = segments.segmentOfArea(rectangle)
      neighborSegments = neighbors.map(segments.segmentOfArea)
      newConstraints = new AdjacencyConstraints(rectSegments, neighborSegments, axis).constraints
    } yield newConstraints).flatten
  }
}
