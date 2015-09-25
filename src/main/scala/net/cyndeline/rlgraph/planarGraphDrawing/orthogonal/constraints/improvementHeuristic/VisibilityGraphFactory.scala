package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic.CornerType._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.VertexWrapper

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.{WDiEdge, WLkDiEdge}
import scalax.collection.immutable.Graph

/**
 * Computes visibility graphs from a set of segments. Unlike shape graphs, edges in a visibility graph are based on
 * the segments a segment can "see" to its left and right (and thus must take into account when compacting the graph).
 *
 * To do this, visibility graphs are computed using the absolute x/y coordinates assigned to a shape graph during
 * constructive heuristics, and not dart from/to relations.
 *
 * @constructor Constructs a new visibility factory.
 * @param ordering Determines the ordering of neighbors, and direction of arcs in the graph. An ordering of
 *                 _ < _ gives a graph where segments of lower coordinate has arcs pointing to segments with
 *                 higher coordinate.
 */
class VisibilityGraphFactory[VType : TypeTag : ClassTag](ordering: Ordering[Segment[VType]]) {

  /**
   * Constructs a factory with a default ordering.
   */
  def this() = this(Ordering.fromLessThan[Segment[VType]](_.coordinate < _.coordinate))

  /**
   * Constructs a visibility graph out of segments and segment coordinates.
   *
   * The algorithm simulates a scanline that passes over every segment of a single orientation from top to bottom in
   * order to determine which segments each segment can see to its left and right. Whenever the line enters a segment
   * from the top, that segment is added to the neighbor list, ordered by the coordinate on the axix opposite to the
   * segments orientation (y-axis for horizontal segments).
   * An edge from its left neighbor to the segment is added to the shape graph, as well as an edge from the segment
   * to its right neighbor.
   *
   * If the line finds the end of a segment, it is removed, and an edge between its left and right neighbor is added
   * if both are present.
   *
   * If both the beginning and end of multiple segments are found at once, the beginning segments are
   * added, and all edges are drawn. Then the ending segments are removed.
   *
   * @param segments All horizontal and vertical segments to compute the graphs from. Every segment must have its x/y
   *                 coordinate set, and every vertex must appear once in each set (horizontal/vertical) segment.
   *                 Should not be based on dummy darts.
   * @return (Horizontal, Vertical) acyclic directed visibility graphs using the segments as vertices, with each edge
   *         s1~>s2 signifying that s1 must be placed before s2 in a graph drawing involving both segments.
   */
  def buildVisibilityGraphs(segments: Set[Segment[VType]]): (Graph[Segment[VType], WLkDiEdge], Graph[Segment[VType], WLkDiEdge]) = {
    val hsg = buildSingleVisibilityGraph(segments, Horizontal)
    val vsg = buildSingleVisibilityGraph(segments, Vertical)

    (hsg, vsg)
  }

  /**
   * Constructs a visibility graph in a single orientation.
   *
   * @param segments All horizontal and vertical segments to compute the graph from. Every segment must have its x/y
   *                 coordinate set, and every vertex must appear once in each set (horizontal/vertical) segment.
   *                 Should not be based on dummy darts.
   * @param orientation Which orientation the graph should have. Horizontal orientation gives a graph based on
   *                    which horizontal segments are visible to each other.
   *
   * @return a visibility graph of the specified orientation.
   */
  def buildSingleVisibilityGraph(segments: Set[Segment[VType]], orientation: SegmentOrientation): Graph[Segment[VType], WLkDiEdge] = {
    val allCorners: Set[SegmentCorner[VType]] = computeCornerCoordinates(segments)
    val cornersToUse = allCorners.filter(_.segment.orientation == orientation)
    makeSingleShapeGraph(cornersToUse)
  }

  /**
   * Implements the algorithm for a shape graph in a single direction.
   * @param segmentCorners Corners of all segments having the same alignment (horizontal/vertical).
   * @return a shape graph representing segment visibility.
   */
  private def makeSingleShapeGraph(segmentCorners: Set[SegmentCorner[VType]]): Graph[Segment[VType], WLkDiEdge] = {
    val scanLine = new ScanlineIterator(segmentCorners)
    val neighborOrder = ordering
    var visibilityEdges = Graph[Segment[VType], WDiEdge]()

    /* Holds every segment current covered by the scan line, in left-to-right order, with the segment
     * having the lowest coordinate first.
     */
    var neighborRelation = TreeSet.empty(neighborOrder)
    var edgeLabelId = 0

    while (scanLine.hasNext) {
      val newCorners: (Int, Set[SegmentCorner[VType]]) = scanLine.next()
      val corners = newCorners._2
      val segmentBeginnings = corners.filter(_.cornerType == Top)
      val segmentEnds = corners.filter(_.cornerType == Bottom)

      /* Process the beginning of every segment twice. Once to add them, and once to draw left/right neighbors after
       * all of them has been added, since some new segments might have each other as neighbors.
       */
      for (beginning <- segmentBeginnings) {
        neighborRelation += beginning.segment
      }

      for (beginning <- segmentBeginnings) {
        val neighbors = findLeftRightNeighbors(beginning.segment, neighborRelation.iterator)
        val leftNeighbor = neighbors._1
        val rightNeighbor = neighbors._2

        if (leftNeighbor.isDefined) {
          visibilityEdges += makeDiEdge(leftNeighbor.get, beginning.segment)
          edgeLabelId += 1
        }

        if (rightNeighbor.isDefined) {
          visibilityEdges += makeDiEdge(beginning.segment, rightNeighbor.get)
          edgeLabelId += 1
        }

      }

      /* Draw an edge between the neighbors of each segment ending here, since that segment may have previously
       * obstructed those segments from seeing each other (and thus not triggering when their top coordinates
       * were processed).
       *
       * However: if the neighbor of an ending segment is another segment ending as well, no edge should be drawn.
       * Only draw leftNeighbor -> rightNeighbor edges for the ending segments with a non-ending neighbor to the
       * left AND right.
       */
      val endingSegments: Set[Segment[VType]] = segmentEnds.map(corner => corner.segment)
      val removableSegments = allRemovableAdjacentSegments(endingSegments, neighborRelation.iterator)
      val endSegmentsToCheckNeighborsFor =  endingSegments -- removableSegments

      /* From here on out, the adjacent ends aren't needed any more, so they can be removed. */
      neighborRelation = neighborRelation -- removableSegments

      for (end <- endSegmentsToCheckNeighborsFor) {
        val neighbors = findLeftRightNeighbors(end, neighborRelation.iterator)
        val left = neighbors._1
        val right = neighbors._2

        if (left.isDefined && right.isDefined) {
          visibilityEdges += makeDiEdge(left.get, right.get)
        }
      }

      neighborRelation = neighborRelation -- endSegmentsToCheckNeighborsFor
    }

    /* Since visibility graphs are a subset of shape graphs, they're represented using the same key-labeled edges,
     * even though the graph doesn't contain multiple edges between two vertices.
     */
    Graph.from(Nil, convertDiEdgesToKeyLabeled(visibilityEdges))
  }

  /**
   * Finds the neighboring segments to the left and right of a particular segment.
   */
  private def findLeftRightNeighbors[V](segment: Segment[V],
                                            neighborOrder: Iterator[Segment[V]]
                                            ): (Option[Segment[V]], Option[Segment[V]]) = {
    val debugNeighborOrder = new ArrayBuffer[Segment[V]]()
    var last: Option[Segment[V]] = None

    while (neighborOrder.hasNext) {
      val neighbor = neighborOrder.next()
      debugNeighborOrder += neighbor

      if (neighbor == segment) {
        val leftNeighbor = last
        val rightNeighbor = if (neighborOrder.hasNext) Option(neighborOrder.next()) else None

        return (leftNeighbor, rightNeighbor)
      }

      last = Option(neighbor)
    }

    throw new IllegalArgumentException("The segment " + segment + " was not found among the neighbors " + debugNeighborOrder.toString)
  }

  /**
   * Computes all adjacent segments that can be removed while still having one segment representing them in terms
   * of visibility. Example (r = removable segment):
   *
   *  s0 | s1 | r | r | r | s2 | r
   *
   *  Out of the three middle segments, two can be removed since segment 1 and two are still separated by a removable
   *  segment.
   *
   * @param removableSegments All segments that can be removed.
   * @param neighbors Every visible segment along the scan line in one orientation of the shape graph.
   * @return every segment that should be removed.
   */
  private def allRemovableAdjacentSegments[V](removableSegments: Set[Segment[V]],
                                                  neighbors: Iterator[Segment[V]]): Set[Segment[V]] = {
    var segmentsToRemove = Set[Segment[V]]()
    var previous: Segment[V] = null

    while (neighbors.hasNext) {
      val n = neighbors.next()

      if (removableSegments.contains(n) && previous != null && removableSegments.contains(previous)) {
        segmentsToRemove += previous
      }

      previous = n
    }

    segmentsToRemove
  }

  /**
   * Computes all corner segment coordinates for each segment.
   *
   * @param segments All horizontal and vertical segments.
   * @return two corners (top and bottom) for each provided segment.
   */
  private def computeCornerCoordinates[V](segments: Set[Segment[V]]): Set[SegmentCorner[V]] = {
    val horizontalSegments = segments.filter(_.orientation == Horizontal)
    val verticalSegments = segments.filter(_.orientation == Vertical)
    val horizontalCorners = computeSingleOrientationCorners(horizontalSegments, verticalSegments)
    val verticalCorners = computeSingleOrientationCorners(verticalSegments, horizontalSegments)

    horizontalCorners ++ verticalCorners
  }

  /**
   * Computes the top/bottom coordinates for segments of a single orientation.
   */
  private def computeSingleOrientationCorners[V](segments: Set[Segment[V]], oppositeOrientation: Set[Segment[V]]): Set[SegmentCorner[V]] = {
    val corners = new ArrayBuffer[SegmentCorner[V]]()
    val segIt = segments.iterator
    while (segIt.hasNext) {
      val s = segIt.next()
      val headC = getCoordinate(s.vertices.head, oppositeOrientation)
      val lastC = getCoordinate(s.vertices.last, oppositeOrientation)

      /* If the segment begins and ends at the same coordinate, headC is considered
       * the top (arbitrary).
       */
      val headCornerType = if (headC >= lastC) Top else Bottom
      val lastCornerType = if (headC >= lastC) Bottom else Top

      val headCorner = new SegmentCorner[V](s, s.coordinate, headC, headCornerType)
      val lastCorner = new SegmentCorner[V](s, s.coordinate, lastC, lastCornerType)
      corners += headCorner
      corners += lastCorner
    }

    corners.toSet
  }

  /**
   * fetches the coordinate of a vertex using the main coordinate of segments it belongs to.
   * @param vertex Vertex to retrieve coordinate for.
   * @param segments all segments of the alignment for which the opposite should have its coordinate retrieved for
   *                 (aka: Horizontal for y-value, vertical for x-value).
   * @return the coordinate of the vertex.
   */
  private def getCoordinate[V](vertex: VertexWrapper[V], segments: Set[Segment[V]]): Int = {
    val segIt = segments.iterator
    while (segIt.hasNext) {
      val s = segIt.next()
      if (s.contains(vertex))
        return s.coordinate
    }

    throw new IllegalArgumentException("Segment set " + segments + " did not contain the vertex " + vertex)
  }

  /**
   * Converts the edges of a directed graph to key-weighted edges instead.
   */
  private def convertDiEdgesToKeyLabeled[V](graph: Graph[Segment[V], WDiEdge]): Vector[WLkDiEdge[Segment[V]]] = {
    var labelId = 0
    var result = Vector[WLkDiEdge[Segment[V]]]()

    for (edge <- graph.edges.toVector) {
      result = result :+ makeWkLDiEdge(edge.from, edge.to, labelId)
      labelId += 1
    }

    result.toVector
  }

  /**
   * Creates a key-labeled weighted directed edge.
   */
  private def makeWkLDiEdge[V](from: Segment[V], to: Segment[V], id: Int): WLkDiEdge[Segment[V]] = {
    (from~%+#>to)(1, "Label[" + id + "]")
  }

  private def makeDiEdge[V](from: Segment[V], to: Segment[V]): WDiEdge[Segment[V]] = {
    (from~%>to)(1)
  }
}
