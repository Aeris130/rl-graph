package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Computes all horizontal and vertical segments based on directed darts from an orthogonal auxiliary representation
 * (rectangular faces with with no bends). All segments are maximally connected, in the upward/leftward direction.
 * Darts pointing in directions opposite to those are ignored.
 *
 * @constructor Constructs a new segment factory.
 */
class SegmentFactory {

  /**
   * Computes all horizontal (left to right ) and vertical (bottom to top) segments of a directed set of face darts.
   * @param directedDarts All directed darts to compute segments from. Must contain only darts with direction specified.
   * @return A set of all segments computed from the faces.
   */
  def computeSegments[VType : Manifest, EType[X] <: UnDiEdge[X]](directedDarts: Set[Dart[VType]]): Set[Segment[VType]] = {

    /* Scala graphs can be used to easily compute segments. For every directed dart, simply add a directed edge
     * to the graph between the two vertices. Once done, all connected components can be found by looking for
     * nodes in the graph with no incoming edges, and traversing them to the final node with no outgoing edges.
     */
    val horizontal: Graph[VertexWrapper[VType], DiEdge] = computeAllComponents(directedDarts.filter(_.direction == Some(Right)))
    val vertical: Graph[VertexWrapper[VType], DiEdge] = computeAllComponents(directedDarts.filter(_.direction == Some(Up)))

    val horizontalSegments = findSegments(horizontal, Horizontal)
    val verticalSegments = findSegments(vertical, Vertical)

    /* For both segment alignments, every vertex that isn't present in a segment must be assigned its own
     * 1-vertex segment.
     */
    var allVertices = Set[VertexWrapper[VType]]()
    for (dart <- directedDarts) {
      allVertices += dart.to
      allVertices += dart.from
    }

    val horizontalSingleSegments = makeSingleSegments(allVertices, horizontalSegments, Horizontal)
    val verticalSingleSegments = makeSingleSegments(allVertices, verticalSegments, Vertical)

    horizontalSegments ++ verticalSegments ++ horizontalSingleSegments ++ verticalSingleSegments
  }

  /**
   * Adds every dart with a specific direction to a graph. Causes the graph to consist of disconnected components.
   */
  private def computeAllComponents[VType : Manifest](directedDarts: Set[Dart[VType]]): Graph[VertexWrapper[VType], DiEdge] = {
    var graph = Graph[VertexWrapper[VType], DiEdge]()
    for (dart <- directedDarts)
      graph += dart.from~>dart.to

    graph
  }

  /**
   * Finds every node with no incoming edges, and traverses their single outgoing edge to the next node until the
   * final node in the component is found. Every node of each component is added to the segment it belongs to.
   */
  private def findSegments[VType : Manifest](graph: Graph[VertexWrapper[VType], DiEdge], orientation: SegmentOrientation): Set[Segment[VType]] = {

    /* Filtering cannot be applied to the graphs own nodeset, since the internal nodes doesn't eq the originals. */
    val originalNodes = graph.nodes.toOuter.toSet[VertexWrapper[VType]]
    val startNodes = originalNodes.filter(n => graph.get(n).inDegree == 0)
    val startIterator = startNodes.iterator
    val segments = new ArrayBuffer[Segment[VType]]()

    while (startIterator.hasNext) {
      val start = startIterator.next()

      var node: graph.type#NodeT = graph.get(start)
      var castNode: VertexWrapper[VType] = node // fetches the inner object stored in the node
      var segment = Vector[VertexWrapper[VType]]()

      while (node.outDegree > 0) {
        segment = segment :+ castNode

        /* Each node should only have one outgoing edge, otherwise something went wrong during
         * direction computation.
         */
        if (node.outDegree != 1)
          throw new Error("Node " + node + " had a higher outgoing degree than 1 (" + node.outDegree + ")")

        /* Outgoing nodes come in the form Graph[VertexWrapper[VType], DiEdge]#NodeT, not graph.type#NodeT, so
         * it has to be retrieved from the graph again to be in a valid format.
         */
        val tmpCast = node.outgoing.head.to // Get the outgoing node
        node = graph.get(tmpCast) // Cast it to graph.type

        castNode = node // Cast it to VertexWrapper
      }

      segment = segment :+ castNode // Add the last node with 0 outgoing edges
      segments += new Segment(segment, orientation)
    }

    segments.toSet
  }

  /**
   * Creates segments containing a single vertex for every vertex not found in a set of segments aligned in a
   * single direction.
   *
   * @param vertices All vertices in a set of faces.
   * @param alignedSegments A set of segments (horizontal OR vertical).
   */
  private def makeSingleSegments[VType](vertices: Set[VertexWrapper[VType]], alignedSegments: Set[Segment[VType]], orientation: SegmentOrientation): Set[Segment[VType]] = {
    val additionalSegments = new ArrayBuffer[Segment[VType]]()

    for (vertex <- vertices) {
      var found = false

      for (segment <- alignedSegments) {
        if (segment.vertices.contains(vertex))
          found = true
      }

      if (!found)
        additionalSegments += new Segment(Vector(vertex), orientation)
    }

    additionalSegments.toSet
  }
}
