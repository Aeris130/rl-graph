package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

/**
 * Creates shape graphs. A shape graph is a pair of placement graphs (horizontal or vertical segments) linked
 * by arcs v -> w where v and w are two segments with the same alignment, connected by a dart.
 *
 * @constructor Constructs a new shape graph factory.
 */
class ShapeGraphFactory {

  /**
   * Computes a pair of shape graphs (Horizontal, Vertical).
   *
   * @param directedDarts Upward and rightward darts of an orthogonal representation.
   * @param segments Maximally connected horizontal and vertical components of said darts.
   * @return A horizontal and vertical shape graph.
   */
  def makeShapeGraphs[VType : Manifest](directedDarts: Set[Dart[VType]],
                                        segments: Set[Segment[VType]]
                                        ): (Graph[Segment[VType], WLkDiEdge], Graph[Segment[VType], WLkDiEdge]) = {

    /* Maps each vertex to the horizontal (._1) and vertical (._2) segment it belongs to. */
    var horizontalVertexSegments = Map[VertexWrapper[VType], Segment[VType]]()
    var verticalVertexSegments = Map[VertexWrapper[VType], Segment[VType]]()


    var vertices = Set[VertexWrapper[VType]]()
    for (dart <- directedDarts) {
      vertices += dart.from
      vertices += dart.to
    }

    for (vertex <- vertices) {
      val horizontalSegment = findSegment(vertex, segments, Horizontal)
      val verticalSegment = findSegment(vertex, segments, Vertical)

      if (horizontalSegment.isDefined)
        horizontalVertexSegments += (vertex -> horizontalSegment.get)

      if (verticalSegment.isDefined)
        verticalVertexSegments += (vertex -> verticalSegment.get)
    }

    val horizontalDarts = directedDarts.filter(_.direction == Some(Right))
    val verticalDarts = directedDarts.filter(_.direction == Some(Up))

    val horizontalShapeGraph = connectSegments(verticalDarts, horizontalVertexSegments, Horizontal)
    val verticalShapeGraph = connectSegments(horizontalDarts, verticalVertexSegments, Vertical)

    (horizontalShapeGraph, verticalShapeGraph)
  }

  /**
   * Finds a segment with a specified orientation that contains a specified vertex.
   */
  private def findSegment[VType](vertex: VertexWrapper[VType],
                                  segments: Set[Segment[VType]],
                                  orientation: SegmentOrientation): Option[Segment[VType]] = {
    for (segment <- segments)
      if (segment.orientation == orientation && segment.contains(vertex))
        return Option(segment)

    None
  }

  private def connectSegments[VType : Manifest, EType[X] <: UnDiEdge[X]]
    (darts: Set[Dart[VType]],
     segments: Map[VertexWrapper[VType], Segment[VType]],
     orientation: SegmentOrientation): Graph[Segment[VType], WLkDiEdge] = {
    val edges = new ArrayBuffer[WLkDiEdge[Segment[VType]]]()

    /* Each edge needs a label to distinguish them in the graph, not related to shape data. */
    var label = 0

    for (dart <- darts) {
      val fromSeg = segments.get(dart.from)
      val toSeg = segments.get(dart.to)

      /* If the graph only has darts in one direction, there will be darts but no segments for the
       * opposite direction.
       */
      if (fromSeg.isDefined && toSeg.isDefined) {

        /* All edges has weight 1, as that is the minimum distance between two segments. Not really
         * necessary to include, but w/e.
         */
        edges += (fromSeg.get~%+#>toSeg.get)(1, "Label[" + label + "]")
        label += 1

      }
    }

    Graph.from(Nil, edges)
  }
}
