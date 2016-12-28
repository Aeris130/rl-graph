package rlgraph.help

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.util.GraphCommons

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Produces straight-line drawings from a graph and a vertex-coordinate map.
  */
object ProduceDrawing {

  def apply(coordinates: Map[Int, Point], graph: Graph[Int, UnDiEdge]): StraightLineDrawing[Int] = {
    require(coordinates.size == graph.nodes.size)
    if (coordinates.isEmpty)
      return new StraightLineDrawing(Vector(), Vector(), coordinates, 1, 1)

    val edges = GraphCommons.outerEdges(graph).map(e => UnorderedPair(e._1, e._2)).map(up => (up._1, up._2))
    val vertices = coordinates.keySet.toVector.sorted
    val width = coordinates.values.minBy(_.x).x + 1
    val height = coordinates.values.minBy(_.y).y + 1
    new StraightLineDrawing(vertices, edges, coordinates, width, height)
  }

}
