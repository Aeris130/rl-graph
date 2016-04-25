package net.cyndeline.rlgraph.pathfinding

import scala.collection.mutable.ListBuffer
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * A path in a graph. To instantiate paths, use the companion object.
 *
 * @param start The first vertex in the path.
 * @param stop The last vertex in the path. May be the same as start if the path contains a single vertex.
 * @param vertices Every vertex in the path, in the order they are traversed from start to stop.
 * @param edges Every edge in the path, in the order they are traversed from start to stop. Empty if the path
 *              only contains a single vertex.
 */
class Path[V, E[X] <: EdgeLikeIn[X]] private (val start: V, val stop: V, val vertices: Vector[V], val edges: Vector[E[V]]) {

  override def equals(other: Any): Boolean = other match {
    case p: Path[V, E] => start == p.start && stop == p.stop && vertices == p.vertices && edges == p.edges
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.## ^ vertices.## ^ edges.##

  override def toString: String = "Path from " + start + " to " + stop + ", via " + edges.mkString(", ")

}

object Path {

  /**
   * Constructs a path with a single vertex.
   * @param startAndStop The only vertex in the path.
   */
  def apply[V, E[X] <: EdgeLikeIn[X]](startAndStop: V): Path[V, E] = new Path[V, E](startAndStop, startAndStop, Vector(startAndStop), Vector())

  /**
   * Parses the stop vertex and vertex collection from a collection of edges ordered as they appear on the path.
   * @param start Start vertex in the
   * @param edges Edges in the order they appear on the path.
   */
  def apply[V, E[X] <: EdgeLikeIn[X]](start: V, edges: Vector[E[V]]): Path[V, E] = {
    require(!edges.isEmpty, "Cannot create a path from edges unless at least one edge is specified.")
    val vertices = parseVertices(start, edges)
    new Path[V, E](start, vertices.last, vertices, edges)
  }

  private def parseVertices[V, E[X] <: EdgeLikeIn[X]](start: V, edges: Vector[E[V]]): Vector[V] = {
    var previous = start
    val vList = new ListBuffer[V]()
    vList += start

    for (e <- edges) {
      val next = e.find(_ != previous).get
      vList += next
      previous = next
    }

    vList.toVector
  }

}
