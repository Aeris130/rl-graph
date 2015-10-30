package net.cyndeline.rlgraph.subgraph.isomorphism

import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Compares edges in a pattern against edges in the graph being matched against the pattern.
 * @tparam E1 Vertex type in the graph to match the pattern against.
 * @tparam E2 Vertex type of the pattern graph.
 */
trait EdgeCompare[E1[X] <: EdgeLikeIn[X], E2[X] <: EdgeLikeIn[X]] {

  /**
   * Compares an edge from a pattern to an edge in the graph that is matched against the pattern.
   * @param e1 Edge in the graph that is matched against the pattern.
   * @param e2 Edge in the pattern.
   * @return True if e1 should be considered equal to e2, otherwise false.
   */
  def compareEdge(e1: E1[_], e2: E2[_]): Boolean

}
