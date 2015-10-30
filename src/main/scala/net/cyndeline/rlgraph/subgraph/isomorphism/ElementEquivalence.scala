package net.cyndeline.rlgraph.subgraph.isomorphism

import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Compares pairs of elements when checking for isomorphism. Use this trait to write custom comparators
 * that only take specific properties of vertices into account when comparing two graphs.
 *
 * @tparam N1 Node type of the pattern graph.
 * @tparam N2 Node type in the graph to match the pattern against.
 * @tparam E1 Edge type in the pattern graph.
 * @tparam E2 Edge type in the graph to match the pattern against.
 */
//TODO delete
trait ElementEquivalence[N1, N2, E1[X] <: EdgeLikeIn[X], E2[X] <: EdgeLikeIn[X]] {

  /**
   * Compares a node from a pattern to a node in the graph that is matched against the pattern.
   * @param n1 Node in the pattern.
   * @param n2 node in the graph that is matched against the pattern.
   * @return True if n2 should be considered equal to n1, otherwise false.
   */
  def compareNode(n1: N1, n2: N2): Boolean

  /**
   * Compares an edge from a pattern to an edge in the graph that is matched against the pattern.
   * @param e1 Edge in the pattern.
   * @param e2 Edge in the graph that is matched against the pattern.
   * @return True if e2 should be considered equal to e1, otherwise false.
   */
  def compareEdge(e1: E1[N1], e2: E2[N2]): Boolean

}
