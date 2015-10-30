package net.cyndeline.rlgraph.subgraph.isomorphism.util

import net.cyndeline.rlgraph.subgraph.isomorphism.ElementEquivalence

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Checks basic equivalence between two vertices using equals and hash.
 *
 * @constructor Creates a new equivalence object.
 */
//TODO delete, the matchers should supply their own default
class DefaultVertexEquivalence[VType, EType[X] <: UnDiEdge[X]] extends ElementEquivalence[VType, VType, EType, EType] {

//  /**
//   * Compares this element to another.
//   *
//   * @param e1 An element to compare.
//   * @param e2 Another element to compare.
//   * @return True if the elements are equal, otherwise false.
//   */
//  def compares(e1: VType, e2: VType, contextForE1: Graph[VType, EType], contextForE2: Graph[VType, EType]): Boolean = e1 == e2
//
//  /**
//   * Computes that hash code used when comparing this element, two elements that are equal should produce the
//   * same hash. If the element only depends on a subset of its data, only that data should be hashed.
//   *
//   * @return the hash code of the element.
//   */
//  def elementHash(element: VType): Int = element.##
  /**
   * Compares a node from a pattern to a node in the graph that is matched against the pattern.
   * @param n1 Node in the pattern.
   * @param n2 node in the graph that is matched against the pattern.
   * @return True if n2 should be considered equal to n1, otherwise false.
   */
  override def compareNode(n1: VType, n2: VType): Boolean = n1 == n2

  /**
   * Compares an edge from a pattern to an edge in the graph that is matched against the pattern.
   * @param e1 Edge in the pattern.
   * @param e2 Edge in the graph that is matched against the pattern.
   * @return True if e2 should be considered equal to e1, otherwise false.
   */
  override def compareEdge(e1: EType[VType], e2: EType[VType]): Boolean = e1 == e2
}
