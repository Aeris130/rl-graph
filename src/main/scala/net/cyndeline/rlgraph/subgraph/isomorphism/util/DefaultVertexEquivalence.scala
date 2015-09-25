package net.cyndeline.rlgraph.subgraph.isomorphism.util

import net.cyndeline.rlgraph.subgraph.isomorphism.ElementEquivalence

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Checks basic equivalence between two vertices using equals and hash.
 *
 * @constructor Creates a new equivalence object.
 */
class DefaultVertexEquivalence[VType, EType[X] <: UnDiEdge[X]] extends ElementEquivalence[VType, EType] {

  /**
   * Compares this element to another.
   *
   * @param e1 An element to compare.
   * @param e2 Another element to compare.
   * @return True if the elements are equal, otherwise false.
   */
  def compares(e1: VType, e2: VType, contextForE1: Graph[VType, EType], contextForE2: Graph[VType, EType]): Boolean = e1 == e2

  /**
   * Computes that hash code used when comparing this element, two elements that are equal should produce the
   * same hash. If the element only depends on a subset of its data, only that data should be hashed.
   *
   * @return the hash code of the element.
   */
  def elementHash(element: VType): Int = element.##

}
