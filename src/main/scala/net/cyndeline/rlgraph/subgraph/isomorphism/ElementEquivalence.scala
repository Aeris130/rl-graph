package net.cyndeline.rlgraph.subgraph.isomorphism

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Compares pairs of elements when checking for isomorphism. Use this trait to write custom comparators
 * that only take specific properties of vertices into account when comparing two graphs.
 *
 * @tparam E Type of element to check equivalence for (vertex or edge type)
 */
trait ElementEquivalence[E, EdgeType[X] <: UnDiEdge[X]] {

  /**
   * Compares this element to another.
   *
   * @param e1 An element to compare.
   * @param e2 Another element to compare.
   * @param contextForE1 Graph that e1 belongs to.
   * @param contextForE2 Graph that e2 belongs to.
   * @return True if the elements are equal, otherwise false.
   */
  def compares(e1: E, e2: E, contextForE1: Graph[E, EdgeType], contextForE2: Graph[E, EdgeType]): Boolean

  /**
   * Computes that hash code used when comparing this element, two elements that are equal (in regards to the values
   * being sought after when performing an isomorphic search) should produce the same hash.
   *
   * Example: Assume a vertex element has two fields of type String and Int. If the isomorphic match only depends on
   * the String, then only the String should be used to compute the hash.
   *
   * @return the hash code of the element.
   */
  def elementHash(element: E): Int

}
