package net.cyndeline.rlgraph.subgraph.isomorphism

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Evaluates a positive isomorphic match and determines if it possesses any properties that should
 * invalidate the result.
 */
trait NegativeCondition[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * Validates or invalidates a mapping from A to B.
   * @param isomorphicMapping A mapping of vertices from a sub graph A that is isomorphic to a graph B.
   * @param context The graph B.
   * @return True if the isomorphic mapping of A to B doesn't violate any negative condition, otherwise false.
   */
  def isValid(isomorphicMapping: Map[VType, VType], context: Graph[VType, EType]): Boolean

}
