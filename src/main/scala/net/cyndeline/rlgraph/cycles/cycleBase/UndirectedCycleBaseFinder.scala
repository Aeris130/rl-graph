package net.cyndeline.rlgraph.cycles.cycleBase

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Finds every simple cycle in an undirected graph. Example: The graph A~B, B~C, C~D, D~A, B~D has three cycles
 * that will be detected: [A, B, D, A], [A, B, C, D, A] and [B, D, C, B]. Of these, [A, B, D, A] and [B, D, C, B]
 * should be returned.
 */
trait UndirectedCycleBaseFinder {

  /**
   * Retrieves the cycle base of an undirected graph.
   * @param graph Graph to compute cycles for. Should not contain multiple edges between any two vertices
   * @tparam VType Type of vertices used in graph.
   * @tparam EType Type of edges used in graph.
   * @return A list of vertex-lists, where every vertex-list represents a cycle in the order that vertices appear in it.
   */
  def findCycleBase[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]): Vector[Vector[VType]]

}
