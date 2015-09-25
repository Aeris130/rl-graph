package net.cyndeline.rlgraph.cycles

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Computes every cycle in a graph.
 */
trait CycleFinder[V, E[X] <: EdgeLikeIn[X]] {

  /**
   * @param graph Graph to search for cycle in.
   * @return Every cycle in the graph. Empty if no cycle exists.
   */
  def findCycles(graph: Graph[V, E]): Vector[Cycle[V]]

}
