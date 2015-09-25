package net.cyndeline.rlgraph.cycles

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Parses all simple cycles from a directed graph.
 *
 * @tparam V Vertex type used in the input graphs.
 */
trait DirectedSimpleCycles[V] {

  /**
   * @param graph A directed graph.
   *
   * @return A vector containing every cimple cycle in the graph.
   */
  def findCycles(graph: Graph[V, DiEdge]): Vector[Cycle[V]]

  /**
   * This method processes each simple cycle individually as they are discovered. As the number of cycles in directed
   * graphs can increase exponentially, this method minimizes the memory usage by discarding the cycle after it has
   * been processed.
   * @param graph A directed graph.
   * @param f A function that takes a single cycle as input and performs some processing.
   */
  def iterateCycles(graph: Graph[V, DiEdge], f: Cycle[V] => Unit): Unit

}
