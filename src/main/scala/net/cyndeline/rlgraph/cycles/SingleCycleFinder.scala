package net.cyndeline.rlgraph.cycles

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Checks for presence of a cycle in a graph, and returns one if one or more cycles exist.
 */
trait SingleCycleFinder[VType, EType[X] <: EdgeLikeIn[X]] {

  /**
   * Computes a cycle from an arbitrary vertex. Only visits vertices approved by the vertex filter.
   * @param graph Graph to search for cycle in.
   * @return The vertices of the cycle, or None if no cycle exists.
   */
  def findCycle(graph: Graph[VType, EType]): Option[Cycle[VType]]

  /**
   * Computes a cycle that start at a specified vertex. Only visits vertices approved by the vertex filter, except
   * for the start vertex.
   *
   * @param start Start vertex for the cycle.
   * @param graph Graph to search for cycle in.
   * @return The vertices of the cycle (with the start vertex as the head), or None if no cycle exists.
   */
  def findCycleFromVertex(start: VType, graph: Graph[VType, EType]): Option[Cycle[VType]]

}
