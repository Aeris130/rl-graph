package net.cyndeline.rlgraph.biconnectivity

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes a list of biconnected components from a connected graph.
 */
trait BiconnectedComponentsOperation[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * Computes a list of biconnected components from a connected graph.
   * @param graph An undirected graph.
   * @return A list of every biconnected component in the graph. If the graph is empty or only
   *         has one vertex, an empty list is returned.
   */
  def components(graph: Graph[VType, EType]): Vector[Graph[VType, EType]]

  /**
   * Computes all articulation/cut points for a graph using a DFS search.
   * @param graph An undirected graph.
   * @return A list of every cutpoint in the graph.
   */
  def articulationPoints(graph: Graph[VType, EType]): Vector[VType]

  /**
   * Computes all biconnected components and identifies which cutpoints belong to which component.
   * @param graph An undirected graph.
   * @return A list of tuples where the first element is a biconnected component, and the second is the set of
   *         cutpoints belonging present on that component.
   */
  def componentsAndArticulationPoints(graph: Graph[VType, EType]): Vector[(Graph[VType, EType], Set[VType])]

  /**
   * Checks if a graph is biconnected or not.
   * @param graph Graph to check biconnectivity for.
   * @return True if the graph is biconnected, otherwise false if the graph has more than one biconnected component.
   */
  def isBiconnected(graph: Graph[VType, EType]): Boolean

}
