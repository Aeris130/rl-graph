package net.cyndeline.rlgraph.biconnectivity

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Modifies a graph by adding edges to it until it is biconnected.
 *
 * @tparam VType Vertex type in the graph.
 */
trait Biconnecter[VType] {

  /**
   * @param graph A graph.
   * @return A graph with the same vertex set as the input, but with added edges in order to make it biconnected.
   *         Or the same graph if the original was already biconnected.
   */
  def biconnect(graph: Graph[VType, UnDiEdge]): GraphBiconnection[VType]

}
