package net.cyndeline.rlgraph.spqr

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Parses an SPQR tree from an undirected planar graph.
 *
 * @tparam VType Vertex type in the graph.
 * @tparam EType Edge type in the graph.
 */
trait TreeBuilder[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @param graph A planar biconnected graph.
   * @return An SPQR tree based on the input graph.
   */
  def buildTree(graph: Graph[VType, EType]): SPQRTree[VType]

  /**
   * Builds an SPQR tree rooted in a node selected by the user.
   * @param graph A planar biconnected graph.
   * @param rootSelection A function that takes a set of node vertices, and returns the index of the node that should be
   *                      used as root.
   * @return An SPQR tree based on the input graph.
   */
  def buildTreeWithRoot(graph: Graph[VType, EType], rootSelection: Array[Set[VType]] => Int): SPQRTree[VType]

}
