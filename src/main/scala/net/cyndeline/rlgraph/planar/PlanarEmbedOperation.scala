package net.cyndeline.rlgraph.planar

import net.cyndeline.rlgraph.embedding.Embedding

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes a planar embedding of a connected undirected graph, if one exists.
 */
trait PlanarEmbedOperation[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * Embeds a planar graph.
   * @param graph Graph to embed.
   * @return A planar embedding, or None if the graph isn't planar.
   */
  def embed(graph: Graph[VType, EType]): Option[Embedding[VType]]

  /**
   * Checks if a graph is planar.
   * @param graph Graph to evaluate planarity for.
   * @return true if the graph is planar, otherwise false.
   */
  def isPlanar(graph: Graph[VType, EType]): Boolean

}
