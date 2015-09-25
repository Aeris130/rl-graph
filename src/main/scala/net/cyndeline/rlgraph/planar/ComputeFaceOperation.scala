package net.cyndeline.rlgraph.planar

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.Face

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes every face from a planar embedding without regard to edge direction.
 *
 * @tparam VType The type of nodes used in each face.
 */
trait ComputeFaceOperation[VType] {

  /**
   * Computes every face from a planar embedding of a connected graph.
   *
   * @param embedding A planar embedding of a graph connected graph.
   * @return A list of faces, where each face is represented by a list of vertices in the order they are
   *         traversed around the face. The last entry in the face is implied to be connected to the first.
   */
  def computeFaces(embedding: Embedding[VType]): Vector[Face[VType]]

  /**
   * Embeds the graph and computes the faces using the embedding.
   * @param graph Graph to compute faces from.
   * @return A list of faces, where each face is represented by a list of vertices in the order they are
   *         traversed around the face. The last entry in the face is implied to be connected to the first.
   */
  def computeFacesFromGraph(graph: Graph[VType, UnDiEdge]): Vector[Face[VType]]
}
