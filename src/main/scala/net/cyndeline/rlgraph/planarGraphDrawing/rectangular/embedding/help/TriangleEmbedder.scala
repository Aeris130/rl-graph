package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.embedding.help

import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}

/**
 * Embeds a common vertex to an edge, forming a triangle.
 */
class TriangleEmbedder {

  /**
   * Embeds a new vertex on the unbounded face, adding a new interior triangular face using two new edges.
   * @param vertex Vertex to embed.
   * @param start Vertex that should lie before the new vertex when traversing the outer face in a clockwise direction.
   * @param end Vertex that should lie after the new vertex when traversing the outer face in a clockwise direction.
   * @param embedding The embedding to add the vertex to.
   * @tparam VType Type of vertex.
   */
  def embedNewTriangle[VType](vertex: VType, start: VType, end: VType, embedding: Embedding[VType]): Embedding[VType] = {
    var currentEmbedding = embedding

    /* Find the edge that lies 1 step counter clockwise to the start vertex.
     * The new edge should be embedded after it.
     */
    val previousAdjacencies = currentEmbedding.embeddingFor(start)
    val previousEdgeEntry = previousAdjacencies.entryFor(end).previous
    currentEmbedding = currentEmbedding.embedEdge(Vertex(start) withDefaultPositionInVertex vertex withInsertPosition previousEdgeEntry.adjacentVertex)

    /* Find the entry for the start vertex in the end vertex. The new vertex should be embedded clockwise (after) it. */
    currentEmbedding.embedEdge(Vertex(vertex) withInsertPosition start inVertex end withDefaultInsertPosition)
  }

  /**
   * Embeds a single edge between a vertex on an edge, and a new vertex that's been processed before.
   * @param oldNewVertex New vertex.
   * @param vertexInsertPos The insert position of the new vertex in the start vertex.
   * @param oldStart The vertex belonging to the current edge.
   * @param startInsertPos The insert position of the start vertex in the new vertex.
   */
  def embedOldTriangleEdge[VType](oldNewVertex: VType,
                                  vertexInsertPos: VType,
                                  oldStart: VType,
                                  startInsertPos: VType,
                                  embedding: Embedding[VType]): Embedding[VType] = {
    embedding.embedEdge(Vertex(oldNewVertex) withInsertPosition vertexInsertPos inVertex oldStart withInsertPosition startInsertPos)
  }
}
