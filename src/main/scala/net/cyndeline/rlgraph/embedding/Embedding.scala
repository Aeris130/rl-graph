package net.cyndeline.rlgraph.embedding

/**
 * Stores the circular order of edges around each vertex in a graph.
 */
trait Embedding[VType] {

  /**
   * Checks if the embedding contains any vertices (a prerequisite for containing edges,
   * and vice versa).
   * @return True if the embedding has at least one edge embedded, otherwise false.
   */
  def isEmpty: Boolean

  /**
   * @param vertex Vertex to check membership for.
   * @return True if the vertex is embedded, otherwise false.
   */
  def isEmbedded(vertex: VType): Boolean

  /**
   * Retrieves the head element in the list of edges connected to a single vertex in a graph. Note that this
   * element is arbitrarily chosen as Head, as the list is circular.
   *
   * @param vertex The vertex to retrieve an embedding for.
   * @return the head of an adjacency list for the specified vertex, or none if the vertex isn't present in the
   *         embedding.
   */
  def embeddingFor(vertex: VType): AdjacencyList[VType]

  /**
   * @return every vertex in the embedding.
   */
  def embeddedVertices: Vector[VType]

  /**
   * @return Every embedded edge.
   */
  def edges: Vector[(VType, VType)]

  /**
   * @return every vertex and its head entry in the embedding.
   */
  def iterator: Iterator[(VType, AdjacencyList[VType])]

  /**
   * Embeds a new edge. Regardless of weather the edge is undirected or directed,
   * it contains a head and a tail node (these are arbitrary on undirected edges).
   * Vertices will be given an empty adjacency list if they're not already present,
   * and each will have the opposite node added to its embedding.
   *
   * Example usage: embed(A, B, A_C, B_F)
   *
   * The above call embeds an edge between A and B. Both A and B exists in the embeddings, but does not have
   * an edge inserted between them. The entry of B will be inserted in A's adjacency list after the edge entry
   * pointing to the vertex C. The entry of A in B will be inserted after the entry pointing to the vertex F.
   *
   * Example usage: embed(A, B, A_C, None)
   *
   * This call can be made to embed the edge (A, B) if B doesn't already exist in the embedding (the entry to A will be
   * the only entry in B's adjacency list, and thus does not need a specified position), or if the entry of A in B may
   * be inserted at an arbitrary position.
   *
   * @param head A vertex to be embedded.
   * @param tail A vertex to be embedded, belonging to the same edge as the Head.
   * @param headPos Node that the Head should be placed after in the Tails embedding. None if it should be
   *                added to the end of the adjacency list or if this is the first edge to embedded into the tail.
   * @param tailPos Node that the Tail should be placed after in the Heads embedding. None if it should be
   *                added to the end of the adjacency list or if this is the first edge to embedded into the head.
   * @return A new embedding with the edge added to it, or the same embedding if both vertices already exist and
   *         contain an edge between each other.
   */
  def embed(head: VType, tail: VType, headPos: Option[VType], tailPos: Option[VType]): Embedding[VType]

  /**
   * Embeds an edge A~B between two vertices A and B, specifying which vertex in B's adjacency list that A should be
   * inserted after, and vice versa.
   * @param insertData DSL specifying insertion points for each vertex, or default insertion.
   * @return This mutable embedding, allows chained embed calls.
   */
  def embedEdge(insertData: EdgeInsert[VType]): Embedding[VType]

  /**
   * Embeds a new edge.
   * @param head A vertex to be embedded.
   * @param tail A vertex to be embedded, belonging to the same edge as the Head.
   * @return A new embedding with the edge added to it, or the same embedding if both vertices already exist and
   *         contain an edge between each other.
   */
  def embed(head: VType, tail: VType): Embedding[VType]

  /**
   * Embeds a single vertex with an empty adjacency list.
   * @param vertex Vertex to embed.
   * @return A new embedding with the specified vertex embedded with an empty adjacency list.
   */
  def embed(vertex: VType): Embedding[VType]

  /**
   * Constructs a complete embedding using adjacencies around each vertex.
   * @param adjacencies A map between every vertex in the embedding and the vertices adjacent to it in the order
   *                    they should be embedded. Note that no vertex in this map may already be present in the
   *                    embedding. If so is the case, embed them separately then join them at specified insertion
   *                    points.
   * @return A copy of this embedding with every specified vertex mapped to its adjacency list.
   */
  def embed(adjacencies: Map[VType, Vector[VType]]): Embedding[VType]

  /**
   * Removes an edge from the embedding.
   * @param head A vertex on the edge.
   * @param tail Another vertex on the edge.
   * @return A new embedding with the selected edge removed.
   */
  def deleteEdge(head: VType, tail: VType): Embedding[VType]

  /**
   * Removes a vertex and any edges connected to it from the embedding.
   * @param vertex Vertex to remove.
   * @return A new embedding with the selected vertex removed.
   */
  def deleteVertex(vertex: VType): Embedding[VType]

  /**
   * Creates a new embedding by joining two embeddings together. Vertices shared by both embeddings will have
   * every edge from one embedding inserted after an entry in the other.
   *
   * @param other Embedding to combine with this one.
   * @param positions Edges in this embedding, mapped towards the vertex of the entry that any edges from 'other
   *                  should be inserted after. This map needs to have a key for every vertex common to both embeddings,
   *                  or an IllegalArgumentException will be thrown. Example: If a vertex 1 is present in both embeddings,
   *                  with edges 2 and 3 in this embedding, and edges 4 and 5 in 'other, vertex 1 must be mapped against
   *                  vertex 2 or 3 so that 4 and 5 may be inserted after it.
   * @return A new embedding with all edges from both previous embeddings. The shares vertex will contain edges
   *         from both embeddings, and all edges from a single embedding will retain the circular order they had
   *         before the join.
   */
  def addEmbedding(other: Embedding[VType], positions: Map[VType, VType]): Embedding[VType]

  /**
   * Reverses every adjacency entrys next/previous links in the embedding.
   * @return A new embedding with every adjacency list reversed.
   */
  def reverse: Embedding[VType]

  /**
   *
   * @param f Function that takes a vertex in the embedding and outputs another vertex.
   * @tparam N Output type.
   * @return A copy of this embedding with every vertex mapped to the ones produced by the function f.
   */
  def map[N](f: VType => N): Embedding[N]

  /**
   * @param v Vertex to retrieve neighbors for.
   * @return Every neighboring vertex of v.
   */
  def neighborsOf(v: VType): Vector[VType]

  /** @return The number of embedded vertices. */
  def vertexSize: Int

}
