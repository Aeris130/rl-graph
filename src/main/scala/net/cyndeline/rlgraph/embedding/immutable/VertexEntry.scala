package net.cyndeline.rlgraph.embedding.immutable

import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, AdjacencyList}

/**
 * Serves as both adjacency list as well as individual adjacency entries.
 *
 * @param vertexIndex The index of the vertex in an embedding that owns the adjacency vector represented by this entry.
 * @param currentAdjacencyIndex The index in the adjacency vector that contains the vertex-index of the neighbor
 *                              that the list currently points at. Again, this is not the index of the neighbors in
 *                              the vector, but rather the vector-indices themselves. -1 if the vertex has no neighbors.
 * @param embedding The immutable embedding that this entry was created from.
 */
class VertexEntry[V](val vertexIndex: Int, val currentAdjacencyIndex: Int, val embedding: UndirectedEmbedding[V]) extends AdjacencyEntry[V] with AdjacencyList[V] {
  private val indexOfCurrentNeighbor: Int = if (currentAdjacencyIndex >= 0)
    embedding.adjacencyLists(vertexIndex)._2(currentAdjacencyIndex)
  else
    -1

  /*
   * Entry methods
   */

  /**
   * The next edge around the vertex this entry is a member of.
   * @return
   */
  override def next: AdjacencyEntry[V] = new VertexEntry(vertexIndex, nextIndex(currentAdjacencyIndex), embedding)

  /**
   * The previous edge around the vertex this entry is a member of.
   * @return
   */
  override def previous: AdjacencyEntry[V] = new VertexEntry(vertexIndex, previousIndex(currentAdjacencyIndex), embedding)

  /**
   * Traverses an embedding by retrieving the entry belonging to this entry's vertex
   * in the vertex represented by this entry.
   *
   * Example:
   *
   * A vertex v1 has two edges embedded in it: v1~v2 and v1~v3. Consequently, the embedding will have
   * two AdjacencyEntries mapped to v1: entry(v2) and entry(v3).
   *
   * If v1 has an edge to v2 embedded in it and it is possible to traverse the graph from v1 to v2,
   * calling moveTo will return v1's entry in v2. From here, it is possible to call next() and
   * previous() on the returned entry to find out which edges lie clockwise/counter-clockwise when
   * traveling to v1 from the edge connected to v1.
   *
   * @return The adjacency-entry belonging to this entry's owner in the specified node.
   */
  override def moveTo: AdjacencyEntry[V] = {
    val adjacencyListOfCurrentAdjacentVertex = embedding.adjacencyLists(indexOfCurrentNeighbor)._2
    var positionOfThisVertex = 0
    for (adjacency <- adjacencyListOfCurrentAdjacentVertex) {
      if (embedding.adjacencyLists(adjacency)._1 == vertex) {
        return new VertexEntry(indexOfCurrentNeighbor, positionOfThisVertex, embedding)
      } else {
        positionOfThisVertex += 1
      }
    }

    throw new Error("Could not find the entry of the current vertex " + vertex + " in one of its neighbors.")
  }

  /**
   * @return The graph vertex on the other end of the edge this entry represents.
   */
  override def adjacentVertex: V = {
    val adjacencyList: Vector[Int] = embedding.adjacencyLists(vertexIndex)._2
    val adjacentVertexIndex = adjacencyList(currentAdjacencyIndex)
    embedding.adjacencyLists(adjacentVertexIndex)._1
  }

  /*
   * List methods
   */

  /**
   * @return The vertex that this adjacency list has been assigned to.
   */
  override val vertex: V = embedding.adjacencyLists(vertexIndex)._1

  /**
   * @return The first entry in the list.
   */
  override def head: AdjacencyEntry[V] = new VertexEntry(vertexIndex, 0, embedding)

  /**
   * @return an iterator over all adjacency entries in the order they are embedded.
   */
  override def iterator: Iterator[AdjacencyEntry[V]] = {
    val adjacencyList: Vector[Int] = embedding.adjacencyLists(vertexIndex)._2
    (for (i <- 0 until adjacencyList.size) yield new VertexEntry(vertexIndex, i, embedding)).iterator
  }

  /**
   * @return the number of entries in the list.
   */
  override val size: Int = embedding.adjacencyLists(vertexIndex)._2.size

  /**
   * @return True if the list has no elements, otherwise false.
   */
  override val isEmpty: Boolean = embedding.adjacencyLists(vertexIndex)._2.isEmpty

  /**
   * Fetches an entry containing a specific vertex. Does not guarantee constant lookup time, and
   * throws a NoSuchElementException if the vertex is not present.
   * @param vertex A vertex embedded in the list.
   * @return the entry storing the specified vertex.
   */
  override def entryFor(vertex: V): AdjacencyEntry[V] = {
    val index = getEntryIndex(vertex)
    val neighborsOfThisVertex = embedding.adjacencyPositions(vertexIndex)
    val positionOfNeighbor = neighborsOfThisVertex(index)

    new VertexEntry(vertexIndex, positionOfNeighbor, embedding)
  }

  /**
   * @param vertex The vertex to look for.
   * @return True if this adjacency list contains an embedding for the specified vertex, otherwise false.
   */
  override def containsEntryFor(vertex: V): Boolean = {
    val index = getEntryIndex(vertex)
    val neighborsOfThisVertex = embedding.adjacencyPositions(vertexIndex)

    neighborsOfThisVertex.contains(index)
  }

  /**
   * @return a list of all vertices in the order they are embedded.
   */
  override def toVector: Vector[AdjacencyEntry[V]] = iterator.toVector

  override def equals(other: Any): Boolean = other match {
    case ve: VertexEntry[V] => ve.vertexIndex == vertexIndex && ve.currentAdjacencyIndex == currentAdjacencyIndex && ve.embedding == embedding
    case _ => false
  }

  override val hashCode: Int = vertexIndex ^ indexOfCurrentNeighbor ^ embedding.##

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "V:" + vertex

    if (!isEmpty) {
      val neighbors = toVector.map(_.adjacentVertex)
      builder ++= " | Adjacencies: " + neighbors
      builder ++= " | Pointing at: " + adjacentVertex
    }

    builder.toString()
  }

  // -1 if non-existent
  private def getEntryIndex(vertex: V): Int = {
    if (vertex == this.vertex)
      throw new Error("The vertex " + vertex + " cannot contain entries for itself.")

    embedding.vertexIndices.get(vertex).getOrElse(-1)
  }

  private def nextIndex(i: Int): Int = if (i == size - 1) 0 else i + 1
  private def previousIndex(i: Int): Int = if (i == 0) size - 1 else i - 1
}
