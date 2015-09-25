package net.cyndeline.rlgraph.embedding

/**
 * A collection of common operations for adjacency entries. Stores a doubly linked
 * list of entries as they are embedded around a vertex.
 */
trait AdjacencyList[VType] {

  /**
   * @return The vertex that this adjacency list has been assigned to.
   */
  def vertex: VType

  /**
   * @return The first entry in the list.
   */
  def head: AdjacencyEntry[VType]

  /**
   * @return an iterator over all adjacency entries in the order they are embedded.
   */
  def iterator: Iterator[AdjacencyEntry[VType]]

  /**
   * @return the number of entries in the list.
   */
  def size: Int

  /**
   * @return True if the list has no elements, otherwise false.
   */
  def isEmpty: Boolean

  /**
   * Fetches an entry containing a specific vertex. Does not guarantee constant lookup time, and
   * throws a NoSuchElementException if the vertex is not present.
   * @param vertex A vertex embedded in the list.
   * @return the entry storing the specified vertex.
   */
  def entryFor(vertex: VType): AdjacencyEntry[VType]

  /**
   * @param vertex The vertex to look for.
   * @return True if this adjacency list contains an embedding for the specified vertex, otherwise false.
   */
  def containsEntryFor(vertex: VType): Boolean

  /**
   * @return a list of all vertices in the order they are embedded.
   */
  def toVector: Vector[AdjacencyEntry[VType]]

}
