package net.cyndeline.rlgraph.embedding

/**
 * A collection of common operations for adjacency entries. Stores a doubly linked
 * list of entries as they are embedded around a vertex.
 */
trait AdjacencyList[V] {

  /**
   * @return The vertex that this adjacency list has been assigned to.
   */
  def vertex: V

  /**
   * @return The first entry in the list.
   */
  def head: AdjacencyEntry[V]

  /**
   * @return an iterator over all adjacency entries in the order they are embedded.
   */
  def iterator: Iterator[AdjacencyEntry[V]]

  /**
    * @param v A vertex member of this adjacency list.
    * @return Vertices of this adjacency list as they are embedded, starting at vertex v.
    */
  def clockwiseIteratorFrom(v: V): Iterator[AdjacencyEntry[V]]

  /**
    * @param v A vertex member of this adjacency list.
    * @return Vertices of this adjacency list as they are visited when traversing the list counter clockwise.
    */
  def counterClockwiseIteratorFrom(v: V): Iterator[AdjacencyEntry[V]]

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
  def entryFor(vertex: V): AdjacencyEntry[V]

  /**
   * @param vertex The vertex to look for.
   * @return True if this adjacency list contains an embedding for the specified vertex, otherwise false.
   */
  def containsEntryFor(vertex: V): Boolean

  /**
   * @return a list of all vertices in the order they are embedded.
   */
  def toVector: Vector[AdjacencyEntry[V]]

}
