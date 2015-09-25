package net.cyndeline.rlgraph.embedding

/**
 * A single entry in an adjacency list. Stores a reference to the next and previous entry in the list.
 */
trait AdjacencyEntry[VType] {

  /**
   * The graph vertex on the other end of the edge this entry represents.
   * @return
   */
  def adjacentVertex: VType

  /**
   * The next edge around the vertex this entry is a member of.
   * @return
   */
  def next: AdjacencyEntry[VType]

  /**
   * The previous edge around the vertex this entry is a member of.
   * @return
   */
  def previous: AdjacencyEntry[VType]

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
  def moveTo: AdjacencyEntry[VType]
}
