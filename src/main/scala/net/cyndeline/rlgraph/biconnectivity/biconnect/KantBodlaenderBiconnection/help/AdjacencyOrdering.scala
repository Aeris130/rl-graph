package net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.help

import scala.collection.mutable.ArrayBuffer

/**
 * Maintains a list of adjacent vertices around a cut vertex. The ordering allows for retrieval of pairs of adjacent
 * neighbors, in the order they are embedded.
 *
 * Example: An adjacency of 1, 2, 3, 4, 5 would retrieve the pairs (1, 2), (2, 3), (3, 4), (4, 5) and then (5, 1)
 * as it is cyclical.
 *
 * The ordering allows a vertex in it to be removed, causing the current pair-list to account for the removal.
 *
 * Example: If the adjacency (see above) has retrieved (1, 2) and is currently set to return (2, 3), removing
 * vertex 2 will result in (1, 3) being returned instead. Removing vertex 3 on the other hand, results in the
 * next pair being (2, 4).
 *
 * If the ordering only has 3 elements, and the middle one is removed, the last and first elements will not wrap.
 */
class AdjacencyOrdering[VType](vertices: Vector[VType]) {
  private val buffer = new ArrayBuffer[VType]()
  vertices.copyToBuffer(buffer)
  private var pos = 0

  if (buffer.size < 2) throw new IllegalArgumentException("Adjacency orderings must contains 2 or more elements.")

  /* This wariable is set to false if the buffer size is less than 3, or if its size is reduced to less than
   * 3 before the first pair has been returned.
   */
  private var shouldWrap = buffer.size > 2

  def this(otherOrder: AdjacencyOrdering[VType]) = this(otherOrder.currentList)

  /**
   * Starts the ordering from the first element in the vertex list, and restores the list.
   */
  def reset(): Unit = {
    buffer.clear()
    vertices.copyToBuffer(buffer)
    pos = 0
    shouldWrap = buffer.size > 2
  }

  /**
   * @return The current vertices in the ordering.
   */
  def currentList: Vector[VType] = buffer.toVector

  /**
   * @return True if more pairs exist in the list.
   */
  def hasNext: Boolean = {
    if ((pos < buffer.size && shouldWrap) || pos < buffer.size - 1) {
      true
    } else {
      false
    }
  }

  /**
   * Retrieves the current pair, then moves ahead to the next.
   * @return The current element pair in the ordering.
   */
  def next(): (VType, VType) = {
    val pair = head
    pos += 1
    pair
  }

  /**
   * @return The next pair to be returned. Doesn't trigger the next pair.
   */
  def head: (VType, VType) = if (shouldWrap && pos == buffer.size - 1) {
    (buffer(pos), buffer(0))
  } else if (pos < buffer.size - 1) {
    (buffer(pos), buffer(pos + 1))
  } else {
    throw new Error("Reached the end of adjacency ordering. No more element pairs available.")
  }

  /**
   * Deletes the first occurrence of an element from the ordering.
   * @param element Element to delete.
   */
  def delete(element: VType): Unit = {
    if (buffer.size < 3) throw new Error("Cannot reduce adjacency orderings to less than 2")
    var i = 0
    while (i < buffer.size) {
      if (buffer(i) == element) {
        buffer.remove(i)

        if (i <= pos) {
          pos = Math.max(0, pos - 1)
        }

        if ((pos == 0 || pos == buffer.size - 1) && buffer.size < 3) {
          shouldWrap = false
        }

        return
      }

      i += 1
    }
  }
}
