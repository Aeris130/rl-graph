package net.cyndeline.rlgraph.regularEdgeLabeling.factories.help

import net.cyndeline.rlgraph.util.IndexVertex

/**
 * The current outer cycle C (minus the south vertex) used when computing the minimal labeling. This cycle orders the
 * vertices currently on C from left to right, and can also be used to check the position of each vertex (with the
 * leftmost vertex having position 0).
 *
 * @param vertices All vertices except South, going from West to East.
 */
class OuterCycle[V] private (val vertices: Vector[IndexVertex[V]], positions: Vector[Int]) {

  /**
   * @param initialVertices All neighbors of the north vertex, going from left (west) to right (east).
   * @return An initial cycle C.
   */
  def this(initialVertices: Vector[IndexVertex[V]], totalSize: Int) = this(initialVertices, {
    var pos = Vector.fill(totalSize)(-1)
    for (initial <- initialVertices.zipWithIndex)
      pos = pos.updated(initial._1.index, initial._2)

    pos
  })

  /** @return The position of the specified vertex in the cycle.  */
  def positionOf(v: IndexVertex[V]): Int = {
    require(positions(v.index) >= 0, "No position defined for vertex " + v)
    positions(v.index)
  }

  /** @return A sub-interval of this cycle between two vertices. */
  def interval(from: IndexVertex[V], to: IndexVertex[V]): Vector[IndexVertex[V]] = {
    require(positionOf(from) < positionOf(to), "The position of 'from must be less than 'to.")
    vertices.slice(positionOf(from), positionOf(to) + 1)
  }

  /**
   * @param v A vertex to check membership for.
   * @return True if the cycle currently contains the vertex v, otherwise false.
   */
  def contains(v: IndexVertex[V]): Boolean = positions(v.index) >= 0

  /**
   * @param vs A collection of vertices from the cycle in any order.
   * @return The vertex with the lowest position.
   */
  def minimal(vs: Vector[IndexVertex[V]]): IndexVertex[V] = vs.map(v => (v, positionOf(v))).minBy(_._2)._1

  /**
   * @param from The vertex on this cycle, to the left of the first vertex on the path.
   * @param to The vertex on this cycle to the right of the last vertex on the path.
   * @param path A path of vertices in an embedding between 'from and 'to.
   * @return A new cycle where the vertices between v and w being replaced by the vertices on the path.
   */
  def addMatchingPath(from: IndexVertex[V], to: IndexVertex[V], path: Vector[IndexVertex[V]]): OuterCycle[V] = {
    val numberOfElementsToReplace = positionOf(to) - positionOf(from) - 1
    val replacementDifference = path.size - numberOfElementsToReplace
    val toPos = positionOf(to)
    val fromPos = positionOf(from)
    val newPositions = path.zipWithIndex.map(vi => (vi._1, vi._2 + fromPos + 1))
    var clearedPositions = positions.map(pos => {
      if (pos > fromPos && pos < toPos) -1
      else if (pos >= toPos) pos + replacementDifference
      else pos
    })

    for (p <- newPositions)
      clearedPositions = clearedPositions.updated(p._1.index, p._2)

    new OuterCycle(vertices.patch(fromPos + 1, path, numberOfElementsToReplace), clearedPositions)
  }

  override val toString: String = {
    val builder = new StringBuilder()

    builder ++= "C: "

    for (v <- vertices) {
      builder ++= v + " [" + positionOf(v) + "] | "
    }

    builder.toString()
  }

}
