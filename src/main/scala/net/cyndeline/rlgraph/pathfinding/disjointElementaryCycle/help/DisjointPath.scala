package net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help

/**
 * A path between two vertices in a graph. When alternating edges along the path, vertex degree is minimized on
 * paths where a vertex V has degree 1 if the edge between V and its neighbor is used.
 *
 * @param path A disjoint path between two vertices in a graph.
 * @param deadEnd If one of the vertices in the original graph has degree 1, it is a dead end.
 */
class DisjointPath[VType] private (val path: Vector[VType], val deadEnd: Option[VType]) {
  if (path.size % 2 == 0)
    throw new IllegalArgumentException("Disjoint paths should have an even amount of edges (" + path + " has " + (path.size - 1) + ").")

  def this(path: Vector[VType], deadEnd: VType) = this(if (path.head == deadEnd) path else path.reverse, Some(deadEnd))

  def this(path: Vector[VType]) = this(path, None)

  /**
   * Compute alternating edges of the path.
   * @return A list of alternating edges beginning in the start vertex. Example: The list A, B, C, D, E parses to
   *         (A, B), (C, D).
   */
  def alternateEdges: Vector[(VType, VType)] = {

    /* if one vertex of the path is a dead end, that vertex will be at the head of the list.
     * Since the number of edges are even, the number of vertices are uneven. The last group
     * will only contain a single vertex, so it is dropped.
     */
    path.grouped(2).toVector.dropRight(1).map(pairList => (pairList(0), pairList(1)))
  }

  override def equals(other: Any): Boolean = other match {
    case otherPath: DisjointPath[VType] => otherPath.deadEnd == deadEnd && otherPath.path == path
    case _ => false
  }

  override def hashCode: Int = path.## ^ deadEnd.##

  override def toString: String = "Path: " + path + (if (deadEnd.isDefined) "(Dead end: " + deadEnd.get + ")" else "")
}

object DisjointPath {

  /**
   * @param path A disjoint path between two vertices in a graph.
   * @param deadEnd If one of the vertices in the original graph has degree 1, it is a dead end.
   */
  def apply[VType](path: Vector[VType], deadEnd: VType) = new DisjointPath(path, deadEnd)

  /**
   * @param path A disjoint path between two vertices in a graph.
   */
  def apply[VType](path: Vector[VType]) = new DisjointPath(path)
}
