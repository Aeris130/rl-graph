package net.cyndeline.rlgraph.triangulation.minimumDegree.faceTriangulation.multiEdgePrevention.help

/**
 * Mutable node that allows visitation status to be set to true or false.
 */
class VisitedVertex[VType] private (val vertex: VType) {
  private var visited = false

  def isVisited: Boolean = visited

  def markAsVisited() {
    visited = true
  }

  override def equals(other: Any): Boolean = other match {
    case v: VisitedVertex[VType] => v.vertex == vertex
    case _ => false
  }

  override def hashCode: Int = vertex.##

  override def toString: String = vertex + " (" + (if (!visited) "not visited" else "visited") + ")"

}

object VisitedVertex {
  def apply[VType](v: VType) = new VisitedVertex(v)
}
