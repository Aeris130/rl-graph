package net.cyndeline.rlgraph.cycles

import net.cyndeline.rlgraph.util.GraphCommons

/**
 * A cycle in a graph. The last element is connected to the first.
 *
 * @param vertices Every vertex in the cycle.
 * @param isUndirected If undirected, two cycles with reversed vertex orderings are considered to be matching
 *                     (i.e 1, 2, 3, 4 is equal to 4, 3, 2, 1).
 */
class Cycle[V] private (val vertices: Vector[V], val isUndirected: Boolean) {

  val length: Int = vertices.size

  val isSimple: Boolean = vertices.distinct.size == length

  val edges: Vector[(V, V)] = makeEdges()

  val edgeSize: Int = edges.size

  def reverse: Cycle[V] = new Cycle(vertices.reverse, isUndirected)

  def makeUndirected: Cycle[V] = new Cycle(vertices, true)

  override def equals(other: Any): Boolean = other match {
    case c: Cycle[V] => {
      if (c.isUndirected != isUndirected) return false
      else if (c.edgeSize != edgeSize || c.length != length) return false
      else if (this eq c) return true
      else if (this.## != c.##) return false

      if (!isUndirected)
        compareEdges(c.edges)
      else
        compareEdges(c.edges) || compareEdges(c.edges.reverse.map(_.swap))
    }
    case _ => false
  }

  override def hashCode: Int = vertices.map(_.##).sum

  override val toString: String = {
    val builder = new StringBuilder()
    builder ++= "Cycle: " + vertices.mkString(", ")
    if (isUndirected)
      builder ++= " (undirected)"

    builder.toString()
  }

  private def makeEdges(): Vector[(V, V)] = {
    val listWithLast = vertices :+ vertices.head
    listWithLast zip listWithLast.tail
  }

  private def compareEdges(otherEdges: Vector[(V, V)]): Boolean = {
    val startEdge = otherEdges.head
    val localStartEdge = edges.find(_ == startEdge).getOrElse(return false)
    val localEdgesWithStartFirst = GraphCommons.placeElementFirst(localStartEdge, edges)
    localEdgesWithStartFirst == otherEdges
  }
}

object Cycle {
  def apply[V](vertices: Vector[V]) = new Cycle(vertices, false)
  def apply[V](v: V, vs: V*) = new Cycle(Vector(v) ++ vs.toVector, false)
  def undirected[V](vertices: Vector[V]) = new Cycle(vertices, true)
  def undirected[V](v: V, vs: V*) = new Cycle(Vector(v) ++ vs.toVector, true)
}
