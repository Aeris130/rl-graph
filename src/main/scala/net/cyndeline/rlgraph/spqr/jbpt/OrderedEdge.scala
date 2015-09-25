package net.cyndeline.rlgraph.spqr.jbpt

import net.cyndeline.rlgraph.spqr.Edge

/**
 * Represents an undirected solid or virtual edge in an SPQR tree. Sorts vertices according to hash.
 */
class OrderedEdge[VType](a: VType, b: VType) extends Edge[VType] {
  val from = if (a.## < b.##) a else b
  val to = if (from == a) b else a

  private val hash = from.## ^ to.##

  override def equals(other: Any): Boolean = other match {
    case edge: OrderedEdge[VType] => edge.from == from && edge.to == to
    case _ => false
  }

  override def hashCode: Int = hash

  override def toString: String = "[Edge: " + from + " ~ " + to + "]"

}

object OrderedEdge {
  def apply[VType](a: VType, b: VType): OrderedEdge[VType] = new OrderedEdge(a, b)
}