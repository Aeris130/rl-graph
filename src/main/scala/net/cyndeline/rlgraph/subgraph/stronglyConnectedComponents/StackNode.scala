package net.cyndeline.rlgraph.subgraph.stronglyConnectedComponents

/**
 * Stores data belonging to each vertex during the algorithm.
 */
class StackNode[V](val vertex: V) {
  private var inx: Option[Int] = None
  private var lLink: Option[Int] = None

  var onStack = false

  def index_=(newIndex: Int) { inx = Some(newIndex) }
  def index: Int = {
    inx.getOrElse(throw new Error("Index value for vertex " + vertex + " not set."))
  }

  def lowLink_=(newLowLink: Int) { lLink = Some(newLowLink) }
  def lowLink: Int = {
    lLink.getOrElse(throw new Error("Low-link value for vertex " + vertex + " not set."))
  }

  def indexDefined: Boolean = inx.isDefined

  override def equals(other: Any): Boolean = other match {
    case sn: StackNode[V] => vertex == sn.vertex
    case _ => false
  }

  override def hashCode: Int = vertex.##
}
