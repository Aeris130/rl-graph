package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util

/**
 * Keeps track of incoming and outgoing flow.
 *
 * @constructor Creates a new flow vertex.
 * @param id Unique id for this node. Looking up graph data is based on this id alone.
 * @param production Positive if this vertex is a source, negative if it is a sink, or 0 if intermediate.
 */
case class FlowVertex(id: Int, production: Int) {

  /**
   * Creates a flow vertex with 0 production.
   * @param id Unique id for this node. Looking up graph data is based on this id alone.
   * @return A new flow vertex with 0 production.
   */
  def this(id: Int) = this(id, 0)

  /**
   * Creates a copy of this vertex with a new production.
   * @param production Production for the copy.
   * @return a new copy of the vertex with the specified production.
   */
  def newProduction(production: Int): FlowVertex = new FlowVertex(id, production)

  override def equals(other: Any): Boolean = other match {
    case that: FlowVertex => that.id == id
    case _ => false
  }

  override def hashCode: Int = id.##

  override def toString: String = "[id:" + id + "|p:" + production + "]"
}
