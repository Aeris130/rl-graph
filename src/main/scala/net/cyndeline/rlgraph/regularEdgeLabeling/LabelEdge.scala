package net.cyndeline.rlgraph.regularEdgeLabeling

/**
 * A directed edge member of a regular edge labeling, mapped to an index.
 */
case class LabelEdge[V](from: V, to: V, index: Int) {
  require(index >= 0, "Cannot create label edges with index < 0.")

  /** @return An edge with its from/to vertex switched. */
  def swap: LabelEdge[V] = LabelEdge(to, from, index)

  override val toString = "Edge[" + from + " -> " + to + " | index: " + index + "]"
}
