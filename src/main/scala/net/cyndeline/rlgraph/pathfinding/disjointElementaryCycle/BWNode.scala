package net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle

import net.cyndeline.rlgraph.embedding.Embedding

/**
 * Represents and black or white node in a BlackWhiteGraph. Black nodes are single vertices, while white nodes
 * represent a subgraph.
 *
 * @param subg The subgraph represented by this node, or None if the node represents a single vertex instead.
 * @param v The single vertex represented by this node, or None if the node represents a subgraph instead.
 */
class BWNode[VType] private (subg: Option[Embedding[VType]], v: Option[VType]) {

  /**
   * Constructs a node that represents a subgraph.
   * @param subgraph Subgraph to represent.
   */
  def this(subgraph: Embedding[VType]) = this(Some(subgraph), None)

  /**
   * Constructs a node that represents a single vertex.
   * @param vertex Vertex to represent.
   */
  def this(vertex: VType) = this(None, Some(vertex))

  /** @return True if this node represents a subgraph, otherwise false. */
  def isWhite: Boolean = subg.isDefined

  /** @return True if ths node represents a single vertex, otherwise false. */
  def isBlack: Boolean = v.isDefined

  /** @return The subgraph represented by this node. */
  def subgraph: Embedding[VType] = subg.getOrElse(throw new Error("Cannot retrieve a subgraph from a black/white node representing a single vertex."))

  /** @return The single vertex represented by this node. */
  def vertex: VType = v.getOrElse(throw new Error("Cannot retrieve a single vertex from a black/white node representing a subgraph."))

  override def equals(other: Any): Boolean = other match {
    case bw: BWNode[VType] => {
      if (bw.isBlack == isBlack && bw.isWhite == isWhite) {
        if (isBlack) {
          bw.vertex == vertex
        } else {
          bw.subgraph == subgraph
        }
      } else {
        false
      }
    }
    case _ => false
  }

  override def hashCode: Int = if (isBlack) v.## else subg.##

  override def toString: String = (if (isBlack) "Black" else "White") + " Node: " + (if (isBlack) vertex else subgraph)
}

object BWNode {
  def apply[VType](subgraph: Embedding[VType]) = new BWNode(Some(subgraph), None)
  def apply[VType](vertex: VType) = new BWNode(None, Some(vertex))
}
