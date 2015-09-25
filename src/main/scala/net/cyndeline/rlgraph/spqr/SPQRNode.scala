package net.cyndeline.rlgraph.spqr

/**
 * An S, P or R node in an SPQR tree.
 * @tparam VType Vertex type of the graph represented by this node.
 */
trait SPQRNode[VType] {

  /** Unique identifier for the node. */
  val id: Int

  /** @return the predecessor of this node in the SPQR tree, or None if this node is the root. */
  def parent: Option[SPQRNode[VType]]

  /** @return The vertices connecting this node to its parent, or None if this is the root. */
  def parentCutPair: Option[(VType, VType)]

  /** @return A list of children connected to this node in the SPQR tree. An Empty list if this node is a leaf. */
  def children: Vector[SPQRNode[VType]]

  /** @return Every vertex represented by this node. */
  def vertices: Vector[VType]

  /** @return Every solid and virtual edge in the node. */
  def edges: Vector[Edge[VType]]

  /** @return Every edge of the skeleton represented by this node that existed in the original graph. */
  def solidEdges: Vector[Edge[VType]]

  /** @return Virtual edges added by the SPQR tree. */
  def virtualEdges: Vector[Edge[VType]]

  /** @return The type of this node (S, P or R). */
  def nodeType: NodeType

}
