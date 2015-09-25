package net.cyndeline.rlgraph.spqr

/**
 * Represents the triconnected components in a biconnected graph using the SPQR data structure.
 * @tparam VType Vertex type of the graph represented by this tree.
 */
trait SPQRTree[VType] {

  /** The root of the tree. */
  val root: SPQRNode[VType]

  /** @return The triconnected components of the tree. Retrieves in O(n) time. */
  def leaves: Vector[SPQRNode[VType]]

  /** @return Every node in the tree. */
  def nodes: Vector[SPQRNode[VType]]

  /** Removes a node and all its successors from the SPQR tree. */
  def deleteNode(n: SPQRNode[VType]): Unit

  /**
   * @param a A node in the tree.
   * @param b A child of parent of the other node.
   * @return The virtual edge connecting the two nodes.
   */
  def virtualEdge(a: SPQRNode[VType], b: SPQRNode[VType]): Edge[VType]

  /** @return The number of nodes in the tree, including the root. */
  def size: Int

}
