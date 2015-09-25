package net.cyndeline.rlgraph.spqr.jbpt

import net.cyndeline.rlgraph.spqr.{Edge, NodeType, SPQRNode}

/**
 * @param parent The predecessor of this node in the SPQR tree, or None if this node is the root.
 * @param parentCutPair The vertex pair connecting this node with its parent, or None if this node is the root.
 * @param nodeType The type of this node (S, P or R).
 */
class HashNode[VType] private (val id: Int,
                               override val parent: Option[SPQRNode[VType]],
                               override val parentCutPair: Option[(VType, VType)],
                               override val nodeType: NodeType) extends SPQRNode[VType] {
  private var childList = Vector[SPQRNode[VType]]()
  private var solidEdgeList = Vector[Edge[VType]]()
  private var virtualEdgeList = Vector[Edge[VType]]()
  private var vertexSet = Set[VType]()

  def this(id: Int, nodeType: NodeType) = this(id, None, None, nodeType)
  def this(id: Int, parent: SPQRNode[VType], parentCutPair: (VType, VType), nodeType: NodeType) = this(id, Some(parent), Some(parentCutPair), nodeType)

  /** @return A list of children connected to this node in the SPQR tree. An Empty list if this node is a leaf. */
  override def children: Vector[SPQRNode[VType]] = childList

  /** Every vertex represented by this node. */
  override def vertices: Vector[VType] = vertexSet.toVector

  /** Every solid and virtual edge in the node. */
  override def edges: Vector[Edge[VType]] = (solidEdgeList ++ virtualEdgeList).toSet.toVector

  /** @return Every edge of the component represented by this node that existed in the original graph. */
  override def solidEdges: Vector[Edge[VType]] = solidEdgeList

  /** @return Virtual edges added by the SPQR tree. */
  override def virtualEdges: Vector[Edge[VType]] = virtualEdgeList

  /**
   * Adds a child node to this one. Do not call directly, use the JBPTTree class.
   * @param child Node specifying this node as parent.
   */
  def addChild(child: SPQRNode[VType]): Unit = {
    if (child.parent.isEmpty)
      throw new Error("Cannot add child without parent.")
    else if (child.parent.get != this)
      throw new Error("Cannot add child to node " + this + " with parent " + child.parent.get)

    childList = child +: childList
  }

  /**
   * Removes a child from this node. Do not call directly, use the JBPTTree class.
   * @param child Node specifying this node as parent.
   */
  def removeChild(child: SPQRNode[VType]): Unit = {
    if (child.parent.isEmpty || child.parent.get != this)
      throw new Error("Cannot remove a child (" + child + ") that doesn't specify this node (" + this + ") as parent.")

    childList = childList.filterNot(_ == child)
  }

  /**
   * @param edge An edge connecting two vertices that are represented by this node in the SPQR tree.
   */
  def addSolidEdge(edge: Edge[VType]): Unit = {
    vertexSet += edge.from
    vertexSet += edge.to
    solidEdgeList = edge +: solidEdgeList
  }

  /**
   * Adds a virtual edge. Note that if a vertex pair has both a solid and a virtual edge, both must be added
   * separately. Multiple virtual edges may be present between the same node pair.
   * @param edge An edge representing the connection of another node, using the vertices of the edge as a cut pair.
   */
  def addVirtualEdge(edge: Edge[VType]): Unit = {
    vertexSet += edge.from
    vertexSet += edge.to
    virtualEdgeList = edge +: virtualEdgeList
  }

  override def equals(other: Any): Boolean = other match {
    case hn: SPQRNode[VType] => hn.id == id
    case _ => false
  }

  override def hashCode: Int = id.##

  override def toString = "[Node " + id + ", " + nodeType + ", edges: " + solidEdges + ", virtual edges: " + virtualEdges + "]"

}

object HashNode {
  def apply[VType](id: Int, nodeType: NodeType) = new HashNode[VType](id, None, None, nodeType)
  def apply[VType](id: Int, parent: SPQRNode[VType], parentCutPair: (VType, VType), nodeType: NodeType) = new HashNode[VType](id, Some(parent), Some(parentCutPair), nodeType)
}