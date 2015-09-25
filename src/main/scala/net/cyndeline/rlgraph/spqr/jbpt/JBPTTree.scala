package net.cyndeline.rlgraph.spqr.jbpt

import net.cyndeline.rlgraph.spqr.{Edge, SPQRNode, SPQRTree}

import scala.collection.mutable

/**
 * SPQR data structure.
 */
class JBPTTree[VType](val root: SPQRNode[VType]) extends SPQRTree[VType] {
  private val nodeSet = new mutable.HashSet[SPQRNode[VType]]()
  addNode(root)

  private var currentSize = 1 // 1 for the initial root

  /** @return Every node in the tree. */
  override def nodes: Vector[SPQRNode[VType]] = nodeSet.toVector

  /** @return The triconnected components of the tree. Retrieves in O(n) time. */
  override def leaves: Vector[SPQRNode[VType]] = nodes.filter(_.children.isEmpty)

  /** Removes a node and all its successors from the SPQR tree. */
  override def deleteNode(n: SPQRNode[VType]): Unit = {
    if (!n.isInstanceOf[HashNode[VType]]) {
      throw new Error("Cannot process nodes of type " + n.getClass + " as " + classOf[HashNode[VType]])
    } else if (!nodeSet.contains(n)) {
      throw new Error("Cannot delete child " + n + " as it is not present in the SPQR structure.")
    } else if (n == root) {
      throw new Error("Cannot delete the root of the SPQR tree.")
    }

    n.parent.get.asInstanceOf[HashNode[VType]].removeChild(n)
    currentSize -= 1
    nodeSet -= n

    val bfsQueue = new mutable.Queue[SPQRNode[VType]]()
    for (c <- n.children)
      bfsQueue.enqueue(c)

    while (!bfsQueue.isEmpty) {
      val child = bfsQueue.dequeue()
      for (c <- child.children)
        bfsQueue.enqueue(c)

      nodeSet -= child
      currentSize -= 1
    }
  }

  /** @return The number of nodes in the tree, including the root. */
  override def size: Int  = currentSize

  /**
   * @param a A node in the tree.
   * @param b A child of parent of the other node.
   * @return The virtual edge connecting the two nodes.
   */
  override def virtualEdge(a: SPQRNode[VType], b: SPQRNode[VType]): Edge[VType] = {
    if (nodeSet.contains(a) && nodeSet.contains(b)) {
      if ((a.parent.isDefined && a.parent.get == b) || b.parent.isDefined && b.parent.get == a) {
        val commonEdges = a.asInstanceOf[HashNode[VType]].virtualEdges intersect b.asInstanceOf[HashNode[VType]].virtualEdges
        commonEdges.head // Return!
      } else {
        throw new IllegalArgumentException("The supplied nodes " + a + " and " + b + " has no virtual edge connecting them, as neither has the other as a parent.")
      }

    } else {
      val missingNodes = nodeSet intersect Set(a, b)
      throw new IllegalArgumentException("Cannot find virtual edge between nodes " + a + " and " + b + ", as the following nodes are not in the tree: " + missingNodes.mkString(", "))
    }
  }

  def addChild(child: HashNode[VType]): Unit = {
    if (child.parent.isEmpty)
      throw new Error("Every non-root node in the SPQR tree must have a parent (" + child + " does not).")
    else if (nodeSet.contains(child))
      throw new Error("The node " + child + " cannot be added to the SPQR tree twice.")
    else if (!nodeSet.contains(child.parent.get))
      throw new Error("Cannot add a child (" + child + ") whose parent (" + child.parent.get + ") isn't in the tree.")

    child.parent.get.asInstanceOf[HashNode[VType]].addChild(child)
    addNode(child)
  }

  private def addNode(n: SPQRNode[VType]) {
    nodeSet += n
    currentSize += 1
  }

  override def toString: String = {
    val str = new StringBuffer()
    val stack = new mutable.Stack[(SPQRNode[VType], Int)]()
    stack.push((root, 0))

    while (!stack.isEmpty) {
      val n = stack.pop()
      for (i <- 0 until n._2)
        str.append("-")

      str.append(n._1.toString + scala.util.Properties.lineSeparator)

      for (child <- n._1.children)
        stack.push((child, n._2 + 1))
    }
    str.toString
  }
}
