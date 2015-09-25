package net.cyndeline.rlgraph.util.graphConverters.vinets

import java.util

import de.fhstralsund.vinets.geometry.NetElementGeometry
import de.fhstralsund.vinets.graphics.NetElementRepresentation
import de.fhstralsund.vinets.structure._

/**
 * Custom implementation of the Vinets library graph class. Stores vertices and edges in a fixed order.
 *
 * Only the methods related to planar undirected graphs are implemented.
 */
class HashGraph extends Graph {
  private var nextId: Int = 0
  private val nodeStore = new java.util.HashSet[Any]()
  private val edgeStore = new java.util.HashSet[Any]()

  def setBooleanLabel(p1: scala.Any, p2: Boolean): Unit = ???

  def setDoubleLabel(p1: scala.Any, p2: Double): Unit = ???

  def setIntLabel(p1: scala.Any, p2: Int): Unit = ???

  def clearLabels(): Unit = ???

  def setLabel(p1: scala.Any, p2: scala.Any): Unit = ???

  def setData(p1: scala.Any): Unit = ???

  def setRepresentation(p1: NetElementRepresentation): Unit = ???

  def setGeometry(p1: NetElementGeometry): Unit = ???

  def setName(p1: String): Unit = ???

  def isDirected: Boolean = false

  def isUndirected: Boolean = true

  def isMixed: Boolean = false

  def isHyperGraph: Boolean = false

  def countNodes(): Int = nodeStore.size

  def countEdges(): Int = edgeStore.size

  def nodes(): util.Iterator[_] = nodeStore.iterator()

  def nodeSet(): util.Set[_] = nodeStore

  def edges(): util.Iterator[_] = edgeStore.iterator()

  def edgeSet(): util.Set[_] = edgeStore

  def createNode(p1: String, p2: scala.Any): Node = {
    val node = new HashNode(nextId, p1)
    nextId += 1
    nodeStore.add(node)
    node
  }

  def createNode(p1: String): Node = createNode(p1, null)

  def createNode(): Node = createNode("", null)

  def createEdge(p1: Node, p2: Node, p3: Boolean, p4: scala.Any): Link = {
    val edge = new HashEdge(p1, p2)
    edgeStore.add(edge)
    p1.asInstanceOf[HashNode].addEdge(edge)
    p2.asInstanceOf[HashNode].addEdge(edge)
    edge
  }

  def createEdge(p1: Node, p2: Node, p3: Boolean): Link = createEdge(p1, p2, true, null) // Bool doesn't matter

  def createEdge(p1: Node, p2: Node): Link = createEdge(p1, p2, true, null) // Bool doesn't matter

  def remove(p1: NetElement): Boolean = ???

  def getGraphType: GraphType = GraphType.UNDIRECTED

  def getID: AnyRef = ???

  def getName: String = ???

  def getGeometry: NetElementGeometry = ???

  def getRepresentation: NetElementRepresentation = ???

  def getData: AnyRef = ???

  def getHost: NetElement = ???

  def getOriginal: NetElement = ???

  def getLabel(p1: scala.Any): AnyRef = ???

  def getLabelKeySet: util.Set[_] = ???

  def containsLabel(p1: scala.Any): Boolean = ???

  def removeLabel(p1: scala.Any): AnyRef = ???

  def getIntLabel(p1: scala.Any): Int = ???

  def getDoubleLabel(p1: scala.Any): Double = ???

  def getBooleanLabel(p1: scala.Any): Boolean = ???
}
