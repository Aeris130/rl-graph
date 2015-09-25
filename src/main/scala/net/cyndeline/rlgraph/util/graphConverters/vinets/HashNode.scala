package net.cyndeline.rlgraph.util.graphConverters.vinets

import java.util

import de.fhstralsund.vinets.geometry.NetElementGeometry
import de.fhstralsund.vinets.graphics.NetElementRepresentation
import de.fhstralsund.vinets.structure.{NetElement, Node, NodeType}

import scala.collection.mutable

/**
 * Node compatible with Vinets graph library, uses a deterministic hash code. Cannot extend the SimpleNode used by Vinet
 * since it is a protected inner class of the SimpleGraph class.
 */
class HashNode(id: Int, nodeName: String) extends Node {
  private var currentName: String = nodeName
  private var labels = new mutable.HashMap[Any, Any]()
  private val edges = new util.ArrayList[HashEdge]()

  /* Custom methods */

  def addEdge(edge: HashEdge) {
    edges.add(edge)
  }

  /* End customs */


  def setBooleanLabel(p1: scala.Any, p2: Boolean): Unit = ???

  def setDoubleLabel(p1: scala.Any, p2: Double): Unit = ???

  def setIntLabel(p1: scala.Any, p2: Int): Unit = ???

  def clearLabels(): Unit = ???

  def setLabel(p1: scala.Any, p2: scala.Any): Unit = labels += (p1 -> p2)

  def setData(p1: scala.Any): Unit = ???

  def setRepresentation(p1: NetElementRepresentation): Unit = ???

  def setGeometry(p1: NetElementGeometry): Unit = ???

  def setName(p1: String): Unit = currentName = p1

  def isAtomar: Boolean = ???

  def outdegree(): Int = ???

  def indegree(): Int = ???

  def degree(): Int = edges.size

  def outArcs(): util.Iterator[_] = ???

  def inArcs(): util.Iterator[_] = ???

  def undirectedEdges(): util.Iterator[_] = edges.iterator()

  def incidentEdges(): util.Iterator[_] = ???

  def getNodeType: NodeType = ???

  def getID: AnyRef = id.asInstanceOf[AnyRef]

  def getName: String = currentName

  def getGeometry: NetElementGeometry = ???

  def getRepresentation: NetElementRepresentation = ???

  def getData: AnyRef = ???

  def getHost: NetElement = ???

  def getOriginal: NetElement = ???

  def getLabel(p1: scala.Any): AnyRef = labels(p1).asInstanceOf[AnyRef]

  def getLabelKeySet: util.Set[_] = ???

  def containsLabel(p1: scala.Any): Boolean = ???

  def removeLabel(p1: scala.Any): AnyRef = ???

  def getIntLabel(p1: scala.Any): Int = ???

  def getDoubleLabel(p1: scala.Any): Double = ???

  def getBooleanLabel(p1: scala.Any): Boolean = ???

  override def equals(other: Any): Boolean = other match {
    case n: HashNode => {
      if (this eq n)
        true
      else if (n.getID equals id)
        true
      else
        false
    }
    case _ => false
  }

  override def hashCode: Int = id

  override def toString: String = "Node " + id + (if (currentName != "") ", name: " + currentName + "." else "")
}
