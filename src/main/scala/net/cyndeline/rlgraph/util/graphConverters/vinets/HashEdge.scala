package net.cyndeline.rlgraph.util.graphConverters.vinets

import java.util

import de.fhstralsund.vinets.structure._

/**
 * Extends the Vinets library edge interface, as the SimpleEdge is a protected inner class.
 * Adds an equals and hash method that results in equal orderings when inserted into an
 * edge set multiple times.
 */
class HashEdge(source: Node, target: Node) extends DefaultNetElement with Edge {

  def getSource: Node = source

  def getTarget: Node = target

  def getEnds: NodePair = new NodePair(source, target) // <--------- Custom hash structure?

  def getOtherEnd(p1: Node): Node = {
    if (p1 == source)
      target
    else if (p1 == target)
      source
    else throw new Error("The node " + p1 + " was not a member of " + this + ".")
  }

  def incidentNodes(): util.Iterator[_] = {
    val l = new util.ArrayList[Any]()
    l.add(source)
    l.add(target)
    l.iterator()
  }

  def isDirected: Boolean = false

  def isUndirected: Boolean = true

  override def equals(other: Any): Boolean = other match {
    case h: HashEdge => {
      if (this eq h)
        true
      else if (source == h.getSource && target == h.getTarget)
        true
      else false
    }
    case _ => false
  }

  override def hashCode: Int = source.## ^ target.##

  override def toString: String = "(" + source + ", " + target + ")"
}
