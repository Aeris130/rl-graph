package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.parallel

import net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.Component

import scala.collection.mutable

/**
 * Stores parallel sets k1 to k5.
 */
class KSets[VType] private (sets: (Set[CollapsedParallelComponent[VType]], Set[CollapsedParallelComponent[VType]], Set[CollapsedParallelComponent[VType]], Set[CollapsedParallelComponent[VType]], Set[CollapsedParallelComponent[VType]])) {

  private def this(children: Vector[Component[VType]]) = {
    this((Set[CollapsedParallelComponent[VType]](), Set[CollapsedParallelComponent[VType]](), Set[CollapsedParallelComponent[VType]](), Set[CollapsedParallelComponent[VType]](), Set[CollapsedParallelComponent[VType]]()))
    collapseToSets(children)
  }

  private var k1Set = sets._1
  private var k2Set = sets._2
  private var k3Set = sets._3
  private var k4Set = sets._4
  private var k5Set = sets._5

  /** All components with a (1,1) production. */
  def k1 = k1Set

  /** All components with a (2,2) production. */
  def k2 = k2Set

  /** All components with a (2,1) production. */
  def k3 = k3Set

  /** All components with a (1,0) production. */
  def k4 = k4Set

  /** All components with a (2,0) production. */
  def k5 = k5Set

  /**
   * Creates a new KSet collection with a specified component moved from one set to another.
   * @param c Component to move.
   * @param from Index of set to move from.
   * @param to Index of set to move to
   * @return A new KSet with the specified component moved to the new set.
   */
  def moveComponent(c: CollapsedParallelComponent[VType], from: Int, to: Int): KSets[VType] = {
    if (from < 1 || from > 5) throw new IllegalArgumentException("Components can only be moved between sets 1 to 5.")
    else if (to < 1 || to > 5) throw new IllegalArgumentException("Components can only be moved between sets 1 to 5.")
    else if (from == to) throw new IllegalArgumentException("Components must be moved between different sets.")

    removeFrom(from, c).addTo(to, c)
  }

  /**
   * Removes a component from the KSet.
   * @param i Index of set (1 to 5) to remove component from.
   * @param c Component to remove.
   * @return A new set collection with the component removed from the specified set index.
   */
  def removeFrom(i: Int, c: CollapsedParallelComponent[VType]): KSets[VType] = i match {
    case 1 => new KSets(k1Set - c, k2, k3, k4, k5)
    case 2 => new KSets(k1, k2Set - c, k3, k4, k5)
    case 3 => new KSets(k1, k2, k3Set - c, k4, k5)
    case 4 => new KSets(k1, k2, k3, k4Set - c, k5)
    case 5 => new KSets(k1, k2, k3, k4, k5Set - c)
  }

  /**
   * Adds a component to the KSet.
   * @param i Index of set (1 to 5) to add component to.
   * @param c Component to add.
   * @return A new set collection with the component added to the specified set index.
   */
  def addTo(i: Int, c: CollapsedParallelComponent[VType]): KSets[VType] = i match {
    case 1 => new KSets(k1Set + c, k2, k3, k4, k5)
    case 2 => new KSets(k1, k2Set + c, k3, k4, k5)
    case 3 => new KSets(k1, k2, k3Set + c, k4, k5)
    case 4 => new KSets(k1, k2, k3, k4Set + c, k5)
    case 5 => new KSets(k1, k2, k3, k4, k5Set + c)
  }

  /**
   * Removes all components from a set index.
   * @param i Index of set (1 to 5) to remove components from.
   * @return A new set collection with the specified set index being empty.
   */
  def removeAll(i: Int): KSets[VType] = i match {
    case 1 => new KSets[VType](Set(), k2, k3, k4, k5)
    case 2 => new KSets[VType](k1, Set(), k3, k4, k5)
    case 3 => new KSets[VType](k1, k2, Set(), k4, k5)
    case 4 => new KSets[VType](k1, k2, k3, Set(), k5)
    case 5 => new KSets[VType](k1, k2, k3, k4, Set())
  }

  private def collapseToSets(children: Vector[Component[VType]]) {
    val tempK1 = new mutable.ArrayBuffer[CollapsedParallelComponent[VType]]()
    val tempK2 = new mutable.ArrayBuffer[CollapsedParallelComponent[VType]]()
    val tempK3 = new mutable.ArrayBuffer[CollapsedParallelComponent[VType]]()
    val tempK4 = new mutable.ArrayBuffer[CollapsedParallelComponent[VType]]()
    val tempK5 = new mutable.ArrayBuffer[CollapsedParallelComponent[VType]]()

    val cs = children.iterator
    while(cs.hasNext) {
      val c = cs.next()

      if (c.containsAugmentation(1, 1))
        tempK1 += K1Single(c)
      else if (c.containsAugmentation(2, 2))
        tempK2 += K2Single(c)
      else if (c.containsAugmentation(2, 1))
        tempK3 += K3Single(c)
      else if (c.containsAugmentation(1, 0))
        tempK4 += K4Single(c)
      else if (c.containsAugmentation(2, 0))
        tempK5 += K5Single(c)
      else
        throw new Error("Child component " + c + " did not contain an augmentation required to process its parallel parent.")
    }

    k1Set = tempK1.toSet
    k2Set = tempK2.toSet
    k3Set = tempK3.toSet
    k4Set = tempK4.toSet
    k5Set = tempK5.toSet
  }

}

/**
 * Factory object used to create new KSets.
 */
object KSets {
  def apply[VType](children: Vector[Component[VType]]) = new KSets(children)
}
