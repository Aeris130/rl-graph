package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.rigid

import net.cyndeline.rlgraph.face.Face

import scala.collection.mutable

/**
 * Stores the 2-sets and 1-sets corresponding to each face of a graph.
 * Note that removing a component from its sets can be done by accessing them from inside the component wrapper.
 */
class FaceSets[VType](faces: Vector[Face[VType]]) {
  private val setMappings: Map[Face[VType], FSets] = (for (f <- faces) yield f -> new FSets()).toMap

  private class FSets {
    val oneSet = mutable.HashSet[ComponentWrapper[VType]]()
    val twoSet = mutable.HashSet[ComponentWrapper[VType]]()
  }

  def oneSetFor(face: Face[VType]): mutable.HashSet[ComponentWrapper[VType]] = setMappings(face).oneSet

  def twoSetFor(face: Face[VType]): mutable.HashSet[ComponentWrapper[VType]] = setMappings(face).twoSet

  /**
   * Registers a child component b(i) to the two faces its virtual edge is a member of in parent(b(i)).
   * @param c Component to add.
   * @param f1 A face sharing the edge representing the child in its parent.
   * @param f2 The face opposite f1 in the components parent.
   */
  def addComponentToSets(c: ComponentWrapper[VType], f1: Face[VType], f2: Face[VType]) {
    val f1Sets = setMappings(f1)
    val f2Sets = setMappings(f2)

    c.addToFaceSets(f1, f1Sets.oneSet, f1Sets.twoSet)
    c.addToFaceSets(f2, f2Sets.oneSet, f2Sets.twoSet)
  }

}
