package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.rigid

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.{Augmentation, Component}

import scala.collection.mutable

/**
 * Adds information about which 1-and 2-sets a component is stored in to a component. Storing the sets that this
 * component is a member of in itself, allows the algorithm to run in O(n) time.
 */
class ComponentWrapper[VType](val component: Component[VType], val aug: Augmentation) {

  // Maps each face to the tuple (1-set, 2-set).
  private var oneSets = Map[Face[VType], mutable.HashSet[ComponentWrapper[VType]]]()
  private var twoSets = Map[Face[VType], mutable.HashSet[ComponentWrapper[VType]]]()

  def addToFaceSets(f: Face[VType], oneSet: mutable.HashSet[ComponentWrapper[VType]], twoSet: mutable.HashSet[ComponentWrapper[VType]]) {
    if(aug.x == 1) {
      oneSet add this
      oneSets += f -> oneSet
    } else {
      twoSet add this
      twoSets += f -> twoSet
    }

    if (aug.y != aug.x && aug.y > 0) {
      if (aug.y == 1) {
        oneSet add this
        oneSets += f -> oneSet
      } else {
        twoSet add this
        twoSets += f -> twoSet
      }
    }
  }

  /**
   * Removes this wrapper from the 1-set of a specified face, as well as the 1-set of the opposite face if
   * this wrapper does not contain the augmentation (1,1).
   */
  def removeFromOneSet(face: Face[VType]) {
    removeFromSet(face, OneSet)
  }

  /**
   * Removes this wrapper from the 2-set of a specified face, as well as the 2-set of the opposite face if
   * this wrapper does not contain the augmentation (2,2).
   */
  def removeFromTwoSet(face: Face[VType]) {
    removeFromSet(face, TwoSet)
  }

  private def removeFromSet(face: Face[VType], t: FaceSetType) {
    val setMap = getSetMap(t)
    setMap(face).remove(this)

    /* Only keep this wrapper in the specified set type of the opposite face if this wrapper has two
     * aug values of the same value, otherwise this component has already been matched in the specified
     * set, and is no longer available using the same set type in the other face.
     */
    if (aug.x != aug.y) {
      val otherFace = setMap.keys.find(_ != face).get
      setMap(otherFace).remove(this)
    }
  }

  private def getSetMap(t: FaceSetType) = t match {
    case OneSet => oneSets
    case TwoSet => twoSets
  }


}
