package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug

import net.cyndeline.rlgraph.spqr.SPQRNode

/**
 * Contains augmentation data for a single node in an SPQR tree.
 */
class Component[VType](val node: SPQRNode[VType], augmentations: Set[Augmentation]) extends AugmentationAmount {

  def aug: Vector[Augmentation] = augmentations.toVector

  def addAugmentation(x: Int, y: Int): Component[VType] = Component(node, augmentations + Augmentation(x, y))

  def removeAugmentation(x: Int, y: Int): Component[VType] = Component(node, augmentations - Augmentation(x, y))

  def containsAugmentation(x: Int, y: Int): Boolean = augmentations contains Augmentation(x, y)

  def containsAugmentations(as: (Int, Int)*): Boolean = as.forall(a => augmentations.contains(Augmentation(a._1, a._2)))

}

object Component {
  def apply[VType](n: SPQRNode[VType], aug: Set[Augmentation]): Component[VType] = new Component(n, aug)
}
