package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug

/**
 * Implemented by objects that computes augmentations.
 */
trait AugmentationAmount {

  /**
   * @return Every computed set of edge augmentations that results in a triconnected component. Cannot be empty.
   */
  def aug: Vector[Augmentation]

  /** @return True if the implementing class contains the specified augmentation, otherwise false. */
  def containsAugmentation(x: Int, y: Int): Boolean

  /** @return True if the implementing class contains all supplied augmentations, otherwise false. */
  def containsAugmentations(as: (Int, Int)*): Boolean

}
