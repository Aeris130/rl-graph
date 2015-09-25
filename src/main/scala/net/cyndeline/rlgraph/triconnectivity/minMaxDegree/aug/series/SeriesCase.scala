package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.series

import net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.{Augmentation, AugmentationAmount}

/**
 * Computes the augmentation set for an S-node, based on the sets of its children.
 *
 * @param childAugmentations The computed augmentation values of all children belonging to the S-node currently
 *                           being processed. These must be placed in the order they're visited when walking
 *                           clockwise around the cycle. The second value in each tuple is the computed augmentations
 *                           for all children of each child (order is irrelevant), or Nil if the child has no children.
 */
class SeriesCase(childAugmentations: Vector[(AugmentationAmount, Vector[AugmentationAmount])]) extends AugmentationAmount {
  private val childArray: Array[AugmentationAmount] = childAugmentations.map(_._1).toArray
  private val childChildArray = childAugmentations.map(_._2).toArray

  /**
   * @return Every computed set of edge augmentations that results in a triconnected component. The fir
   */
  def aug: Vector[Augmentation] = computeCompleteAug

  /** @return True if the implementing class contains the specified augmentation, otherwise false. */
  def containsAugmentation(x: Int, y: Int): Boolean = aug.contains(Augmentation(x, y))

  override def containsAugmentations(as: (Int, Int)*): Boolean = ???

  private def computeCompleteAug: Vector[Augmentation] = {

    /* Find the maximal augmentation (x + y) for each child. */
    val maximalChildAugs: Array[Augmentation] = for (augmentation <- childArray) yield augmentation.aug.foldLeft(augmentation.aug.head)((a, b) => if ((a.x + a.y) > (b.x + b.y)) a else b)
    val singleAugmentation = computeSingleAug(maximalChildAugs)

    /* Run the algorithm again if a child's child contains the augmentations (2,2), (1,1) or (2,1), (1,0).
     * If (2,2)... is found, the algorithm is run by replacing that childs augmentation with (1,1).
     * If (2,1)... is found, (1,0) is used instead.
     */
    val childFound = childWithMatchingAugmentations

    if (childFound.isDefined) {
      val cIndex = childFound.get._1
      val augmentationToUse = if (childFound.get._2) Augmentation(1, 1) else Augmentation(1, 0)
      maximalChildAugs(cIndex) = augmentationToUse
      val secondAugmentation = computeSingleAug(maximalChildAugs)

      Vector(singleAugmentation, secondAugmentation)
    } else {
      Vector(singleAugmentation)
    }

  }

  private def computeSingleAug(augmentations: Array[Augmentation]): Augmentation = {

    /* Needed since Kant uses indices in his algorithm description. For simplicity's sake, a scrap value is added
     * at index 0, to allow indices 1 -> k to be used as in the description.
     */
    val x: Array[Int] = -10000 +: augmentations.map(_.x)
    val y: Array[Int] = -10000 +: augmentations.map(_.y)

    var nLeft = x(1)
    var nRight = y(1)

    var i = 2
    val k = augmentations.size

    while (i <= k) {
      nLeft = Math.abs(nLeft - y(i))
      nRight = nRight + x(i)
      var nPrimeLeft = 0
      var j = i

      while (nPrimeLeft < nLeft && j < k) {
        j += 1
        nPrimeLeft += x(j)
        nRight += y(j)
      }

      nLeft = nPrimeLeft - nLeft

      while (nRight > 2) {
        nRight -= 2
      }

      i = j + 1

      //swap left/right
      val tmp = nLeft
      nLeft = nRight
      nRight = tmp
    }

    Augmentation(nLeft, nRight)
  }

  /**
   * Checks if the children of any child contains {(2,2), (1,1)} or {(2,1), (1,0)}.
   * @return Index of childs child found, or None if no such child was found. The second value is set to true if
   *         the augmentations found were 2,2 and 1,1, otherwise false if 2,1 and 1,0.
   */
  private def childWithMatchingAugmentations: Option[(Int, Boolean)] = {
    var i = 0
    while (i < childChildArray.size) {
      val childsChildren: Vector[AugmentationAmount] = childChildArray(i)
      for (c <- childsChildren) {
        if (augAmountContains(c, 2, 2) && augAmountContains(c, 1, 1))
          return Some((i, true))
        else if (augAmountContains(c, 2, 1) && augAmountContains(c, 1, 0))
          return Some((i, false))
      }

      i += 1
    }

    None
  }

  private def augAmountContains(amounts: AugmentationAmount, x: Int, y: Int): Boolean = amounts.aug.exists(a => a.x == x && a.y == y)
}
