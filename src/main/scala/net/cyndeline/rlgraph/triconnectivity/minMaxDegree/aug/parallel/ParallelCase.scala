package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.parallel

import net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.{Augmentation, Component}

import scala.collection.mutable.ArrayBuffer

/**
 * Computes the augmentation set for a P-node, as well as the order that the nodes children should be connected in
 * when merging them into a single triconnected component.
 */
class ParallelCase[VType](b: Component[VType], children: Vector[Component[VType]]) {
  private val solutionBelongingToAugmentation = computeAllResults

  case class PResult(aug: Augmentation, components: Vector[CollapsedParallelComponent[VType]])

  def augmentations: Set[Augmentation] = solutionBelongingToAugmentation.keySet

  def componentOrderingFor(a: Augmentation) = solutionBelongingToAugmentation(a)

  /**
   * Computes a single result for the augmentations of the supplied child list, and also one additional result
   * for each child with aug(bi) = {(2, 1), (1, 0)} (2,1 is changed to 1,0) as well as every child not part of the
   * first group, with aug(bi) = {(2,2), (2,0)} (2,2 is changed to 2,0).
   * @return A list of all optimal solutions aug(b), mapped to the components ordering required to obtain it.
   */
  private def computeAllResults: Map[Augmentation, PResult] = {

    /* One KSet contains the original set memberships. */
    val uncollapsedKSets = KSets(children)

    /* Compute all KSets that contains an optimal augmentation. */
    val kSets = new ParallelChildPermutations(uncollapsedKSets).childPermutations

    (for (kSet <- kSets) yield algorithmResult(kSet)).map(pr => pr.aug -> pr).toMap
  }

  private def algorithmResult(kSets: KSets[VType]): PResult = {
    var current = kSets
    current = collapseK1(current)
    current = collapseK2(current)
    current = reduceK4AndK5(current)
    current = collapseK4(current).getOrElse(current)
    current = collapseK5(current).getOrElse(current)
    current = joinK4K5(kSets).getOrElse(current)
    current = collapseK3(kSets)
    val augmentation = computeAugmentation(kSets)

    val componentOrdering = new ArrayBuffer[CollapsedParallelComponent[VType]]()

    /* If k4 has size 1, it ends up at the beginning of k1. */
    if (kSets.k4.size == 1)
      componentOrdering += kSets.k4.head

    componentOrdering ++= kSets.k1
    componentOrdering ++= kSets.k3 // Empty if components were added to k1 or k2.
    componentOrdering ++= kSets.k2

    if (kSets.k5.size == 1)
      componentOrdering += kSets.k5.head

    PResult(augmentation, componentOrdering.toVector)
  }

  /**
   * Can only be called while k1 contains only k1Singles.
   */
  private def collapseK1(sets: KSets[VType]): KSets[VType] = {
    if (!sets.k1.isEmpty) {
      val k1Components = sets.k1.toVector.map(_.asInstanceOf[K1Single[VType]].component)
      sets.removeAll(1).addTo(1, K1Collapse(k1Components))
    } else {
      sets
    }
  }

  /**
   * Can only be called while k2 contains only k1Singles.
   */
  private def collapseK2(sets: KSets[VType]): KSets[VType] = {
    if (!sets.k2.isEmpty) {
      val k2Components = sets.k2.toVector.map(_.asInstanceOf[K2Single[VType]].component)
      sets.removeAll(2).addTo(2, K2Collapse(k2Components))
    } else {
      sets
    }
  }

  /** Reduce the size of k4 and k5 such that |k4+k5| <= 2 by adding extra edges to the productions (2,0) and (1,0),
    * turning them into (2,1) and (1,1).
    */
  private def reduceK4AndK5(sets: KSets[VType]): KSets[VType] = {
    var result = reduceK5(sets)
    result = reduceK4(result)

    if (result.k4.size + result.k5.size > 2)
      throw new Error("The total size of sets k4 and k5 must be <= 2.")

    result
  }

  /**
   * If |k5| > 2, |k5| - 2 components in k5 receives an extra edge (2,0 -> 2,1) and is added to k3.
   * @param sets
   */
  private def reduceK5(sets: KSets[VType]): KSets[VType] = {
    var current = sets
    if (sets.k5.size > 2) {
      val components = sets.k5.iterator
      val k5Size = sets.k5.size
      val numberOfElemensToChange = k5Size - 2
      val elementsToAdjust = sets.k5.dropRight(k5Size - numberOfElemensToChange)
      for (e <- elementsToAdjust) {
        current = current.removeFrom(5, e)
        current = current.addTo(3, K3Single(e.asInstanceOf[K5Single[VType]].component))
      }

      current
    } else {
      sets
    }
  }

  /**
   * Can only be called after reduceK5.
   *
   * If k4 > (2 - k5), change (k4 - 2 + k5) augmentations from (1,0) to (1,1) and add them to k1.
   */
  private def reduceK4(sets: KSets[VType]): KSets[VType] = {
    val k4Size = sets.k4.size
    val k5Size = sets.k5.size
    if (k4Size > (2 - k5Size)) {
      var current = sets
      val numberOfElementsToRemove = k4Size - 2 + k5Size
      val elementsToAdjust = sets.k4.dropRight(k4Size - numberOfElementsToRemove)
      for (e <- elementsToAdjust) {
        current = current.removeFrom(4, e)
        current = current.addTo(1, K1Single(e.asInstanceOf[K4Single[VType]].component))
      }

      current
    } else {
      sets
    }
  }

  /** If k4 has two elements, collapse them to a single (1,1) component with a gap and add them to k1. */
  private def collapseK4(sets: KSets[VType]): Option[KSets[VType]] = {
    if (sets.k4.size == 2) {
      val k4List = sets.k4.toVector
      val a = k4List(0).asInstanceOf[K4Single[VType]]
      val b = k4List(1).asInstanceOf[K4Single[VType]]
      val aComp = a.component
      val bComp = b.component
      var current = sets.removeFrom(4, a).removeFrom(4, b)
      current = current.addTo(1, K4GapCollapse(aComp, bComp))
      Some(current)
    } else {
      None
    }
  }

  /** If k5 has to elements, collapse them to a single (2,2) component with a gap and add them to k2. */
  private def collapseK5(sets: KSets[VType]): Option[KSets[VType]] =  {
    if (sets.k5.size == 2) {
      val k5List = sets.k5.toVector
      val a = k5List(0).asInstanceOf[K5Single[VType]]
      val b = k5List(1).asInstanceOf[K5Single[VType]]
      val aComp = a.component
      val bComp = b.component
      var current = sets.removeFrom(5, a).removeFrom(5, b)
      current = current.addTo(2, K5GapCollapse(aComp, bComp))
      Some(current)
    } else {
      None
    }
  }

  /**
   * If k3 is odd, collapse it into a single (2,1) component and add it to k3.
   * Otherwise if k2 isn't empty, collapse it to a single (2,2) component and add it to k2.
   * Otherwise, collapse it to a single (1,1) component and add it to k1.
   */
  private def collapseK3(sets: KSets[VType]): KSets[VType] = {
    val cList = sets.k3.toVector.map(c => c -> c.asInstanceOf[K3Single[VType]].component)
    val components = cList.map(_._2)
    val current = sets.removeAll(3)

    if ((sets.k3.size % 2) != 0) {  // Collapse k3 to a single 2,1 component
      val singleComponent = new K3IntoK3Collapse[VType](components)
      current.addTo(3, singleComponent)

    } else { // |k3| even

      if (!sets.k2.isEmpty) {
        val singleComponent = K3IntoK2Collapse(components)
        current.addTo(2, singleComponent)
      } else {
        val singleComponent = K3IntoK1Collapse(components)
        current.addTo(1, singleComponent)
      }

    }
  }

  /** If one component exists in k4 and k5, they are removed and instead combined in a (2,1) component.
    */
  private def joinK4K5(sets: KSets[VType]): Option[KSets[VType]] = {
    if (sets.k4.size == 1 && sets.k5.size == 1) {
      var current = sets.removeAll(4).removeAll(5)
      val compPos = new K4K5IntoK3WithGapCollapse(sets.k4.head.asInstanceOf[K4Single[VType]].component, sets.k5.head.asInstanceOf[K5Single[VType]].component)
      current = current.addTo(3, compPos)
      Some(current)
    } else {
      None
    }
  }

  private def computeAugmentation(sets: KSets[VType]): Augmentation = {
    if (sets.k4.size == 1) {
      if (sets.k2.size > 0 || sets.k3.size > 0) Augmentation(2, 0)
      else Augmentation(1, 0)

    } else if (sets.k5.size == 1) {
      if (sets.k1.size > 0 || sets.k3.size > 0) Augmentation(1, 0)
      else Augmentation(2, 0)

    } else {
      if (sets.k3.size > 0 || (sets.k1.size > 0 && sets.k2.size > 0)) Augmentation(2, 1)
      else if (sets.k1.size > 0) Augmentation(1, 1)
      else Augmentation(2, 2)
    }
  }

  private def childPermutations(previous: List[Component[VType]],
                                next: List[Component[VType]],
                                currentResult: List[List[Component[VType]]]): List[List[Component[VType]]] = next match {
    case (x: Component[VType])::xs => if (x.containsAugmentations((2, 1), (1, 0)) && !x.containsAugmentations((1, 1), (2, 2))) {
      val updatedComponent = x.removeAugmentation(2, 1)
      childPermutations(previous ++ List(x), xs, (previous ++ (updatedComponent +: next)) +: currentResult)

    } else if (x.containsAugmentations((2, 2), (2, 0)) && !x.containsAugmentation(1, 1)) {
      val updatedComponent = x.removeAugmentation(2, 2).removeAugmentation(2, 1)
      (previous ++ (x +: next)) +: childPermutations(previous ++ List(x), xs, (previous ++ (updatedComponent +: next)) +: currentResult)

    } else {
      childPermutations(previous ++ List(x), xs, currentResult)
    }
    case _ => List[List[Component[VType]]]()
  }

}