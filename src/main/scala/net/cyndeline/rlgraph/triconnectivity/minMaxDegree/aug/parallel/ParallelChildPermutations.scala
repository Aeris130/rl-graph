package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.parallel

import scala.collection.mutable.ArrayBuffer

/**
 * Computes the permutations of children in the parallel case: For each child with both augmentations 2,1 and 1,0,
 * replace 2,1 with 1,0. If not present, do the same with 2,2 and 2,0.
 *
 * @param kSet A set collection where each set only contains singles.
 */
class ParallelChildPermutations[VType](kSet: KSets[VType]) {
  val childPermutations: Vector[KSets[VType]] = allPermutations

  /**
   * Due to the way the K-sets are filled, it is possible for a component to contain (2,1) and still not end up in
   * k3 due to k2 getting it first. Because of this, all singles are added to a single list, and checked for both
   * cases.
   */
  private def allPermutations: Vector[KSets[VType]] = {
    val newSets = new ArrayBuffer[KSets[VType]]()
    val allSingles = kSet.k3 ++ kSet.k2
    for (single <- allSingles) {
      single match {
        case k3: K3Single[VType] => if (k3.component.containsAugmentations((2, 1), (1, 0))) {
          newSets += kSet.removeFrom(3, k3).addTo(4, K4Single(k3.component))
        } else if (k3.component.containsAugmentations((2, 2), (2, 0))) {
          newSets += kSet.removeFrom(3, k3).addTo(5, K5Single(k3.component))
        }
        case k2: K2Single[VType] => if (k2.component.containsAugmentations((2, 1), (1, 0))) {
          newSets += kSet.removeFrom(2, k2).addTo(4, K4Single(k2.component))
        } else if (k2.component.containsAugmentations((2, 2), (2, 0))) {
          newSets += kSet.removeFrom(2, k2).addTo(5, K5Single(k2.component))
        }
        case _ => throw new Error("Cannot process KSet collection after k2 and k3 has had its singles collapsed.")
      }
    }

    newSets.toVector
  }
}
