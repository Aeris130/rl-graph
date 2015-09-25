package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.Dart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.{ArtificialDart, DefaultDart, SimpleDart, SplitDart}

/**
 * Removes every dart from a collection of darts if that dart is either artificial, or a split dart that was
 * created by splitting an artificial dart. Can also be used to retrieve darts as they were before they were
 * split using artificial vertices.
 *
 * @constructor Creates a new artificial dart remover.
 */
class ArtificialRemover {

  /**
   * Original-dart values are followed recursively for every dart until the algorithm either finds a default dart
   * (dart is kept), or an artificial dart (dart is removed).
   *
   * @param darts All darts to either remove or keep.
   * @return a set of all darts that either are default darts or created from a default dart
   *         (by splitting or simplifying).
   */
  def removeArtificialDarts[VType](darts: Set[Dart[VType]]): Set[Dart[VType]] = for {
      dart <- darts
      if !isArtificial(dart)
    } yield dart

  /**
   * Removes artificial vertices from a set of darts by replacing a dart beginning and/or ending in an artificial
   * vertex with the dart it originated from before it was split. This algorithm preserves simplification, as such,
   * if a simple dart is found as origin of a split dart, the dart is returned rather than having its original (the
   * default dart) retrieved instead. If a default dart is found before a simple dart is, it means the default dart
   * had no bends to begin with, and can be returned.
   *
   * @param darts All darts, some of which may contain artificial vertices.
   * @return a collection of the specified darts, with split darts being replaced by the darts they were originally
   *         split from. If a dart was split from a previously split dart, the chain will be followed all the way down
   *         to the original.
   */
  def removeArtificialVertices[VType](darts: Set[Dart[VType]]): Set[Dart[VType]] = for {
    dart <- darts
  } yield findSimpleOriginal(dart)

  /**
   * A dart is artificial if it itself is, or if a dart it is based on is.
   */
  private def isArtificial[VType](dart: Dart[VType]): Boolean = dart match {
    case artificial: ArtificialDart[VType] => true
    case default: DefaultDart[VType] => false
    case simple: SimpleDart[VType] => isArtificial(simple.originalDart)
    case split: SplitDart[VType] => isArtificial(split.originalDart)
    case _ => throw new IllegalArgumentException("Unspecified dart class: " + dart.getClass)
  }

  /**
   * Finds the first original instance of a simple dart.
   */
  private def findSimpleOriginal[VType](dart: Dart[VType]): Dart[VType] = dart match {
    case simple: SimpleDart[VType] => simple
    case default: DefaultDart[VType] => default // Only gets here if the dart never were simplified, i.e were already simple
    case split: SplitDart[VType] => findSimpleOriginal(split.originalDart)
    case artificial: ArtificialDart[VType] => throw new IllegalArgumentException("No artificial darts should remain when clearing artificial vertices.")
    case _ => throw new IllegalArgumentException("Unspecified dart class: " + dart.getClass)
  }
}
