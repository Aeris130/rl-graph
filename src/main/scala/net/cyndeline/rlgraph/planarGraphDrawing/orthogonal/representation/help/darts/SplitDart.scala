package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection.DartDirection
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}

/**
 * A dart that has been made by splitting another dart into two.
 *
 * @constructor Creates a new split dart.
 * @param from Wrapper for the start vertex.
 * @param to Wrapper for the stop vertex.
 * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
 *                   beginning of the next.
 * @param originalDart The dart that has been simplified.
 * @param direction The direction this dart points at in a drawing.
 */
class SplitDart[VType]
  (from: VertexWrapper[VType],
   to: VertexWrapper[VType],
   nextDegree: Int,
   val originalDart: Dart[VType],
   direction: Option[DartDirection] = None) extends Dart[VType](from, to, nextDegree, 0, direction) {
  private val originalVertexPair: (VertexWrapper[VType], VertexWrapper[VType]) = getOriginal(originalDart)
  private val dartOfOriginal: Dart[VType] = originalDart

  override def newAngle(angle: Int): Dart[VType] = new SplitDart[VType](from, to, angle, dartOfOriginal, direction)

  override def original: (VertexWrapper[VType], VertexWrapper[VType]) = originalVertexPair

  override def newDirection(newDirection: DartDirection): Dart[VType] = new SplitDart[VType](from, to, nextDegree, dartOfOriginal, Option(newDirection))

  override def findSimpleOriginal: Dart[VType] = originalDart.findSimpleOriginal

  override def isBasedOnArtificialDart: Boolean = originalDart.isBasedOnArtificialDart

  override def equals(other: Any): Boolean = other match {
    case d: SplitDart[VType] => d.from == from && d.to == to && d.nextDegree == nextDegree && d.original == original && direction == d.direction
    case _ => false
  }

  override def hashCode: Int = {
    from.## ^ to.## ^ nextDegree ^ original.## ^ direction.##
  }

  private def getOriginal[V](d: Dart[V]): (VertexWrapper[V], VertexWrapper[V]) = d match {
    case default: DefaultDart[V] => (default.from, default.to)
    case simple: SimpleDart[V] => simple.original
    case artificial: ArtificialDart[V] => (artificial.from, artificial.to)
    case split: SplitDart[V] => (split.from, split.to)
    case _ => throw new IllegalArgumentException("Unknown dart class: " + d.getClass)
  }

}
