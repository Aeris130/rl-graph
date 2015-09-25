package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection.DartDirection
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}

/**
 * Represents a dummy-edge that isn't a part of the final drawing. Doesn't have any bends.
 *
 * @constructor Creates a new artificial dart.
 * @param from The start vertex in the beginning of the dart.
 * @param to The stop vertex at the end of the dart.
 * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
 *                   beginning of the next.
 * @param dir Which direction the dart points at in a drawing.
 */
class ArtificialDart[VType]
  (from: VertexWrapper[VType],
   to: VertexWrapper[VType],
   nextDegree: Int,
   dir: Option[DartDirection]) extends Dart[VType](from, to, nextDegree, 0, dir) {

  def this(from: VertexWrapper[VType], to: VertexWrapper[VType], nextDegree: Int) = this(from, to, nextDegree, None)

  override def newAngle(angle: Int): Dart[VType] = new ArtificialDart[VType](from, to, angle, direction)

  override def original: (VertexWrapper[VType], VertexWrapper[VType]) = (null, null)

  override def newDirection(newDirection: DartDirection): Dart[VType] = new ArtificialDart(from, to, nextDegree, Option(newDirection))

  override def findSimpleOriginal: Dart[VType] = throw new IllegalArgumentException("Artificial darts are not based on other darts.")

  override def isBasedOnArtificialDart: Boolean = true

  override def equals(other: Any): Boolean = other match {
    case d: ArtificialDart[VType] => d.from == from && d.to == to && d.nextDegree == nextDegree && direction == d.direction
    case _ => false
  }

  override def hashCode: Int = {
    from.## ^ to.## ^ nextDegree ^ direction.##
  }
}
