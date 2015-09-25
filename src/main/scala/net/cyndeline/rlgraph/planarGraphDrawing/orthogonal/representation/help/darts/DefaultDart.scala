package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper, VertexWrapperFactory}

/**
 * A dart representing an unchanged edge, bends included.
 *
 * @constructor Creates a new default dart.
 * @param from Wrapper for the start vertex.
 * @param to Wrapper for the stop vertex.
 * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
 *                   beginning of the next.
 * @param bends The number of 90 degree bends inside the face this dart belongs to. 270 degree bends can
 *              be computed using the 90 degree bends belonging to the dart going in opposite direction of this one.
 * @param direction The direction this dart points at in a drawing.
 */
class DefaultDart[VType]
  (from: VertexWrapper[VType],
   to: VertexWrapper[VType],
   nextDegree: Int,
   bends: Int,
   direction: Option[DartDirection]) extends Dart[VType](from, to, nextDegree, bends, direction) {
  if (!from.isDefined)
    throw new IllegalArgumentException("Default darts cannot be instantiated using a dummy vertex as from-value")

  if (!to.isDefined)
    throw new IllegalArgumentException("Default darts cannot be instantiated using a dummy vertex as to-value")

  /**
   * Creates a new dart by wrapping a pair of vertices.
   * @param from An unwrapped start vertex.
   * @param to An unwrapped stop vertex.
   * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
   *                   beginning of the next.
   * @param bends The number of 90 degree bends inside the face this dart belongs to. 270 degree bends can
   *              be computed using the 90 degree bends belonging to the dart going in opposite direction of this one.
   * @param direction The direction this dart points at in a drawing.
   * @return A new default dart with its vertices wrapped.
   */
  def this(from: VType, to: VType, nextDegree: Int, bends: Int, direction: Option[DartDirection], wrapperFact: VertexWrapperFactory[VType])
    = this(wrapperFact.createVertexWrapper(from), wrapperFact.createVertexWrapper(to), nextDegree, bends, direction)

  /**
   * Creates a new dart with no set direction by wrapping a vertex pair.
   * @param from An unwrapped start vertex.
   * @param to An unwrapped stop vertex.
   * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
   *                   beginning of the next.
   * @param bends The number of 90 degree bends inside the face this dart belongs to. 270 degree bends can
   *              be computed using the 90 degree bends belonging to the dart going in opposite direction of this one.
   * @return A new default dart with no direction.
   */
  def this(from: VType, to: VType, nextDegree: Int, bends: Int, wrapperFact: VertexWrapperFactory[VType]) = this(from, to, nextDegree, bends, None, wrapperFact)

  /**
   * Creates a new dart from a wrapped vertex pair with no set direction.
   * @param from Wrapper for the start vertex.
   * @param to Wrapper for the stop vertex.
   * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
   *                   beginning of the next.
   * @param bends The number of 90 degree bends inside the face this dart belongs to. 270 degree bends can
   *              be computed using the 90 degree bends belonging to the dart going in opposite direction of this one.
   * @return A new default dart with no direction.
   */
  def this(from: VertexWrapper[VType], to: VertexWrapper[VType], nextDegree: Int, bends: Int) = this(from, to, nextDegree, bends, None)

  override def newAngle(angle: Int): Dart[VType] = new DefaultDart[VType](from, to, angle, bends, direction)

  override def newDirection(newDirection: DartDirection): Dart[VType] = new DefaultDart[VType](from, to, nextDegree, bends, Option(newDirection))

  override def original: (VertexWrapper[VType], VertexWrapper[VType]) = (from, to)

  override def findSimpleOriginal: Dart[VType] = this

  override def isBasedOnArtificialDart: Boolean = false

  override def equals(other: Any): Boolean = other match {
    case d: DefaultDart[VType] => d.from == from && d.to == to && d.nextDegree == nextDegree && d.bends == bends && direction == d.direction
    case _ => false
  }

  override def hashCode: Int = {
    from.## ^ to.## ^ nextDegree ^ bends ^ direction.##
  }
}
