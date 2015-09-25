package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection.DartDirection

/**
 * Represents part of an edge that's been simplified by translating bends to individual darts.
 *
 * @constructor Creates a new simple dart.
 * @param from Wrapper for the start vertex.
 * @param to Wrapper for the stop vertex.
 * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
 *                   beginning of the next.
 * @param originalDart The dart that has been simplified.
 */
class SimpleDart[VType](from: VertexWrapper[VType],
                        to: VertexWrapper[VType],
                        nextDegree: Int,
                        val originalDart: Dart[VType],
                        direction: Option[DartDirection]
                        ) extends Dart[VType](from, to, nextDegree, 0, direction) {
  if (nextDegree < 1 || nextDegree > 4) throw new IllegalArgumentException("Vertex degree for " + this + "must be between 1 and 4 (currently " + nextDegree + ")")

  private val originalDartEdge: Dart[VType] = {
    val original = getOriginal(originalDart)

    if (!original.from.isDefined)
      throw new IllegalArgumentException("Original vertex " + original.from + " must be defined.")
    if (!original.to.isDefined)
      throw new IllegalArgumentException("Original vertex " + original.to + " must be defined.")

    original
  }

  /**
   * Creates a new simple dart with no set direction.
   * @param from Wrapper for the start vertex.
   * @param to Wrapper for the stop vertex.
   * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
   *                   beginning of the next.
   * @param originalDart The dart that has been simplified.
   * @return A new simple dart with no direction.
   */
  def this(from: VertexWrapper[VType], to: VertexWrapper[VType], nextDegree: Int, originalDart: Dart[VType]) =
    this(from, to, nextDegree, originalDart, None)

  override def newAngle(angle: Int): Dart[VType] = new SimpleDart[VType](from, to, angle, originalDartEdge, direction)

  override def newDirection(newDirection: DartDirection): Dart[VType] = new SimpleDart[VType](from, to, nextDegree, originalDartEdge, Option(newDirection))

  override def original: (VertexWrapper[VType], VertexWrapper[VType]) = (originalDartEdge.from, originalDartEdge.to)

  override def findSimpleOriginal: Dart[VType] = this

  override def isBasedOnArtificialDart: Boolean = originalDart.isBasedOnArtificialDart

  override def equals(other: Any): Boolean = other match {
    case d: SimpleDart[VType] => d.from == from && d.to == to && d.nextDegree == nextDegree && d.original == original && direction == d.direction
    case _ => false
  }

  override def hashCode: Int = {
    from.## ^ to.## ^ nextDegree ^ original.## ^ direction.##
  }

  private def getOriginal[V](d: Dart[V]): DefaultDart[V] = d match {
    case default: DefaultDart[V] => default
    case _ => throw new Error("Simple darts can only be instanced from Defaults.")
  }
}
