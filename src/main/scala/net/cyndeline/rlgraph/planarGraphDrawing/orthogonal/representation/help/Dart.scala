package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._

/**
 * Contains base data for edges that has been translated into darts.
 *
 * @param from An unwrapped start vertex.
 * @param to An unwrapped stop vertex.
 * @param nextDegree The sum of 90 degree angles that exist between the end of tis dart and the
 *                   beginning of the next.
 * @param bends The number of 90 degree bends inside the face this dart belongs to. 270 degree bends can
 *              be computed using the 90 degree bends belonging to the dart going in opposite direction of this one.
 * @param direction The direction this dart points at in a drawing.
 * @tparam VType Type vertex represented by the dart.
 */
abstract class Dart[VType](val from: VertexWrapper[VType],
                           val to: VertexWrapper[VType],
                           val nextDegree: Int,
                           val bends: Int,
                           val direction: Option[DartDirection]) {
  if (nextDegree < 1 || nextDegree > 4) throw new IllegalArgumentException("Vertex degree for " + this + "must be between 1 and 4 (currently " + nextDegree + ")")

  def original: (VertexWrapper[VType], VertexWrapper[VType])

  def newAngle(angle: Int): Dart[VType]

  def newDirection(newDirection: DartDirection): Dart[VType]

  def findSimpleOriginal: Dart[VType]

  /**
   * Checks if a dart was at any time based on an artificial dart.
   * @return False if an original dart down the chain is artificial, otherwise false.
   */
  def isBasedOnArtificialDart: Boolean

  override def toString: String = "[" + from + "->" + to + "|nxt:" + nextDegree +
    (if (bends > 0) " |bend:" + bends else "") +
    (if (direction.isDefined) "|Dir:" + direction.get else "") + "]"
}
