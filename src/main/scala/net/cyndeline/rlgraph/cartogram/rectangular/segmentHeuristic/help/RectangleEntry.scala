package net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help

import net.cyndeline.rlgraph.cartogram.rectangular.common.{Bottom, Left, Right, SegmentsOfRectangle, Side, Top}

/**
 * @param segments The segments bounding the rectangle.
 * @param targetSize Optimal user-specified size.
 * @param side Which side of the owner segment the rectangle is on.
 * @param maxAspectRatio Max(h/w, w/h) allowed for this rectangle.
 * @param notOptimized True if this rectangle shouldn't care about error or aspect ratio.
 */
class RectangleEntry(val segments: SegmentsOfRectangle, val targetSize: Int, val side: Side, val maxAspectRatio: Double, val notOptimized: Boolean) {

  /**
   * @param direction The direction (from its pov) that the segment is moving in. If it is the same direction
   *                  that the rectangle lies in (from the segments pov), the rectangle shrinks. Otherwise it grows.
   * @return The change in cartographic error that occurs when moving a segment. Negative if the error after movement
   *         is less.
   */
  def canMove(direction: Side): MovementChange =  {
    val currentError = cartographicError(Geometry.area(segments))

    val whMod = widthHeightChange(direction)
    val newWidth = Geometry.width(segments) + whMod._1
    val newHeight = Geometry.height(segments) + whMod._2
    val newError = cartographicError(newWidth * newHeight)
    val oldRatio = Math.max(Geometry.width(segments).toDouble / Geometry.height(segments), Geometry.height(segments).toDouble / Geometry.width(segments))
    val newRatio = Math.max(newWidth.toDouble / newHeight, newHeight.toDouble / newWidth)

    if (notOptimized)
      MovementChange(1, 1, 0, 0)
    else
      MovementChange(newRatio, oldRatio, newError, currentError)
  }

  private def cartographicError(area: Int): Double = Math.abs(area - targetSize).toDouble / targetSize

  /** @return Width, height modification from moving the owner segment. */
  private def widthHeightChange(direction: Side): (Int, Int) = {
    if (direction == side) { // Shrinks
      side match {
        case Left | Right => (- 1, 0)
        case Top | Bottom => (0, -1)
      }

    } else { // Grows
      side match {
        case Left | Right => (1, 0)
        case Top | Bottom => (0, 1)
      }
    }
  }

  override def equals(other: Any): Boolean = other match {
    case re: RectangleEntry => segments == re.segments && targetSize == re.targetSize && side == re.side && notOptimized == re.notOptimized
    case _ => false
  }
  override val hashCode: Int = segments.## ^ targetSize ^ side.## ^ notOptimized.##
}

case class MovementChange(newRatio: Double, oldRatio: Double, newError: Double, oldError: Double)