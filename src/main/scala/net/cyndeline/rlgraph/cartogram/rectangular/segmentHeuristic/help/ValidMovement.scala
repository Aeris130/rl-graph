package net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help

import net.cyndeline.rlgraph.cartogram.rectangular.common._
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help.MoveDir._

import scala.collection.mutable.ArrayBuffer

/**
 * Checks constraints to determine which direction a segment is allowed to move into. Only segments within 1 coordinate
 * of the moving segment is considered to be blocking.
 */
class ValidMovement {

  /**
   * @param segment Segment entry to move.
   * @return Directions the entry is allowed to move into. Up = Right/Bottom, Down = Left/Top.
   */
  def validMovementDirections(segment: SegmentEntry): Vector[MoveDir] = {
    val l = new ArrayBuffer[MoveDir]()
    if (movementAllowed(segment.segment, segment.greaterThan))
      l += Up

    if (movementAllowed(segment.segment, segment.lessThan) && segment.segment.value > 0)
      l += Down

    l.toVector
  }

  /**
   * @param segment The segment to move.
   * @param constraints Constraints potentially blocking the segment.
   * @return True if no constraint blocks movement, otherwise false.
   */
  private def movementAllowed(segment: Segment, constraints: Vector[Constraint]): Boolean = {
    val cs = constraints.toIterator
    while (cs.hasNext) {
      val c = cs.next()

      c.relation match {
        case GreaterThan => if (c.other.value == segment.value + 1) return false
        case LessThan => if (c.other.value == segment.value - 1) return false
      }
    }

    true
  }
}
