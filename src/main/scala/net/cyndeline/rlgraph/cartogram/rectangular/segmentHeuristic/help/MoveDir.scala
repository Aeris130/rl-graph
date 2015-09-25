package net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help

/**
 * Moving down (left, top) or up (right, bottom).
 */
object MoveDir extends Enumeration {
  type MoveDir = Value
  val Up, Down = Value
}