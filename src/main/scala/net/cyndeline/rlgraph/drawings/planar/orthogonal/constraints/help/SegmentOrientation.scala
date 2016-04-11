package net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help

/**
 * Direction a segment of vertices is aligned in.
 */
object SegmentOrientation extends Enumeration {
  type SegmentOrientation = Value
  val Horizontal = Value("Horizontal")
  val Vertical = Value("Vertical")
}
