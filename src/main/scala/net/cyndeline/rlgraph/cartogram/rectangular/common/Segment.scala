package net.cyndeline.rlgraph.cartogram.rectangular.common

/**
 * A value along some axis in a rectangular dual. May serve as start or stop coordinate for multiple rectangles if
 * they share a side, or has one of their sides lining up.
 *
 * @param variableIndex The index of this segment in the linear programming variable array.
 * @param axis The axis that the segment value bounds a rectangle on.
 */
class Segment(val variableIndex: Int, initialSegmentValue: Int, val axis: Axis) {

  /** The current segment value (x if the segment represents a boundary on the x axis, or vice versa with y). */
  var value: Int = initialSegmentValue

  override def equals(other: Any): Boolean = other match {
    case s: Segment => variableIndex == s.variableIndex && axis == s.axis
    case _ => false
  }

  override val hashCode: Int = variableIndex ^ axis.##

  override def toString: String = "Segment(" + variableIndex + ", value " + value + ", axis: " + axis + ")"
}

object Segment {
  def apply(index: Int, initialValue: Int, axis: Axis) = new Segment(index, initialValue, axis)
}