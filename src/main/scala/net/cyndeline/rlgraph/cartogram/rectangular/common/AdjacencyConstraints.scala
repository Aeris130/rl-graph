package net.cyndeline.rlgraph.cartogram.rectangular.common

import scala.collection.mutable.ListBuffer

/**
 * Computes adjacency constraints for a single segment belonging to a segment map, based on the adjacencies of
 * rectangular areas in a rectangular layout and their original graph representation. These constraints should only
 * be computed once, before any optimization has taken place.
 *
 * This example uses vertical segment constraints for the x-axis. A similar argument follows for the y-axis. It
 * also only deals with adjacent segments on one side of the current rectangle, but it should be repeated
 * for segments on the other side as well.
 *
 * Constraints are computed as follows: For every rectangular area R, the adjacent rectangles AR appearing on one side
 * of R are computed, and the rectangle with the lowest bounding segment on the x axis (i.e the leftmost area), and
 * the one with the highest (the rightmost) are computed (these can be the same rectangle if AR has size 1).
 *
 * The right segment of AR(leftmost) cannot go below the lower bound of R. The left segment of AR(rightmost) cannot
 * go above the upper bound of R. As long as these constraints are fulfilled, any rectangles between left- and rightmost
 * can never leave R's adjacency regardless of weather they were adjacent in the original graph or not.
 *
 * @param area The area to compute constraints for.
 * @param areaNeighbors The segments of every adjacent rectangle to the input area in the original layout.
 * @param axis X_Axis if the constraints should be generated for vertical segments based on their left/right
 *             relationships, Y_Axis for horizontal segments based on their top/bottom relationships.
 */
class AdjacencyConstraints(area: SegmentsOfRectangle, areaNeighbors: Vector[SegmentsOfRectangle], axis: Axis) {

  /** Every constraint related to the input area and its adjacent neighbors. */
  val constraints: Vector[Constraint] = lessThan ++ greaterThan

  /** The highest segment of the input area, tupled with the adjacent segments that must be less than it. */
  private def lessThan: Vector[Constraint] = {
    val above = areasAbove
    val below = areasBelow
    val result = new ListBuffer[Constraint]()

    if (!above.isEmpty) {
      val highestAbove = highestArea(above)

      // no need to add constraint if the highest upper adjacency shares the highest boundary with the current area
      if (upperSegment(area) != upperSegment(highestAbove)) {
        result += Constraint(upperSegment(area), lowerSegment(highestAbove), LessThan)
      }

    }

    if (!below.isEmpty) {
      val highestBelow = highestArea(below)

      // no need to add constraint if the highest lower adjacency shares the highest boundary with the current area
      if (upperSegment(area) != upperSegment(highestBelow)) {
        result += Constraint(upperSegment(area), lowerSegment(highestBelow), LessThan)
      }
    }

    result.toVector
  }

  /** The lowest segment of the input area, tupled with the adjacent segments that must be greater than it. */
  private def greaterThan: Vector[Constraint] = {
    val above = areasAbove
    val below = areasBelow
    val result = new ListBuffer[Constraint]()

    if (!above.isEmpty) {
      val lowestAbove = lowestArea(above)

      // no need to add constraint if the lowest upper adjacency shares the lowest boundary with the current area
      if (lowerSegment(area) != lowerSegment(lowestAbove)) {
        result += Constraint(lowerSegment(area), upperSegment(lowestAbove), GreaterThan)
      }
    }

    if (!below.isEmpty) {
      val lowestBelow = lowestArea(below)

      // no need to add constraint if the lowest bottom adjacency shares the lowest boundary with the current area
      if (lowerSegment(area) != lowerSegment(lowestBelow))
      result += Constraint(lowerSegment(area), upperSegment(lowestBelow), GreaterThan)
    }

    result.toVector
  }

  private def lowerSegment(s: SegmentsOfRectangle): Segment = axis match {
    case X_Axis => s.left
    case Y_Axis => s.top
  }

  private def upperSegment(s: SegmentsOfRectangle): Segment = axis match {
    case X_Axis => s.right
    case Y_Axis => s.bottom
  }

  private def highestArea(areas: Vector[SegmentsOfRectangle]): SegmentsOfRectangle = areas.maxBy(upperSegment(_).value)
  private def lowestArea(areas: Vector[SegmentsOfRectangle]): SegmentsOfRectangle = areas.minBy(upperSegment(_).value)

  private def areasAbove: Vector[SegmentsOfRectangle] = axis match {
    case X_Axis => areaNeighbors.filter(s => s.bottom.value == area.top.value)
    case Y_Axis => areaNeighbors.filter(s => s.left.value == area.right.value)
  }

  private def areasBelow: Vector[SegmentsOfRectangle] = axis match {
    case X_Axis => areaNeighbors.filter(s => s.top.value == area.bottom.value)
    case Y_Axis => areaNeighbors.filter(s => s.right.value == area.left.value)
  }
}
