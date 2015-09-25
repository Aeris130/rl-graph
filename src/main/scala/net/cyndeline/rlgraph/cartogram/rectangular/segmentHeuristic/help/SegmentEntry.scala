package net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help

import net.cyndeline.rlgraph.cartogram.rectangular.common._

/**
 * The data needed to decide if moving a segment along its axis leads to a lower error.
 *
 * @param segment The segment to move.
 * @param greaterThan Other segments along the same axis that constrains this segment from above. Includes
 *                    planarity constraints.
 * @param lessThan Other segments along the same axis that constrains this segment from below. Includes
 *                    planarity constraints.
 * @param rectangles Data of rectangles bounded by this segment.
 */
case class SegmentEntry(segment: Segment,
                       greaterThan: Vector[Constraint],
                       lessThan: Vector[Constraint],
                       rectangles: Vector[RectangleEntry],
                       axis: Axis)