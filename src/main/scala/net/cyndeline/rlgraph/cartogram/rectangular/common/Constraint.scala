package net.cyndeline.rlgraph.cartogram.rectangular.common

/**
 * Marks that a segments value should not be greater or les than another segment.
 *
 * @param relation Weather 'other should be greater or less than the first segment.
 */
case class Constraint(segment: Segment, other: Segment, relation: SegmentRelation)

sealed trait SegmentRelation

case object LessThan extends SegmentRelation
case object GreaterThan extends SegmentRelation
