package net.cyndeline.rlgraph.cartogram.rectangular.common

/**
 * Segment constraints keeping the segment a < b if a is lower than b in a rectangle.
 */
class PlanarConstraints(lower: Segment, higher: Segment) {

  /** Planar constraint for the lower segment. */
  val lowerConstraint: Constraint = Constraint(lower, higher, GreaterThan)

  /** Planar constraint for the higher segment. */
  val higherConstraint: Constraint = Constraint(higher, lower, LessThan)
}
