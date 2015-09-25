package net.cyndeline.rlgraph.cartogram.rectangular.common

/**
 * X or Y.
 */
sealed trait Axis

case object X_Axis extends Axis
case object Y_Axis extends Axis