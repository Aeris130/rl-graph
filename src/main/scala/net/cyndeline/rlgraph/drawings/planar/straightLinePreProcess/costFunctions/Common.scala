package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions

import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.DefaultState

/**
  * Common methods used by cost functions.
  */
class Common {
  final def diagonal(state: DefaultState): Double = state.size * Math.sqrt(2)
}
