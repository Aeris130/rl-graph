package net.cyndeline.rlgraph.cartogram.rectangular.evolution

import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Computes a single fitness metric for a rectangular dual. The score must be higher the more fit a layout candidate is.
 */
trait FitnessScore {

  /**
   * @param layout Rectangular dual to compute fitness score for.
   * @return The score for the layout. Should be 0 if the solution matches the specification perfectly, otherwise
   *         a higher error should result in a higher score.
   */
  def computeScore[V <: MapArea, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E]): Double

}
