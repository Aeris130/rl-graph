package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{CostFunction, DefaultState}
import net.cyndeline.rlgraph.util.GraphCommons

/**
  * Rates a vertex movement better the closer its neighboring edges are to the target length.
  */
class EdgeLengthScore(val weight: Double, targetLength: Int) extends Common with CostFunction[DefaultState] {

  /**
    * @param state The current state of the layout.
    * @return A value between 0 and 1 (inclusive). A better layout should receive a lower score.
    */
  override def score(state: DefaultState, v: Int): Double = {
    if (state.vertices == 1)
      return 0

    val longestEdgeValue = diagonal(state)
    val maxDivergence = {
      val a = longestEdgeValue - targetLength
      val b = longestEdgeValue - a
      Math.max(a, b)
    }
    val vCoordinate = state.coordinate(v)
    val neighborRectangles = GraphCommons.outerNeighbors(v, state.graph).map(state.coordinate)

    // Give every edge a score equal to its divergence^2, to prioritise modifying edges with large divergence-
    val distances = neighborRectangles.map(_.distanceTo(vCoordinate)).map(distance => {
      Math.pow(distance - targetLength, 2)
    })
    val maxSum = Math.pow(maxDivergence, 2) * distances.size
    Normalize(distances.sum, 0, maxSum)
  }

}
