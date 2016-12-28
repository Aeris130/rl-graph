package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlcommon.stat.Statistics
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{CostFunction, DefaultState}

/**
  * Scores layout better the more evenly its vertices are distributed across the drawing.
  */
class DistributionScore(val weight: Double, neighborsToCheck: Int) extends Common with CostFunction[DefaultState] {

  /**
    * @param state The current state of the layout.
    * @param v     The latest vertex that was adjusted in the layout. Used for scoring algorithms that only wants
    *              to examine changes local to a vertex.
    * @return A value between 0 and 1 (inclusive). A better layout should receive a lower score.
    */
  override def score(state: DefaultState, v: Int): Double = {
    if (state.vertices == 1)
      return 0

    val vCoordinate = state.coordinate(v)
    //TODO Change to approximation method to optimize search
    val neighbors = state.kdTree.nearestNeighbor(neighborsToCheck, vCoordinate).filter(_ != vCoordinate) // Nearest neighbor search will also include v
    val distances = neighbors.map(_.distanceTo(vCoordinate))
    val longestDistance = diagonal(state)
    val deviationFromMean: Double = Statistics.stdDeviation(distances)

    /* The lowest possible variance is 0, the highest is the diagonal of the layout. This occurs if all vertices are
     * placed in a corner with no variance between each other, and a single vertex lies at the opposite corner.
     */
    Normalize(deviationFromMean, 0, longestDistance)
  }

}
