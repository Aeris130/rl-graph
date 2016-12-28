package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlcommon.stat.Statistics
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{CostFunction, DefaultState}
import net.cyndeline.rlgraph.util.GraphCommons

/**
  * Scores a layout better if a vertex v is more uniformly distanced from its neighbors.
  */
class NeighborDistScore(val weight: Double) extends Common with CostFunction[DefaultState] {

  /**
    * @param state The current state of the layout.
    * @param v     The latest vertex that was adjusted in the layout. Used for scoring algorithms that only wants
    *              to examine changes local to a vertex.
    * @return A value between 0 and 1 (inclusive). A better layout should receive a lower score.
    */
  override def score(state: DefaultState, v: Int): Double = {
    val neighbors = GraphCommons.outerNeighbors(v, state.graph)
    val vc = state.coordinate(v)
    val distances = neighbors.map(n => state.coordinate(n).distanceTo(vc))
    val deviation = Statistics.stdDeviation(distances)
    val longestDistance = diagonal(state)
    Normalize(deviation, 0, longestDistance)
  }

}
