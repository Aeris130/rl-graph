package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{CostFunction, DefaultState}

/**
  * Scores a layout better the further away from its borders that its vertices lie.
  */
class BorderLinesScore(val weight: Double) extends Common with CostFunction[DefaultState] {

  /**
    * @param state The current state of the layout.
    * @return A value between 0 and 1 (inclusive). A better layout should receive a lower score.
    */
  override def score(state: DefaultState, v: Int): Double = {
    val vc = state.coordinate(v)
    def distance(p: Point, side: String) = side match {
      case "Left" => p.x //(0 - x)
      case "Right" => (state.size - 1) - p.x
      case "Bottom" => p.y
      case "Top" => (state.size - 1) - p.y
    }

    // 1 (worst score) if the rectangle is at the border, then successively better the further away from the border it is.
    def score(distance: Double) = if (distance <= 1) 1d else 1d / distance

    // This score will never be 0, as moving away from one border will move the rectangle towards another
    val totalScore = score(distance(vc, "Left")) + score(distance(vc, "Right")) + score(distance(vc, "Bottom")) + score(distance(vc, "Top"))
    Normalize(totalScore, 0, 4)
  }

}
