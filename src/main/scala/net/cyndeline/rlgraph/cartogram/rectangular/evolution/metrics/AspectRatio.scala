package net.cyndeline.rlgraph.cartogram.rectangular.evolution.metrics

import net.cyndeline.rlcommon.util.Geom
import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.rlgraph.cartogram.rectangular.evolution.FitnessScore
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Computes the average error in aspect ratio. The error is computed using the ratio (maximal value of weight/height and
 * height/weight), as the optimal ratio is 1:1. However, once the ratio of an area exceeds a certain threshold, the
 * error no longer increases. This is because from a visual standpoint, the difference between a 1:12 and a 1:14 ratio
 * is much less than between 1:1 and 1:3.
 *
 * @param weight Value to multiply the final result by.
 */
class AspectRatio(weight: Double, ratioErrorLimit: Int) extends FitnessScore {

  def computeScore[V <: MapArea, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E]): Double = {
    val sum: Double = layout.allAreas
      .map(a => {
      val h = Geom.height(a.startX, a.stopX).toDouble
      val w = Geom.width(a.startY, a.stopY).toDouble
      Math.min(ratioErrorLimit, Math.max(h/w, w/h)) - 1.0 // A 1:1 ratio gives score 0.
    }).sum

    (sum / layout.allAreas.size) * weight
  }
}
