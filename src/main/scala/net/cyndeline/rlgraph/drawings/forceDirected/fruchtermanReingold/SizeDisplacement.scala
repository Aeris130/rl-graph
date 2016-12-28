package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold

import net.cyndeline.rlcommon.math.geom.{DPoint, Point}
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid.GridRectangle

/**
  * Computes forces that positions vertices relative to their size, preventing overlap of their rectangles and
  * minimizes the space wasted due to some rectangles being larger than others. The basic algorithm is lifted from the
  * article "Generating Customized Layouts" by Xiaobo Wang and Isao Miyamoto. A slight modification occurs when
  * computing repulsing forces: Overlap is computed using the vertices rectangles rather than circles based on a radius
  * that encloses said rectangle.
  *
  * @param optimalDistance The user-specified optimal distance between two vertices connected by an edge.
  * @param constant A constant that scales repulsing forces. No optimal value exists, the user should experiment
  *                 with various values to see how they affect the final drawing.
  */
class SizeDisplacement(optimalDistance: Double, constant: Double) extends Force {
  private val epsilon = 1e-3

  /**
    * @param a The vertex whose displacement should be computed.
    * @param b A neighbor of 'a.
    * @return The displacement that should occur to vertex 'a, should be a positive value.
    */
  override def computeAttractingForce(a: GridRectangle, b: GridRectangle): Double = {
    val aRadius = a.diagonal / 2
    val bRadius = b.diagonal / 2
    val distanceAB = a.center distanceTo b.center
    val distanceBetweenCircles = distanceAB - aRadius - bRadius

    /* Note that rectangle overlap cannot be used here since their circles may have distance 0 even if the rectangles
     * doesn't intersect.
     */
    if (distanceBetweenCircles <= 0) { // Circles overlap
      0

    } else {
      Math.pow(distanceBetweenCircles, 2) / (optimalDistance + aRadius)
    }
  }

  /**
    * @return The displacement that should occur between two adjacent but non-connected vertices. Should be a positive
    *         value.
    */
  override def computeRepulsingForce(a: GridRectangle, b: GridRectangle): Double = {
    if (a overlaps b) {
      val centers = if (a.center == b.center) overlappingCenter(a, b) else (a.center, b.center)

      // Use epsilon in can both rectangles have the same center coordinate
      constant * (Math.pow(optimalDistance, 2) / centers._1.distanceTo(centers._2).max(epsilon))

    } else {
      Math.pow(optimalDistance, 2) / a.center.distanceTo(b.center) // Denominator can't be 0 since there's no overlap
    }
  }

  /**
    * Computes the temperature that should be used when computing displacement.
    * @param temperature The current temperature.
    * @param currentIteration The number of iterations that has completed on this drawing.
    * @param maxIterations The maximum amount of iteration to perform before the algorithm is considered finished.
    * @return The temperature after it has been cooled off.
    */
  def cool(temperature: Double, currentIteration: Int, maxIterations: Int): Double = temperature * (1.0 - (currentIteration / maxIterations.toDouble))

  /**
    * Computes how two rectangles whose centers overlap should have their center coordinates computed as. This will
    * be used in order to give overlapping rectangles a direction to be repulsed in.
    * @param v A vertex rectangle.
    * @param n And adjacent neighbour that should apply a repulsing force onto v.
    * @return The center coordinates of v and n as they should be used when computing repulsing forces.
    */
  def overlappingCenter(v: GridRectangle, n: GridRectangle): (Point, Point) = {
    val vc = if (v.id > n.id) v.center else v.center + 1
    val nc = if (v.id > n.id) n.center + 1 else n.center
    (vc, nc)
  }

}
