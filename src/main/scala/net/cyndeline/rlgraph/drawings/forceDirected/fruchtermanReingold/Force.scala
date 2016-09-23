package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold

import net.cyndeline.rlcommon.math.geom.{DPoint, Point, RPoint}
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid.GridRectangle
import spire.math.Rational

/**
  * Provides methods needed to compute the forces affecting vertices during the FR algorithm.
  */
trait Force {

  /**
    * @param a The vertex whose displacement should be computed.
    * @param b A neighbor of 'a.
    * @return The displacement that should occur to vertex 'a, should be a positive value.
    */
  def computeAttractingForce(a: GridRectangle, b: GridRectangle): Rational

  /**
    * @return The displacement that should occur between two adjacent but non-connected vertices. Should be a positive
    *         value.
    */
  def computeRepulsingForce(a: GridRectangle, b: GridRectangle): Rational

  /**
    * Computes the temperature that should be used when computing displacement. This method will be called once after
    * each iteration in order to generate the temperature for the next.
    * @param temperature The current temperature.
    * @param currentIteration The number of iterations that has completed on this drawing. 0 on the first call.
    * @param maxIterations The maximum amount of iteration to perform before the algorithm is considered finished.
    * @return The temperature after it has been cooled off.
    */
  def cool(temperature: Rational, currentIteration: Int, maxIterations: Int): Rational

  /**
    * Computes how two rectangles whose centers overlap should have their center coordinates computed as. This will
    * be used in order to give overlapping rectangles a direction to be repulsed in.
    * @param v A vertex rectangle.
    * @param n And adjacent neighbour that should apply a repulsing force onto v.
    * @return The center coordinates of v and n as they should be used when computing repulsing forces.
    */
  def overlappingCenter(v: GridRectangle, n: GridRectangle): (RPoint, RPoint)

}
