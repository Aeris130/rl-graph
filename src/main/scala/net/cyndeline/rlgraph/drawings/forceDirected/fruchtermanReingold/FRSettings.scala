package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold

/**
  * Parameters used by the FR drawing algorithm.
  *
  * @param maxIterations The number of iterations the algorithm should stop at.
  * @param forceComputation User-supplied object controlling how far vertices moved when affected by attracting and
  *                         repulsing forces. Also controls temperature decrease.
  */
class FRSettings(val maxIterations: Int,
                 val forceComputation: Force) {

}

object FRSettings {
  private val optimalDistance = 20 // The optimal distance in coordinates between vertices in the drawing.
  private val constant = 2 // A constant scalar for repulsive forces when two vertices has rectangles that overlap. Experiment with this value to find visually pleasing results.

  def defaultSettings: FRSettings = new FRSettings(100, new SizeDisplacement(optimalDistance, constant))

  /**
    * @param maxIterations The number of iterations the algorithm should stop at.
    * @param optimalDistance Target distance between vertices. This value won't be strictly adhered to, but will factor
    *                        into the distance computations.
    * @param c A constant to scale the repulsive force by. Experiment to find the value that produces the best visual
    *          results.
    * @return Settings for the FR drawing algorithm.
    */
  def apply(maxIterations: Int, optimalDistance: Int, c: Int): FRSettings = new FRSettings(maxIterations, new SizeDisplacement(optimalDistance, c))
}
