package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

import net.cyndeline.rlcommon.math.geom.Dimensions

/**
  * User-specified parameters for the pre-processing algorithm.
  *
  * A note on weights: Each of the three weights influences the final drawing relative to each other. As long as they
  * are set to equal values (regardless of what those values are) they're equal. Adjust the weights to see which
  * values your graphs benefits from the most.
  *
  * @param rectangles Maps every vertex against the dimensions of the rectangle it should be drawn as.
  * @param boundMultiplier The drawing will have its size increased by this factor before it starts moving vertices
  *                        around. Adjust this value to give the algorithm more/less space to work with.
  * @param iterations The number of times the vertex set should have its vertices processed by the annealing algorithm.
  *                   Each iteration processes all n vertices in a drawing. Each temperature will run the iteration
  *                   process, see algorithm description.
  * @param cooling The cooling percentage to apply to the temperature, between 0 and 1 (exclusive)
  * @param targetEdgeLength Optimal edge length in coordinates.
  * @param edgeWeight Specifies how heavily the length of the edges (relative to the target) affects the final drawing.
  * @param borderWeight Specifies how heavily te proximity to the drawings borders (the further away the vertices lie,
  *                     the better) affects the final drawing.
  * @param distributionWeight Specifies how heavily the vertex-distributions uniformity affects the final drawing.
  */
case class Settings(rectangles: Map[Int, Dimensions],
                    boundMultiplier: Double,
                    iterations: Int,
                    cooling: Double,
                    targetEdgeLength: Double,
                    edgeWeight: Double,
                    borderWeight: Double,
                    distributionWeight: Double) {
  require(boundMultiplier >= 1, "Bound multiplier must be >= 1.")

}

object Settings {

  def default = Settings(
    rectangles = Map(),
    boundMultiplier = 2,
    iterations = 30,
    cooling = 0.75,
    targetEdgeLength = 0,
    edgeWeight = 1,
    borderWeight = 1,
    distributionWeight = 1
  )

  def defaultWithRectangles(r: Map[Int, Dimensions]) = Settings(
    rectangles = r,
    boundMultiplier = 2,
    iterations = 30,
    cooling = 0.75,
    targetEdgeLength = 0,
    edgeWeight = 1,
    borderWeight = 1,
    distributionWeight = 1
  )

}
