package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

/**
  * User-specified parameters for the pre-processing algorithm.
  *
  * A note on weights: Each of the three weights influences the final drawing relative to each other. As long as they
  * are set to equal values (regardless of what those values are) they're equal. Adjust the weights to see which
  * values your graphs benefits from the most.
  *
  * @param boundMultiplier The drawing will have its size increased by this factor before it starts moving vertices
  *                        around. Adjust this value to give the algorithm more/less space to work with.
  * @param annealingIterations The number of times the vertex set should have its vertices processed by the annealing algorithm.
  *                   Each iteration processes all n vertices in a drawing. Each temperature will run the iteration
  *                   process, see algorithm description.
  * @param cooling The cooling percentage to apply to the temperature, between 0 and 1 (exclusive)
  * @param costFunctions All cost functions that should be used for annealing step.
  */
case class Settings[S <: LayoutState[S]](boundMultiplier: Double,
                                         centerIterations: Int,
                                         annealingIterations: Int,
                                         cooling: Double,
                                         costFunctions: Vector[CostFunction[S]],
                                         initialState: S) {
  require(costFunctions.nonEmpty, "At least one cost function must be specified.")
  require(boundMultiplier >= 1, "Bound multiplier must be >= 1.")
  require(centerIterations >= 0, "Center iterations must be 0 or higher.")
  require(annealingIterations >= 0, "Annealing iterations must be 0 or higher.")

}

object Settings {

  def default[S <: LayoutState[S]](initialState: S, cfs: Vector[CostFunction[S]]) = Settings(
    boundMultiplier = 2,
    centerIterations = 2,
    annealingIterations = 30,
    cooling = 0.75,
    costFunctions = cfs,
    initialState = initialState
  )

}
