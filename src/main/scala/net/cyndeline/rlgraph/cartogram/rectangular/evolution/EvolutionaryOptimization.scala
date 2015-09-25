package net.cyndeline.rlgraph.cartogram.rectangular.evolution

import net.cyndeline.rlcommon.util.ProbabilityCollection
import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.SegmentHeuristic
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.{RELDualAlgorithm, RectangularLayout}
import net.cyndeline.rlgraph.regularEdgeLabeling.EdgeLabeling
import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.{AlternatingFourCycles, FourCycle}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.runtime.universe.TypeTag
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge

/**
 * An implementation of the optimization heuristic described by B.Speckmann et. al in "Evolution Strategies for
 * Optimizing Rectangular Cartograms". The goal of this algorithm is to explore the search-space of solutions
 * for rectangular duals of a graph, by examining valid regular edge labelings of it and selecting the one that
 * yields the best drawing.
 *
 * The algorithm begins by creating an initial set of solutions from an already constructed REL. This is done by
 * taking the minimal REL (in which every alternating 4-cycle is left-alternating) and flipping N left-cycles, storing
 * the solution after each flip. N is computed as |D * (0.5 + (r / 8))|, where D is the number of alternating 4-cycles
 * in the REL, and r is a random normal-distributed number. Once the initial population is created, the algorithm loops
 * a number of times then stops and selects the best solution found.
 *
 * Algorithm description:
 *
 *  1. Compute the fitness scores for every solution in the population.
 *  1. Keep the top 4% of all solutions as-is, based on which ones that received the better score.
 *  1. Sort the remaining solutions based on their scores, and assign each solution a probability equal to 0.9 raised
 *  to the power of i, where i is the position in the sorted list of solutions (i = 1 for the best solution, i = n for
 *  the worst, given n solutions).
 *  1. Fill the remaining slots in the population using the following rank selection for each slot:
 *   1. Randomly select an individual from the set of ranked solutions. The probability that a solution is selected
 *   is equal to the probability computed from its rank. This means that more fit solutions may end up selected for
 *   multiple slots.
 *   1. Given a probability of 0.05 (i.e a 5% chance), flip (D * r)/6 random cycles. D is the total number of
 *   alternating 4-cycles, and r is a random number with normal distribution. If r is positive, only flip among
 *   left-alternating cycles (meaning the solutions comes closer to the maximal REL). If r is negative, only flip
 *   among right-alternating cycles.
 *   1. Given a probability of 0.9, flip a single random alternating (in either direction) 4-cycle.
 *
 * The algorithm runs a number of iterations specified by the user, then selects the solution with the best fitness
 * score found so far.
 *
 * @param iterations The number of times the algorithm should loop before selecting the best solution.
 * @param fitnessScores User supplied classes computing fitness scores. These classes should assign a score of 0 for
 *                      a perfect solution, with increasingly higher score for solutions with higher errors.
 * @param heuristic Used to optimize the layouts computed by the algorithm, to better represent how the final drawing
 *                  will look when applying fitness scores.
 * @param exceptions Areas that shouldn't be optimized by the heuristic.
 */
class EvolutionaryOptimization[V <: MapArea : TypeTag, E[X] <: UnDiEdge[X]](iterations: Int,
                                                                            fitnessScores: Vector[FitnessScore],
                                                                            heuristic: SegmentHeuristic[V, E],
                                                                            exceptions: Set[V]) {

  /**
   * @param initialREL A regular edge labeling to start the algorithm from. The only requirement is that it produces
   *                   valid duals.
   * @param algorithm The algorithm object that was created along with the initial REL. Will be used to produce layouts
   *                  from labelings derived from the initial one.
   * @return The rectangular dual that was produced by the best REL, optimized by the submitted heuristic.
   */
  def optimizeLayout(initialREL: EdgeLabeling[RVertex[V]], algorithm: RELDualAlgorithm[V, E]): RectangularLayout[V, E] = {
    val cycles = AlternatingFourCycles(initialREL)
    val minimalCycles = minimalEdgLabeling(cycles)

    if (minimalCycles.isEmpty)
      return algorithm.computeLayout(initialREL)

    val random = new Random(12345) // Doesn't matter which seed it is
    var population = createInitialPopulation(minimalCycles, random)
    val nrOfTopSolutionsToKeep = Math.ceil(population.size * 0.04).toInt

    var i = 0

    while (i < iterations) {
      val solutionsWithScore = addFitnessScores(population, algorithm)
      val top = solutionsWithScore.dropRight(solutionsWithScore.size - nrOfTopSolutionsToKeep)
      val bottom = solutionsWithScore.drop(nrOfTopSolutionsToKeep)
      val nextBottomPop = fillRemainingPopulationSlots(bottom.map(_._1), random)
      // Apply mutations to the suboptimal solution set
        .map(s => applyDrasticMutation(s, random))
        .map(s => applyMinorMutation(s, random))

      population = top.map(_._1) ++ nextBottomPop
      i += 1
    }

    val bestSolution = addFitnessScores(population, algorithm).head
    heuristic.applyToLayoutWithExceptions(algorithm.computeLayout(bestSolution._1.regularEdgeLabeling), exceptions)
  }

  /** Adds a fitness score to every layout in a solution set, then sorts them with the highest score first. */
  private def addFitnessScores(solutions: Vector[AlternatingFourCycles[RVertex[V]]],
                               alg: RELDualAlgorithm[V, E]): Vector[(AlternatingFourCycles[RVertex[V]], Double)] = {
    solutions.map(s => (s, {
      val layout = heuristic.applyToLayoutWithExceptions(alg.computeLayout(s.regularEdgeLabeling), exceptions)
      fitnessScores.map(_.computeScore(layout)).sum
    }))
      .sortBy(_._2) // Sort by score, lowest first
  }

  /**
   * @param suboptimalSolutions All solutions that weren't selected as the top 4%, ordered with the best solution first.
   * @return A vector of solutions to use in the next iteration.
   */
  private def fillRemainingPopulationSlots(suboptimalSolutions: Vector[AlternatingFourCycles[RVertex[V]]],
                                           ran: Random): Vector[AlternatingFourCycles[RVertex[V]]] = {
    val withProbability: Vector[(Double, AlternatingFourCycles[RVertex[V]])] = suboptimalSolutions
      .zipWithIndex
      .map(s => (Math.pow(0.9, s._2 + 1), s._1)) // +1 since indices start at 0 rather than 1
    val randomSelection = new ProbabilityCollection(ran, withProbability:_*)
    val selected = new ListBuffer[AlternatingFourCycles[RVertex[V]]]()
    for (i <- 0 until suboptimalSolutions.size)
      selected += randomSelection.next

    selected.toVector
  }

  /**
   * A 5% chance to apply a drastic mutation of a solution by moving ((d*r)/6) steps up or down the lattice. 'd is the
   * number of left/right-alternating cycles that must be flipped in order to reach the maximal labeling. If no
   * mutation is applied, the input solution is returned instead.
   *
   * Weather movement should occur up or down depends on the normal distributed value r. If r is positive, move up.
   * Otherwise move down.
   */
  private def applyDrasticMutation(c: AlternatingFourCycles[RVertex[V]], ran: Random): AlternatingFourCycles[RVertex[V]] = {
    if (ran.nextInt(100) < 5) {
      val r = ran.nextGaussian()

      val cycles = if (r >= 0) c.allCycles.filter(_.isLeftAlternating) else c.allCycles.filter(_.isRightAlternating)
      val d = cycles.size
      val cyclesToFlip = Math.abs((r * d) / 6).toInt
      val remainingCycles = new ArrayBuffer[FourCycle[RVertex[V]]]() ++ cycles
      var current = c
      var i = 0

      while (i < cyclesToFlip && !remainingCycles.isEmpty) {
        val cycle = remainingCycles(ran.nextInt(remainingCycles.size))
        current = current.flipCycle(cycle)
        i += 1
      }

      current
    } else {
      c
    }
  }

  /**
   * A 90% chance to flip a random cycle.
   */
  private def applyMinorMutation(c: AlternatingFourCycles[RVertex[V]], ran: Random): AlternatingFourCycles[RVertex[V]] = {
    if (ran.nextInt(100) < 90) {
      val cycle = c.allCycles(ran.nextInt(c.allCycles.size))
      c.flipCycle(cycle)

    } else {
      c
    }
  }

  /**
   * Computes the minimal regular edge labeling by flipping every right-alternating cycle to the left.
   */
  private def minimalEdgLabeling(cycles: AlternatingFourCycles[RVertex[V]]): AlternatingFourCycles[RVertex[V]] = {
    var c = cycles
    for (rightCycle <- cycles.allCycles.filter(_.isRightAlternating))
      c = c.flipCycle(rightCycle)

    c
  }

  private def createInitialPopulation(minimalLabeling: AlternatingFourCycles[RVertex[V]], r: Random): Vector[AlternatingFourCycles[RVertex[V]]] = {
    val diameter = minimalLabeling.allCycles.size
    val randomDistributed = r.nextGaussian()
    val cyclesToFlip: Int = {
      val n: Double = Math.abs(diameter * (0.5 + (randomDistributed / 8)))
      Math.max(Math.ceil(n), 1).toInt
    }

    val population = new ListBuffer[AlternatingFourCycles[RVertex[V]]]()
    val leftCyclesRemaining = new ArrayBuffer[FourCycle[RVertex[V]]]() ++ minimalLabeling.allCycles
    var currentSolution = minimalLabeling
    population += currentSolution

    for (d <- 0 until cyclesToFlip if !leftCyclesRemaining.isEmpty) {
      val cycleIndex = r.nextInt(leftCyclesRemaining.size)
      val randomCycle = leftCyclesRemaining(cycleIndex)
      currentSolution = currentSolution.flipCycle(randomCycle)
      population += currentSolution
      leftCyclesRemaining.remove(cycleIndex)
    }

    population.toVector
  }

}
