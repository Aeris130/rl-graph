package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{CostFunction, LayoutState}
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess.Settings
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Performs a number of modifications on a graph layout by randomly moving its vertices one by one, then
  * rating the new layout based on some cost functions that evaluates how nicely the new layout is drawn.
  * If the modified layout has a better cost than the old one, it is kept.
  *
  * The severity of each modification (the distance a vertex is moved) depends on a temperature T that decreases
  * as the process goes on.
  *
  * @param vertices The number of vertices to modify. Assumes that vertices goes from 0 to (vertices - 1)
  */
class AnnealingProcess[S <: LayoutState[S]] private(vertices: Int,
                                                   score: Double,
                                                   state: LayoutState[S],
                                                   adjustmentFactory: VertexAdjustment,
                                                   settings: Settings[S]) {

  /** @return Every vertex-coordinate at the current state of the process. */
  def coordinates: Map[Int, Point] = adjustmentFactory.coordinate

  /**
    * @param v A vertex in the adjustment.
    * @return A rectangle representing the vertex. A 1x1 rectangle if no user-specified dimensions are available.
    */
  def rectangle(v: Int): Rectangle = adjustmentFactory.rectangle(v)

  /**
    * Restarts the process with a new temperature and cooling.
    *
    * @param newTemperature New temperature to use.
    * @param newCooling     New cooling value to use.
    * @return A new process using the current segments of the old process, but with a new temperature and cooling.
    */
  def reset(newTemperature: Double, newCooling: Double) = updateSettings(settings.setTemperature(newTemperature).setCooling(newCooling))

  /** @return A new process having its temperature decreased by temp * cooling. */
  def cool = updateSettings(settings.setTemperature(settings.temperature * settings.cooling))

  /**
    * @param iterations The number of times the entire vertex set should be modified at the current temperature.
    * @return A copy of this process with x * n modifications attempted, where x is the number of iterations, and n
    *         is the number of vertices in the process.
    */
  def run(iterations: Int, random: Random): Option[AnnealingProcess[S]] = {
    // State-related data
    var currentState = state
    var currentAdjustment = adjustmentFactory
    var currentScore = score
    // ------------------
    var modified = false

    var i = 0
    while (i < iterations) {
      var v = 0
      while (v < vertices) {
        val angle = random.nextInt(360)
        val oldPos = currentAdjustment.coordinate(v)
        val newPos = oldPos.move(angle, settings.temperature)
        val adjusted = currentAdjustment.moveVertex(v, newPos)

        // Discard change if edge crossings occurred
        if (adjusted.isDefined) {

          // Update the state of the layout
          val newState = currentState.moveVertex(v, oldPos, newPos)
          val vertexScore = AnnealingProcess.scoreLayout(newState, v, settings, vertices)
          val newScore = currentScore - currentState.score(v) + vertexScore

          if (newScore < currentScore) {
            modified = true
            currentScore = newScore
            currentAdjustment = adjusted.get

            /* Re-balances the neighbor tree occasionally. Needs to be done only when a new state is accepted,
             * otherwise balancing may be done multiple times if the n:th state is scheduled for balancing and
             * the (n-1)th state produces multiple changes that all receives a lower score than itself.
             */
            currentState = newState.registerChange(v, oldPos, newPos).setScore(v, vertexScore)
          }
        }

        v += 1
      } // End of vertices

      i += 1
    } // End of iterations

    if (modified)
      Some(updateState(currentScore, currentState, currentAdjustment))
    else
      None
  }

  private def updateSettings(newSettings: Settings[S]) = new AnnealingProcess(vertices, score, state, adjustmentFactory, newSettings)

  private def updateState(newScore: Double, newState: LayoutState[S], newAdjustment: VertexAdjustment) =
    new AnnealingProcess(vertices, newScore, newState, newAdjustment, settings)

}

/** Factory object for the algorithm. */
object AnnealingProcess {

  /**
    * @param drawing The drawing to modify.
    * @param graph The original graph, containing edge-neighbor relationships.
    * @param border Size b of the border, signifying a border from (0,0) to (b-1,b-1) that movements must reside within
    *               to be considered valid.
    * @param settings Parameters for the algorithm.
    * @return An initial algorithm used to adjst the drawing based on its cost functions.
    */
  def apply[S <: LayoutState[S]](drawing: StraightLineDrawing[Int],
                                 graph: Graph[Int, UnDiEdge],
                                 border: Int,
                                 settings: Settings[S],
                                 state: S): AnnealingProcess[S] = {
    require(border >= drawing.width && border >= drawing.height, "Border dimensions must be >= input drawing.")
    require(border > 0, "Border must be >= 1.")
    val adjustment = VertexAdjustment(drawing, graph, border, Map())
    var newState = state.setLayout(drawing.setSize(Dimensions(border, border)), graph)

    var i = 0
    val stop = drawing.vertices.size
    while (i < stop) {
      val vertexScore = settings.costFunctions.map(_.score(newState, i))
      newState = newState.setScore(i, vertexScore.sum)
      i += 1
    }

    new AnnealingProcess(drawing.vertices.size, newState.totalScore, newState, adjustment, settings)
  }

  private def scoreLayout[S <: LayoutState[S]](state: S, v: Int, settings: Settings[S], vertexAmount: Int): Double = {
    settings.costFunctions.flatMap(f => if (f.weight != 0) Some(f.score(state, v)) else None).sum
  }

  /** Misc data needed for computations. */
  case class Settings[S <: LayoutState[S]](temperature: Double,
                                           cooling: Double,
                                           costFunctions: Vector[CostFunction[S]]) {

    require(cooling > 0, "Cooling must be greater than 0")
    require(cooling <= 1, "Cooling must be less than or equal to 1")

    def setTemperature(newTemp: Double) = Settings(newTemp, cooling, costFunctions)

    def setCooling(newCooling: Double) = Settings(temperature, newCooling, costFunctions)

  }

}
