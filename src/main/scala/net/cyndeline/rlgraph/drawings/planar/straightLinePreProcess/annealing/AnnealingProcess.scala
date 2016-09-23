package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlcommon.math.SpireRational._
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess.Settings
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment
import spire.math.Rational

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
class AnnealingProcess private(vertices: Int,
                               score: Rational,
                               state: LayoutState,
                               adjustmentFactory: VertexAdjustment,
                               settings: Settings) {

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
  def reset(newTemperature: Rational, newCooling: Rational) = updateSettings(settings.setTemperature(newTemperature).setCooling(newCooling))

  /** @return A new process having its temperature decreased by temp * cooling. */
  def cool = updateSettings(settings.setTemperature(settings.temperature * settings.cooling))

  /**
    * @param iterations The number of times the entire vertex set should be modified at the current temperature.
    * @return A copy of this process with x * n modifications attempted, where x is the number of iterations, and n
    *         is the number of vertices in the process.
    */
  def run(iterations: Int, random: Random): Option[AnnealingProcess] = {
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
        val pos = currentAdjustment.coordinate(v).move(angle, settings.temperature)
        val adjusted = currentAdjustment.moveVertex(v, pos)

        // Discard change if edge crossings occurred
        if (adjusted.isDefined) {

          // Update the state of the layout
          val newState = currentState.move(v, adjusted.get)
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
            currentState = newState.balance.updateScore(v, vertexScore)
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

  private def updateSettings(newSettings: Settings) = new AnnealingProcess(vertices, score, state, adjustmentFactory, newSettings)

  private def updateState(newScore: Rational, newState: LayoutState, newAdjustment: VertexAdjustment) =
    new AnnealingProcess(vertices, newScore, newState, newState.layout, settings)

}

/** Factory object for the algorithm. */
object AnnealingProcess {

  /**
    * @param drawing The drawing to modify.
    * @param graph The original graph, containing edge-neighbor relationships.
    * @param border Size b of the border, signifying a border from (0,0) to (b-1,b-1) that movements must reside within
    *               to be considered valid.
    * @param vertexRectangles Specifies dimensions for rectangles representing vertices. If a vertex is missing an
    *                         entry, it will be considered as a 1x1 rectangle.
    * @param settings Parameters for the algorithm.
    * @return An initial algorithm used to adjst the drawing based on its cost functions.
    */
  def apply(drawing: StraightLineDrawing[Int],
            graph: Graph[Int, UnDiEdge],
            border: Int,
            vertexRectangles: Map[Int, Dimensions],
            settings: Settings): AnnealingProcess = {
    require(border >= drawing.width && border >= drawing.height, "Border dimensions must be >= input drawing.")
    require(border > 0, "Border must be >= 1.")
    val adjustment = VertexAdjustment(drawing, graph, border, vertexRectangles)
    val tree = KDTree.rectangleTree(drawing.vertices.map(adjustment.rectangle))
    var state = new LayoutState(border, adjustment, tree, settings)
    val vAmount = drawing.vertices.size

    var i = 0
    val stop = drawing.vertices.size
    while (i < stop) {
      val vertexScore = scoreLayout(state, i, settings, vAmount)
      state = state.updateScore(i, vertexScore)
      i += 1
    }

    new AnnealingProcess(drawing.vertices.size, state.score.sum, state, adjustment, settings)
  }

  private def scoreLayout(state: LayoutState, v: Int, settings: Settings, vertexAmount: Int): Rational = {

    // Optimize as much as possible by skipping checks that will get multiplied to zero anyway.
    val edgeScore = if (!settings.edgeWeight.isZero)
      CostFunctions.edgeLength(state, v, settings.targetEdgeLength, vertexAmount) * settings.edgeWeight
    else
      Rational.zero

    val borderScore = if (!settings.borderWeight.isZero)
      CostFunctions.borderLines(state, v) * settings.borderWeight
    else
      Rational.zero

    val distributionScore = if (!settings.distributionWeight.isZero)
      CostFunctions.vertexDistribution(state, v, vertexAmount) * settings.distributionWeight
    else
      Rational.zero

    edgeScore + borderScore + distributionScore
  }

  /** Misc data needed for computations. */
  case class Settings(temperature: Rational,
                      cooling: Rational,
                      targetEdgeLength: Rational,
                      edgeWeight: Rational,
                      borderWeight: Rational,
                      distributionWeight: Rational) {

    require(targetEdgeLength >= Rational.zero && edgeWeight >= Rational.zero && borderWeight >= Rational.zero, "Cost weights must be >= 0.")
    require(cooling > Rational.zero, "Cooling must be greater than 0")
    require(cooling < Rational.one, "Cooling must be less than 1")

    def setTargetEdgeLength(newLength: Rational) = Settings(temperature, cooling, newLength, edgeWeight, borderWeight, distributionWeight)

    def setTemperature(newTemp: Rational) = Settings(newTemp, cooling, targetEdgeLength, edgeWeight, borderWeight, distributionWeight)

    def setCooling(newCooling: Rational) = Settings(temperature, newCooling, targetEdgeLength, edgeWeight, borderWeight, distributionWeight)

  }

}
