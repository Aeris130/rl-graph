package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Implemented by cost functions used by the annealing process.
  */
trait CostFunction[S <: LayoutState[S]] {

  /**
    * @param state The current state of the layout.
    * @param v The latest vertex that was adjusted in the layout. Used for scoring algorithms that only wants
    *          to examine changes local to a vertex.
    * @return A value between 0 and 1 (inclusive). A better layout should receive a lower score.
    */
  def score(state: S, v: Int): Double

  /** @return A value that the score of the cost function should be multiplied with. */
  def weight: Double

}

trait LayoutState[S <: LayoutState[S]] {

  /** @return The number of vertices in the layout. */
  def vertices: Int

  /** Called once before the annealing process begins.
    * @param drawing The initial drawing sent to the annealing process.
    * @param graph The graph that the drawing was based on.
    * @return A copy of the state with the drawing and its data set.
    */
  def setLayout(drawing: StraightLineDrawing[Int], graph: Graph[Int, UnDiEdge]): S

  /**
    * Adjusts the state of the layout by moving a single vertex. This method is called before the layout is
    * accepted, and changes in underlying data-structures that only needs to be made after acception can be put
    * in the register method.
    * @param v A vertex in the input graph.
    * @param from Old position of v.
    * @param to New position of v. This will result in a valid layout.
    * @return A copy of the layout with the vertex v being registered at a new position.
    */
  def moveVertex(v: Int, from: Point, to: Point): S

  /**
    * Called whenever a change in the layout is accepted and made permanent.
    */
  def registerChange(v: Int, from: Point, to: Point): S

  /**
    * @param v A vertex v.
    * @param score An accepted score for v.
    */
  def setScore(v: Int, score: Double): S

  /**
    * @param v A vertex v.
    * @return The current score of vertex v that was computed when the state was accepted or created. -1 if no
    *         score has yet been set for v.
    */
  def score(v: Int): Double

  /** @return The sum of all vertex scores. */
  def totalScore: Double

}
