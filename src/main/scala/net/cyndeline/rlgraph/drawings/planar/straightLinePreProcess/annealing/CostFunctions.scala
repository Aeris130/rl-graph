package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlcommon.math.SpireRational._
import net.cyndeline.rlcommon.math.geom.Rectangle
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlcommon.stat.Statistics
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess.Settings
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment
import net.cyndeline.rlgraph.util.GraphCommons
import spire.math.Rational

/**
  * The functions used to evaluate the current state of a layout using costs between 0 and 1.
  * A lower cost indicates a more optimal state.
  */
object CostFunctions {

  val maximumScore = Rational(3)

  // The number of neighbors to compare against when checking distance uniformity
  private val neighborsToCheck = 20

  /**
    * Scores the layout based on how close to the target edge length the edges of a vertex is.
    * Can be called once per vertex modification.
    */
  def edgeLength(state: LayoutState, v: Int, targetLength: Rational, vertexAmount: Int): Rational = {
    if (vertexAmount == 1)
      return 0

    val longestEdgeValue = diagonal(state)
    val maxDivergence = longestEdgeValue - targetLength
    val minDivergence = Rational.zero // 0 divergence when the edge is equal to the target
    val vRectangle = state.layout.rectangle(v)
    val neighborRectangles = GraphCommons.outerNeighbors(v, state.layout.graph).map(state.layout.rectangle)

    val distances = neighborRectangles.map(_.shortestDistance(vRectangle)).map(distance => Normalize((distance - targetLength).abs, minDivergence, maxDivergence))
    Normalize(distances.sum, Rational.zero, Rational(distances.size))
  }

  /**
    * Scores a layout better the further its vertices lies from the border lines. This pushes vertices towards the
    * center, i.e it acts as an opposite force to the distribution metric.
    */
  def borderLines(state: LayoutState, v: Int): Rational = {
    val vr = state.layout.rectangle(v)
    def distance(r: Rectangle, side: String) = side match {
      case "Left" => r.start.x //(0 - x)
      case "Right" => Rational(state.size - 1) - r.stop.x
      case "Bottom" => r.start.y
      case "Top" => Rational(state.size - 1) - r.stop.y
    }

    // 1 (worst score) if the rectangle is at the border, then successively better the further away from the border it is.
    def score(distance: Rational) = if (distance == Rational.zero) Rational.one else Rational.one / distance

    // This score will never be 0, as moving away from one border will move the rectangle towards another
    val totalScore = score(distance(vr, "Left")) + score(distance(vr, "Right")) + score(distance(vr, "Bottom")) + score(distance(vr, "Top"))
    Normalize(totalScore, Rational.zero, Rational(4))
  }

  /**
    * Scores the layout based on how evenly the vertices are positions compared to each other, by checking how
    * close the vertex is positioned to its k nearest neighbors in the layout.
    */
  def vertexDistribution(state: LayoutState, v: Int, vertexAmount: Int): Rational = {
    if (vertexAmount == 1)
      return 0

    val vRectangle = state.layout.rectangle(v)
    //TODO Change to approximation method to optimize search
    val neighbors = state.kdTree.nearestNeighbor(neighborsToCheck, vRectangle).filter(_ != vRectangle) // Nearest neighbor search will also include v
    val distances = neighbors.map(_.shortestDistance(vRectangle))
    val longestDistance = diagonal(state)
    val deviationFromMean: Double = Statistics.stdDeviation(distances)

    /* The lowest possible variance is 0, the highest is the diagonal of the layout. This occurs if all vertices are
     * placed in a corner with no variance between each other, and a single vertex lies at the opposite corner.
     */
    Normalize(Rational(deviationFromMean), Rational.zero, longestDistance)
  }

  private def diagonal(state: LayoutState): Rational = Rational(state.size * Math.sqrt(2))

}

/**
  * Stores the current state of the layout.
  *
  * @param size   The height and width of the drawing. Used as the upper maximum for edge lengths, and also to check where
  *               the borders are (assumes that the layout begins at (0,0).
  * @param kdTree Contains an up-to-date representation of the vertex rectangles in the layout, so that distance
  *               uniformity can be computed using the k nearest neighbors, and not just the neighbors connected
  *               using edges.
  * @param score Every vertex last score, stored at the index of the vertex id.
  */
class LayoutState private(val size: Int,
                          val layout: VertexAdjustment,
                          val kdTree: KDTree[Rectangle, Rectangle],
                          val score: Vector[Rational],
                          adjustments: Int) {

  // Sets the score to its maximum for every vertex
  def this(size: Int, layout: VertexAdjustment, kdTree: KDTree[Rectangle, Rectangle], settings: Settings) =
    this(size, layout, kdTree, Vector.fill(layout.coordinate.size)(-1), 0)

  def move(v: Int, newLayout: VertexAdjustment) = {
    val oldRectangle = layout.rectangle(v)
    val newRectangle = newLayout.rectangle(v)
    new LayoutState(size, newLayout, kdTree.delete(oldRectangle).insert(newRectangle), score, adjustments + 1)
  }

  def balance = if (adjustments < size) {
    this
  } else {
    new LayoutState(size, layout, kdTree.balance, score, 0)
  }

  def updateScore(v: Int, newScore: Rational) = new LayoutState(size, layout, kdTree, score.updated(v, newScore), adjustments)

}
