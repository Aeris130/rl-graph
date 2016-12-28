package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Updates an interval KD tree in order to find the k nearest neighbors.
  */
class DefaultState(val size: Int,
                   vertexAmount: Int,
                   scores: Vector[Double],
                   val kdTree: KDTree[Point, Rectangle],
                   kdTreeAdjustments: Int,
                   balanceAdjustments: Int,
                   oldAdjustment: VertexAdjustment,
                   latestAdjustment: VertexAdjustment,
                   val graph: Graph[Int, UnDiEdge]) extends LayoutState[DefaultState] {

  def this() = this(0, 0, Vector(), KDTree.point2DTree(Vector()), 1, 1, null, null, null)

  def coordinate(v: Int) = latestAdjustment.coordinate(v)

  override def vertices: Int = vertexAmount

  // Called once
  override def setLayout(drawing: StraightLineDrawing[Int], graph: Graph[Int, UnDiEdge]): DefaultState = {
    val size = Math.max(drawing.width, drawing.height)
    val scores = drawing.vertices.map(v => -1d)
    var tree = kdTree
    for (v <- drawing.vertices) {
      tree = tree.insert(drawing.coordinates(v))
    }
    val vertexAmount = drawing.vertices.size
    val adjustment = VertexAdjustment(drawing, graph, size, Map())
    new DefaultState(size, vertexAmount, scores, tree.balance, 0, vertexAmount, adjustment, adjustment, graph)
  }

  override def moveVertex(v: Int, from: Point, to: Point): DefaultState = {
    val newAdjust = latestAdjustment.moveVertex(v, to).getOrElse(throw new Error(s"Attempted to register a move from $from to $to for vertex $v, but it resulted in intersections."))
    new DefaultState(size, vertexAmount, scores, kdTree, kdTreeAdjustments, balanceAdjustments, latestAdjustment, newAdjust, graph)
  }

  override def registerChange(v: Int, from: Point, to: Point): DefaultState = {
    var tree = kdTree.delete(oldAdjustment.coordinate(v)).insert(latestAdjustment.coordinate(v))
    val kdAdjustments = if (kdTreeAdjustments >= balanceAdjustments - 1) {
      tree = tree.balance
      0
    } else {
      kdTreeAdjustments + 1
    }
    new DefaultState(size, vertexAmount, scores, tree, kdAdjustments, balanceAdjustments, oldAdjustment, latestAdjustment, graph)
  }

  override def setScore(v: Int, score: Double): DefaultState = {
    new DefaultState(size, vertexAmount, scores.updated(v, score), kdTree, kdTreeAdjustments, balanceAdjustments, oldAdjustment, latestAdjustment, graph)
  }

  override def score(v: Int): Double = scores(v)

  override def totalScore: Double = scores.sum
}
