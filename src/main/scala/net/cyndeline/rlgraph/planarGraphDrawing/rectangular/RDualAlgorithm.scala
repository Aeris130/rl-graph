package net.cyndeline.rlgraph.planarGraphDrawing.rectangular

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes a rectangular dual without any input from the user other than the graph and the outer face. For an algorithm
 * description, see the RectangularDualAlgorithm documentation.
 */
trait RDualAlgorithm[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @param graph A planar graph.
   * @return A rectangular layout of the graph.
   */
  def computeLayout(graph: Graph[VType, EType]): RectangularLayout[VType, EType]

}
