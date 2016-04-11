package net.cyndeline.rlgraph.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.drawing.DrawnEdge

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Contains the final orthogonalized graph data.
 */
trait OrthogonalRepresentation[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @return A list of every edge in the graph, along with bends.
   */
  def edges: Vector[DrawnEdge[VType, EType]]

  /**
   * @return A list of every vertex in the graph, the x coordinate it is drawn on, and the y coordinate.
   */
  def vertices: Vector[(VType, Int, Int)]

}
