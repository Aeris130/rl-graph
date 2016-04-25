package net.cyndeline.rlgraph.drawings.planar.grid

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing

/**
  * Represents a planar graph drawn on a grid using straight lines using the algorithm by Chrobak and Payne. It adds the
  * following information to a straight-line drawing:
  *  1. Which pair of vertices serves as left and right neighbor of a vertex (if the graph isn't maximally planar one of
  *  these vertices may not be connected using an edge in the original graph)
  *  1. Which sequence of vertices (if any) a vertex covered when it was added to the drawing.
  *
  * Note that if the algorithm that generated the drawing supports biconnected graphs, every vertex added to the drawing
  * may not have two children already present in it. In such a case, a vertex in the drawing will need to be used
  * as a substitute left/right child in order to attain the triangular characteristics of the drawing. The result is
  * that some of the nodes designated as left or right children may not be connected to their parent in the original
  * embedding, as the connection is equivalent to a dummy edge being added to make the embedding valid.
  *
  * @param vertices Every vertex in the drawing, in the order they were added to it.
  * @param es Every edge in the drawing.
  * @param coordinates Every vertex in the drawing mapped to the coordinate it should be drawn to. If this drawing
  *                    was based off of rectangles with dimensions > 1x1, this is the center coordinate for each
  *                    rectangle (rounded down if rectangle dimension is even).
  * @param rects Maps every vertex to a rectangle representing it in the drawing. Leave this map empty if every
  *              rectangle should have size 1x1 by default.
  */
class GridDrawing(override val vertices: Vector[Int],
                  es: Vector[(Int, Int)],
                  override val coordinates: Map[Int, Point],
                  rects: Map[Int, Rectangle]) extends StraightLineDrawing[Int](vertices, es, coordinates, coordinates.values.maxBy(_.x).x + 1, coordinates.values.maxBy(_.y).y + 1) {
  require(rects.isEmpty || rects.keySet == vertices.toSet, "The rectangle map for a drawing must either be empty or specified for every vertex.")

  /**
    * @param v A vertex in the drawing.
    * @return The rectangle that represents the vertex v in the drawing, guaranteed to not overlap other rectangles
    *         other that sharing edges with them.
    */
  def rectangle(v: Int): Rectangle = rects.getOrElse(v, Rectangle(coordinates(v), 1, 1))

}

object GridDrawing {

  /**
    * Produces a grid drawing where every rectangle has size 1x1 and starts at the coordinate of the vertex it
    * belongs to.
    * @param vertices Every vertex in the drawing.
    * @param edges Every edge in the drawing.
    * @param coordinates Every vertex in the drawing mapped to the coordinate it should be drawn to.
    * @param leftRightChildren Maps each vertex to the (left, right) child neighbor that it was drawn against in the
    *                          drawing.
    * @param cover Maps every vertex to the sequence of vertices that were removed from the outer contour of the drawing
    *              when the vertex was added.
    * @return A drawing with every rectangle set to size 1x1.
    */
  def singleCoordinateRectangles(vertices: Vector[Int], edges: Vector[(Int, Int)],
                                 coordinates: Map[Int, Point], leftRightChildren: Map[Int, (Int, Int)],
                                 cover: Map[Int, Vector[Int]]): GridDrawing = {
    new GridDrawing(vertices, edges, coordinates, Map())
  }
}
