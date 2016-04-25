package net.cyndeline.rlgraph.drawings

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

/**
  * Stores vertices and the coordinates they're drawn at. Assumes that a straight line is drawn between any
  * vertex pair.
  *
  * @param vertices Every vertex in the drawing.
  * @param edges Every pair of vertices in the drawing connected by a straight edge.
  * @param coordinates Maps ever vertex in the drawing to the coordinate it should be drawn to.
  * @param width The total width of the drawing. This area may be larger than the lowest/highest coordinate if the
  *              algorithm that produced it lets the user specify how large the drawings area should be.
  * @param height The total height of the drawing. This area may be larger than the lowest/highest coordinate if the
  *               algorithm that produced it lets the user specify how large the drawings area should be.
  */
class StraightLineDrawing[V](val vertices: Vector[V],
                             val edges: Vector[(V, V)],
                             val coordinates: Map[V, Point],
                             val width: Int,
                             val height: Int) {

}
