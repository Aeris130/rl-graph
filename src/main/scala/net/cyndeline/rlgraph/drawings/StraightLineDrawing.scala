package net.cyndeline.rlgraph.drawings

import net.cyndeline.rlcommon.math.geom.Point

/**
  * Stores vertices and the coordinates they're drawn at. Assumes that a straight line is drawn between any
  * vertex pair.
  */
class StraightLineDrawing[V](val vertices: Vector[V], val edges: Vector[(V, V)], val coordinates: Map[V, Point]) {

}
