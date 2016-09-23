package net.cyndeline.rlgraph.drawings

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}

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
  require((vertices.isEmpty && edges.isEmpty && coordinates.isEmpty) || vertices.nonEmpty,
    "An empty drawing cannot have vertex coordinates or edges set.")

  def isEmpty: Boolean = vertices.isEmpty

  def updateCoordinates(newCoordinates: Map[V, Point]) =
    new StraightLineDrawing[V](vertices, edges, newCoordinates, width, height)

  def updateEdges(newEdges: Vector[(V, V)]) = new StraightLineDrawing[V](vertices, newEdges, coordinates, width, height)

  override def equals(other: Any): Boolean = other match {
    case sld: StraightLineDrawing[V] => vertices == sld.vertices && edges == sld.edges && coordinates == sld.coordinates && width == sld.width && height == sld.height
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + vertices.## + edges.## + coordinates.## + width.## + height.##)

}

object StraightLineDrawing {

  def empty[V] = new StraightLineDrawing[V](Vector(), Vector(), Map(), 0, 0)

  def singleVertex[V](v: V, p: Point = Point(0, 0)) = new StraightLineDrawing[V](Vector(v), Vector(), Map(v -> p), 1, 1)

  /**
    * Scales the coordinates of each vertex in the drawing based on a set of 2D dimensions, such that the vertices
    * retains their coordinates relative to each other, and every vertex lies at the center of its rectangle. This is
    * done by scaling the drawing such that every coordinate becomes an nxn rectangular area, where n is the longest
    * rectangle-side. If that side has even length, it is increased by 1 to allow a whole center coordinate for each
    * rectangular vertex area.
    *
    * @param drawing Drawing to scale.
    * @param rectangles Maps each vertex against the dimensions that should be used to draw it as a rectangle. If a
    *                   vertex is missing its entry in this map, it'll be processed using a 1x1 area.
    * @tparam V Vertex type in drawing.
    * @return A layout scaled such that every vertex can be drawn at the center of its rectangle.
    */
  def scale[V](drawing: StraightLineDrawing[V], rectangles: Map[V, Dimensions]): StraightLineDrawing[V] = {
    if (drawing.vertices.isEmpty)
      return drawing // Nothing to scale...

    val largestSide: Int = {
      val v = rectangles.values.map(d => Math.max(d.width, d.height))
      if (v.isEmpty)
        1
      else {
        val max = v.max
        if (max % 2 == 0) max + 1 else max
      }
    }

    if (largestSide < 2)
      return drawing // No need to scale anything.

    // Multiply coordinates
    val updatedCoordinates = drawing.coordinates.map(kv => kv._1 -> kv._2 * largestSide)
    new StraightLineDrawing(drawing.vertices, drawing.edges, updatedCoordinates, drawing.width * largestSide, drawing.height * largestSide)
  }

  /**
    * Adds a constant to the x- and y coordinates of every point in a drawing.
    * @param x Value to add to every x-coordinate in the drawing.
    * @param y Value to add to every y-coordinate in the drawing.
    * @param drawing Drawing to modify.
    * @tparam V Vertex type in the drawing.
    * @return A copy of the drawing with every coordinate adjusted by (x,y).
    */
  def adjust[V](x: Int, y: Int, drawing: StraightLineDrawing[V]) = {
    val p = Point(x, y)
    val updatedCoordinates = drawing.coordinates.map(kv => kv._1 -> (kv._2 + p))
    drawing.updateCoordinates(updatedCoordinates)
  }

  /**
    * Adjusts the coordinates in the drawing so that the lowest x- and y coordinates are 0.
    * @param drawing Drawing to modify.
    * @tparam V Vertex type in the drawing.
    * @return A copy of the drawing where every vertex has coordinates higher or equal to 0.
    */
  def adjustToZero[V](drawing: StraightLineDrawing[V]): StraightLineDrawing[V] = {
    if (drawing.isEmpty)
      return drawing

    val lowX = drawing.coordinates.minBy(_._2.x)._2.x * -1
    val lowY = drawing.coordinates.minBy(_._2.y)._2.y * -1
    adjust(lowX, lowY, drawing)
  }

}
