package net.cyndeline.rlgraph.drawings

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlgraph.util.GraphCommons

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

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

  def setSize(s: Dimensions) = new StraightLineDrawing(vertices, edges, coordinates, s.width, s.height)

  def map[E](f: V => E): StraightLineDrawing[E] = {
    val m = vertices.map(v => v -> f(v))
    val mapped = m.toMap
    val updatedVertices = m.map(_._2)
    val updatedEdges = edges.map(e => (mapped(e._1), mapped(e._2)))
    val updatedCoordinates = coordinates.map(kv => mapped(kv._1) -> kv._2)
    new StraightLineDrawing(updatedVertices, updatedEdges, updatedCoordinates, width, height)
  }

  /** @return Cuts the width/height down the the size occupied by the vertices and their rectangles and also
    *         adjusts coordinates to 0.
    */
  def cut = {
    val d = adjustToZero
    val size = d.coordinateSize
    d.setSize(size)
  }

  /** @return How may coordinates are occupied along each axis by the vertices and rectangles of the drawing. Note
    *         that the width/height of the drawing may be greater.
    */
  def coordinateSize: Dimensions = StraightLineDrawing.size(coordinates)

  def scale(factor: Int): StraightLineDrawing[V] = {
    if (factor == 1) {
      this
    } else {
      val updatedCoordinates = coordinates.map(kv => kv._1 -> kv._2 * factor)
      val d = new StraightLineDrawing(vertices, edges, updatedCoordinates, width, height)
      d.setSize(d.coordinateSize)
    }
  }

  /**
    * Adds a constant to the x- and y coordinates of every point in a drawing.
    * @param x Value to add to every x-coordinate in the drawing.
    * @param y Value to add to every y-coordinate in the drawing.
    * @return A copy of the drawing with every coordinate adjusted by (x,y).
    */
  def adjust(x: Int, y: Int) = {
    val p = Point(x, y)
    val updatedCoordinates = coordinates.map(kv => kv._1 -> (kv._2 + p))
    updateCoordinates(updatedCoordinates)
  }

  /**
    * Adjusts the coordinates in the drawing so that the lowest x- and y coordinates are 0.
    * @return A copy of the drawing where every vertex has coordinates higher or equal to 0.
    */
  def adjustToZero: StraightLineDrawing[V] = {
    if (isEmpty)
      return this

    val lowX = coordinates.minBy(_._2.x)._2.x * -1
    val lowY = coordinates.minBy(_._2.y)._2.y * -1
    adjust(lowX, lowY)
  }

  def updateCoordinates(newCoordinates: Map[V, Point]) = {
    new StraightLineDrawing[V](vertices, edges, newCoordinates, width, height)
  }


  def updateEdges(newEdges: Vector[(V, V)]) = new StraightLineDrawing[V](vertices, newEdges, coordinates, width, height)

  override def equals(other: Any): Boolean = other match {
    case sld: StraightLineDrawing[V] =>
      vertices == sld.vertices && edges == sld.edges && coordinates == sld.coordinates && width == sld.width && height == sld.height
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + vertices.## + edges.## + coordinates.## + width.## + height.##)

}

object StraightLineDrawing {

  def empty[V] = new StraightLineDrawing[V](Vector(), Vector(), Map(), 0, 0)

  def singleVertex[V](v: V, p: Point = Point(0, 0)) = new StraightLineDrawing[V](Vector(v), Vector(), Map(v -> p), 1, 1)

  def apply[V](graph: Graph[V, UnDiEdge], coordinates: Map[V, Point]) = {
    val vertices = GraphCommons.outerVertices(graph)
    val edges = GraphCommons.outerEdges(graph).map(e => (e._1, e._2))
    val size = StraightLineDrawing.size(coordinates)
    new StraightLineDrawing[V](vertices, edges, coordinates, size.width, size.height)
  }

  /**
    * Computes width and height for a drawing based on the rectangles specified for its vertices rather than the
    * vertex coordinates itself.
    */
  def setRectangleSize[V](drawing: StraightLineDrawing[V], rectangles: Map[V, Dimensions]): StraightLineDrawing[V] = {
    val rs = drawing.vertices.map(v => Rectangle.centerAround(drawing.coordinates(v), rectangles.getOrElse(v, Dimensions(1, 1)))).iterator
    var minX = Int.MaxValue
    var minY = Int.MaxValue
    var maxX = Int.MinValue
    var maxY = Int.MinValue

    while (rs.hasNext) {
      val r = rs.next()
      minX = Math.min(minX, r.start.x)
      maxX = Math.max(maxX, r.stop.x)
      minY = Math.min(minY, r.start.y)
      maxY = Math.max(maxY, r.stop.y)
    }

    drawing.setSize(Dimensions(Math.abs(maxX - minX) + 1, Math.abs(maxY - minY) + 1))
  }

  /**
    * Sets the drawings dimension based on rectangle values rather than vertex coordinates. Assumes the vertex
    * lies at the centre of the coordinate. Also adjusts coordinates to 0.
    */
  def rectangleCut[V](drawing: StraightLineDrawing[V], rectangles: Map[V, Dimensions]): StraightLineDrawing[V] = {
    val rs = drawing.vertices.map(v => Rectangle.centerAround(drawing.coordinates(v), rectangles.getOrElse(v, Dimensions(1, 1))))
    val lowX = rs.minBy(_.start.x).start.x * -1
    val lowY = rs.minBy(_.start.y).start.y * -1
    setRectangleSize(drawing.adjust(lowX, lowY), rectangles)
  }

  /**
    * Scales the coordinates of a drawing by a factor of the largest rectangle side, then re-computes the coordinate
    * by assuming it's at the center of the rectangle.
    */
  def rectangleScale[V](drawing: StraightLineDrawing[V], rectangles: Map[V, Dimensions]): StraightLineDrawing[V] = {
    if (rectangles.isEmpty)
      return drawing

    val longestSide = rectangles.values.map(d => Math.max(d.width, d.height)).max
    val scaled = drawing.scale(longestSide)
    val center = Math.floor(longestSide / 2d).toInt
    setRectangleSize(scaled.adjust(center, center), rectangles)
  }

  private def size[V](coordinates: Map[V, Point]): Dimensions = {
    if (coordinates.isEmpty) {
      Dimensions(0, 0)
    } else {
      var minX = Int.MaxValue
      var minY = Int.MaxValue
      var maxX = Int.MinValue
      var maxY = Int.MinValue
      val points = coordinates.valuesIterator
      while (points.hasNext) {
        val point = points.next()
        minX = Math.min(minX, point.x)
        maxX = Math.max(maxX, point.x)
        minY = Math.min(minY, point.y)
        maxY = Math.max(maxY, point.y)

      }

      Dimensions(Math.abs(maxX - minX) + 1, Math.abs(maxY - minY) + 1)
    }
  }

}
