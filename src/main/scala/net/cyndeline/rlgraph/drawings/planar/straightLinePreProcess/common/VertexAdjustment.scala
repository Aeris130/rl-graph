package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlcommon.math.geom.spatialIndex.ArrayGrid
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.util.GraphCommons

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Keeps track of line segments and vertices (represented as rectangular areas) in a drawing, and determines if
  * moving a vertex from its current position to a new one results in a valid drawing. Each vertex-rectangle will
  * have the vertex in the center of it. If no center coordinate exists, the one closer to the rectangles starting point
  * will be chosen.
  *
  * A movement is rejected if:
  *
  *  - The movement causes the vertices rectangle to overlap the outside of the drawings border (even partially).
  *  - An edge that was connected to the moved vertex ends up intersecting another edge or vertex-rectangle.
  *  - The vertex-rectangle that was moved ends up intersecting another edge or vertex-rectangle.
  *
  */
class VertexAdjustment(border: Rectangle,
                       val coordinate: Map[Int, Point],
                       val graph: Graph[Int, UnDiEdge],
                       segments: IDSegmentManager,
                       grid: ArrayGrid[IDLine],
                       dimensions: Map[Int, Dimensions]) {

  /**
    * @param v A vertex in the adjustment.
    * @return A rectangle representing the vertex. A 1x1 rectangle if no user-specified dimensions are available.
    */
  def rectangle(v: Int): Rectangle = if (dimensions.contains(v))
      Rectangle(coordinate(v), dimensions(v))
    else
      Rectangle(coordinate(v), 1, 1)

  /**
    * @param v A vertex to move.
    * @param to Position to move vertex to.
    * @return A new adjustment object with the coordinate for the vertex updated, or None if moving the vertex
    *         (and its associated rectangle, if rectangle dimensions have been specified) results in intersections
    *         with other vertices or edges.
    */
  def moveVertex(v: Int, to: Point): Option[VertexAdjustment] = {
    val oldC = coordinate(v)

    if (oldC == to)
      return None

    val dx = to.x - oldC.x
    val dy = to.y - oldC.y

    val neighbors = GraphCommons.outerNeighbors(v, graph)
    val edgesToMove = (for(n <- neighbors) yield segments.edgeLine(n, v)).map(id => segments.segments(id).asInstanceOf[EdgeLine])
    val borderToMove = vertexEdges(v) // Empty if the vertex is single-coordinate

    /* Check that v isn't moved onto any of its neighbors. Classes below can't handle segments starting
     * and ending on the same coordinate.
     */
    if (neighbors.exists(n => coordinate(n) == to))
      return None

    val movedEdges = edgesToMove.map(e => e.adjust(v, to))
    val movedBorder = borderToMove.map(b => b.adjustBy(dx, dy))

    // Special border-case if the vertex has no neighbors and no rectangle-borders
    if (!border.containsPoint(to))
      return None

    // Check that no edge moves outside the border
    if (movedEdges.exists(e => !border.containsPoint(e.start) || !border.containsPoint(e.stop)) ||
      movedBorder.exists(b => !border.containsPoint(b.start) || !border.containsPoint(b.stop)))
      return None

    /* Check that v isn't moved onto one of its neighbors, and that v isn't moved inside one of its
     * neighbors rectangles. This won't get caught in the other checks, as a pair of vertices with no
     * other edges won't induce any intersections this way.
     */
    if (neighborOverlapExists(v, to, neighbors))
      return None


    /* There are three instances of intersection that can be overlooked: When two edges connected to the same vertex
     * intersect at that vertex (overlaps are still invalid), when the edge between two vertex centres intersects the
     * border of a rectangle belonging to one those vertices, when two corners of border segments belonging to the
     * same rectangle intersects.
     */
    def intersects(l1: IDLine, l2: IDLine): Boolean = {
      if (l1.intersects(l2)) {
        (l1, l2) match {
          case (e1: EdgeLine, e2: EdgeLine) =>
            val intersection = e1.intersection(e2)
            (intersection.isDefined && intersection.get.isInterval) ||
              (e1.startVertex != e2.startVertex && e1.stopVertex != e2.startVertex &&
                e1.startVertex != e2.stopVertex && e1.stopVertex != e2.stopVertex)
          case (e: EdgeLine, v: VertexLine) => !Set(e.startVertex, e.stopVertex).contains(v.v)
          case (v: VertexLine, e: EdgeLine) => !Set(e.startVertex, e.stopVertex).contains(v.v)
          case (v1: VertexLine, v2: VertexLine) => v1.v != v2.v
        }
      } else {
        false
      }
    }

    // Start by removing the rectangle (if any) and all neighboring edges from the grid
    val clearGrid = grid.remove(edgesToMove).remove(borderToMove)

    // Check if the border segments intersect any other rectangles or edges
    if (movedBorder.exists(l => clearGrid.intersects(l, intersects)))
      return None

    // Check if the edges intersect any other rectangles or edges
    if (movedEdges.exists(l => clearGrid.intersects(l, intersects)))
      return None

    // Finally, check if moving v causes it to become collinear with two or ore neighbors such that their edges overlap.
    if (CollinearOverlap.hasOverlap(to, neighbors.map(n => (n, coordinate(n)))))
      return None

    val finalGrid = clearGrid.add(movedBorder).add(movedEdges)
    val addedBorder = if (movedBorder.isEmpty) segments else segments.updateBorder(movedBorder(0), movedBorder(1), movedBorder(2), movedBorder(3))
    val addedEdges = addedBorder.updateEdge(movedEdges)

    Some(new VertexAdjustment(border, coordinate + (v -> to), graph, addedEdges, finalGrid, dimensions))
  }

  private def vertexEdges(v: Int): Vector[VertexLine] = {
    if (segments.hasRectangle(v)) {
      val ids = segments.rectangles(v)
      Vector(ids._1, ids._2, ids._3, ids._4).map(id => segments.segments(id).asInstanceOf[VertexLine])
    } else {
      Vector()
    }
  }

  private def neighborOverlapExists(v: Int, to: Point, neighbors: Vector[Int]): Boolean = {
    val dim = if (segments.hasRectangle(v)) dimensions(v) else Dimensions(1, 1)
    val movedRectangle = Rectangle.centerAround(RPoint(to), dim.width, dim.height)
    val ns = neighbors.iterator
    while (ns.hasNext) {
      val neighbor = ns.next()
      val ndim = if (segments.hasRectangle(neighbor)) dimensions(neighbor) else Dimensions(1, 1)
      val nr = Rectangle.centerAround(RPoint(coordinate(neighbor)), ndim.width, ndim.height)
      if (movedRectangle.overlaps(nr))
        return true
    }

    false
  }

}

object VertexAdjustment {

  /**
    * @param drawing A straight-line drawing.
    * @param graph The graph that the drawing was based on.
    * @param border How many coordinates along each axis (starting at 0) that vertices should be moved within.
    * @param vertexRectangles Dimensions for all vertex-rectangles. Any vertex that doesn't get an entry in this map
    *                         will be represented as single-coordinate endpoints.
    * @return An initial adjustment factory.
    */
  def apply(drawing: StraightLineDrawing[Int], graph: Graph[Int, UnDiEdge], border: Int, vertexRectangles: Map[Int, Dimensions] = Map()): VertexAdjustment = {
    build(border, drawing, graph, vertexRectangles)
  }

  private def build(border: Int,
                    drawing: StraightLineDrawing[Int],
                    graph: Graph[Int, UnDiEdge],
                    vertexRectangles: Map[Int, Dimensions]): VertexAdjustment = {
    require(border > 0, "Border size must be > 0.")
    require(graph.isConnected || !graph.nodes.exists(_.neighbors.isEmpty), "Every node in the input graph must be connected to a neighbor.")
    require(GraphCommons.outerVertices(graph).toSet == drawing.vertices.toSet, "The vertex set between the drawing and the graph didn't match.")

    /* Every edge is the diagonal of an enclosing rectangle beginning in (min x, min y). */
    val edges = makeEdges(drawing, vertexRectangles)

    // Make sure that the initial rectangle set adheres to the border limits
    val b = Rectangle.apply(RPoint(0, 0), Dimensions(border, border))
    for (e <- edges) {
      require(b.containsPoint(e.start) && b.containsPoint(e.stop),
        e match {
          case el: EdgeLine => s"The line between vertices ${el.startVertex} and ${el.stopVertex} is not confined within the specified border."
          case vl: VertexLine => s"The ${vl.side} rectangle segment representing vertex ${vl.v} lies outside the specified border."
        })
    }
    for (v <- drawing.coordinates.values)
      require(b.containsPoint(v), "Every vertex in the drawing must lie inside the specified border.")

    val segmentManager = IDSegmentManager(edges)
    val cells = 1000
    val coordinatesPerCell = Math.ceil(border.toDouble / cells).toInt
    val grid = ArrayGrid[IDLine](cells, coordinatesPerCell).add(edges)

    new VertexAdjustment(b, drawing.coordinates, graph, segmentManager, grid, vertexRectangles)
  }

  /* Produces edge- and border lines. */
  private def makeEdges(drawing: StraightLineDrawing[Int], vertexRectangles: Map[Int, Dimensions]): Vector[IDLine] = {
    var nextId = 0
    var result = Vector[IDLine]()
    val edges = drawing.edges.iterator
    while (edges.hasNext) {
      val next = edges.next()
      result = EdgeLine(nextId, next._1, next._2, RPoint(drawing.coordinates(next._1)), RPoint(drawing.coordinates(next._2))) +: result
      nextId += 1
    }

    val vs = drawing.vertices.toIterator
    while (vs.hasNext) {
      val v = vs.next()
      val dim = vertexRectangles.get(v)
      if (dim.isDefined && dim.get.width > 1 && dim.get.height > 1) {
        val rectangle = Rectangle.centerAround(RPoint(drawing.coordinates(v)), dim.get.width, dim.get.height)
        result = VertexLine(v, rectangle, nextId).reverse ++ result
        nextId += 4 // 4 vertex lines are added
      } else if (dim.isDefined && (dim.get.width > 1 || dim.get.height > 1)) {
        throw new Error("Cannot specify width or height 1 for vertex rectangles, as that causes their borders to intersect each other.")
      }
    }

    result.reverse // Since we're prepending
  }

}
