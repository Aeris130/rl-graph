package net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlgraph.drawings.{RVertex, StraightLineDrawing}
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid.{GridRectangle, VertexGrid}
import net.cyndeline.rlgraph.util.GraphCommons
import spire.math.{Algebraic, Rational}

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Implements a modified version of the force-directed algorithm used to draw graphs, as described by in the paper
  * "Graph Drawing by Force-directed Placement" by Fruchterman and Reingold. The basic concept is an algorithm that
  * iterates n number of times, and applies a repulsing force between every pair of vertices, and an attracting force
  * between any neighbors connected by an edge. The force applied becomes stronger the closer the vertices are in the
  * case of repulsing forces, and vice versa for attracting forces.
  *
  * Computing the repulsing forces for every vertex pair has runtime O(n2), so to speed up the algorithm a grid
  * representing the canvas is used. When looking for non-connected neighbors in the drawing, only the vertices present
  * within a certain distance of the current vertex is considered. Out of these, only the ones present within a circular
  * circumference of the vertex are used when computing repulsing forces.
  *
  * The distance that each vertex is moved in each iteration is controlled by the temperature of the algorithm.
  * For every iteration, the temperature decreases, leading to smaller (i.e finer) adjustments in the drawing.
  *
  * The algorithm has been modified to support vertices represented as rectangular areas rather than just single
  * coordinates. These modifications can be applied using the SizeDisplacement class as force input, other than
  * that it is worth noting that the area around a vertex that gets checked for non-connected neighbors are expanded to
  * include the vertex rectangular area. Using rectangle of width and height 1 gives the same result as the original
  * algorithm with single-coordinate vertices.
  *
  * @param grid Speeds up the algorithm by only letting each vertex retrieve neighbors within a set amount of
  *             grid cells, reducing runtime to O(n).
  * @param graph The graph that the drawing is based upon.
  * @param neighborRange The circumference length that should be used when searching for adjacent vertices that
  *                      exhibits repulsing forces. This value will be roughly equal to the grids cell sizes (2k).
  * @param temperature Constant that adjusts how fine the adjustment should be in each iteration of the algorithm.
  * @param currentIteration The number of iterations that has completed on this drawing.
  * @param maxIterations The maximum amount of iteration to perform before the algorithm is considered finished.
  * @param force User-supplied object controlling how far vertices moved when affected by attracting and repulsing
  *              forces.
  */
class FRDrawing private (grid: VertexGrid,
                         graph: Graph[Int, UnDiEdge],
                         neighborRange: Int,
                         val temperature: Rational,
                         val currentIteration: Int,
                         val maxIterations: Int,
                         force: Force) {
  require(!grid.isEmpty, "Cannot instantiate a drawing without vertices")
  private val epsilon: Double = 1e-3 // Prevents the delta divisor from becoming 0

  def width: Int = grid.width
  def height: Int = grid.height

  /**
    * @return True if no more iterations should be ran on this algorithm, otherwise false.
    */
  def isFinished: Boolean = currentIteration == maxIterations

  /**
    * @return Every vertex in the drawing mapped against its rectangle, computed in O(n) time.
    */
  def computeDrawing: StraightLineDrawing[RVertex] = {
    val allVertices: Vector[GridRectangle] = grid.allVertices.distinct
    val rVertices: Vector[RVertex] = allVertices.map(r => RVertex(r.id, Rectangle(r.start, r.width.toInt, r.height.toInt)))
    val vertexToRVertex: Map[Int, RVertex] = rVertices.map(v => v.vertex -> v).toMap
    val rVertexToPoint = allVertices.map(v => vertexToRVertex(v.id) -> Point(v.center)).toMap
    val edges = GraphCommons.outerEdges(graph).map(e => (vertexToRVertex(e._1), vertexToRVertex(e._2)))
    new StraightLineDrawing[RVertex](rVertices, edges, rVertexToPoint, width, height)
  }

  /**
    * @return A drawing with all iterations applied to it.
    */
  def iterateAll: FRDrawing = iterate(maxIterations)

  /**
    * Use this to apply n number of iterations. useful when visualizing or debugging each step of the algorithm.
    * @param iterations The number of times to apply the forces to the current drawing.
    * @return A drawing where the repulsing and attracting forces has been applied the specified number of times, or the
    *         amount allowed before the maximum amount of iterations were reached. Each iteration will decrease the
    *         algorithms temperature.
    */
  def iterate(iterations: Int = 1): FRDrawing = {
    require(iterations >= 0, "Iteration amount must be 0 or greater.")
    if (iterations == 0 || currentIteration == maxIterations)
      return this

    val highestId = grid.vertices.keys.max
    var xDisplacement = Vector.fill(highestId + 1)(Rational.zero) // assumes that vertex id's are continuous from 0 to highestId
    var yDisplacement = Vector.fill(highestId + 1)(Rational.zero)

    /* Compute repulsing forces */
    var allVertices = grid.vertices.toIterator

    /* Every fifth iteration the repulsing forces are disabled in order to let the attracting forces adjust the
     * layout without them interfering. This has nothing to do with the correctness of the algorithm, but is an attempt
     * to escape local minima in order to improve the visual qualities of the layout.
     */
    if (currentIteration == 0 || currentIteration % 5 != 0) {
      while (allVertices.hasNext) {
        val entry = allVertices.next()
        val rectangle = entry._2
        val xyRep = computeRepulsion(rectangle)
        xDisplacement = xDisplacement.updated(rectangle.id, xDisplacement(rectangle.id) + xyRep._1)
        yDisplacement = yDisplacement.updated(rectangle.id, yDisplacement(rectangle.id) + xyRep._2)
      }
    }

    /* Compute attracting forces */
    val allEdges = graph.edges.toIterator
    while (allEdges.hasNext) {
      val edge = allEdges.next()
      val v1 = edge._1
      val v2 = edge._2
      val xyAttr: ((Rational, Rational), (Rational, Rational)) = computeAttraction(edge.toOuter)
      val v1XYAttraction = xyAttr._1
      val v2XYAttraction = xyAttr._2

      // V1
      xDisplacement = xDisplacement.updated(v1, xDisplacement(v1) - v1XYAttraction._1)
      yDisplacement = yDisplacement.updated(v1, yDisplacement(v1) - v1XYAttraction._2)

      // V2
      xDisplacement = xDisplacement.updated(v2, xDisplacement(v2) + v2XYAttraction._1)
      yDisplacement = yDisplacement.updated(v2, yDisplacement(v2) + v2XYAttraction._2)
    }

    /* Apply forces to each vertex, and limit the displacement based on temperature and frame dimensions */
    var updatedGrid = grid
    allVertices = grid.vertices.toIterator
    while (allVertices.hasNext) {
      val entry = allVertices.next()
      val rectangle = entry._2
      val dx = xDisplacement(rectangle.id)
      val dy = yDisplacement(rectangle.id)
      val delta = Algebraic(dx * dx + dy * dy).sqrt.toDouble

      // No point applying forces unless they actually move the vertex.
      if (delta - epsilon > 0) {
        val initXDispl = (dx / delta) * Math.min(delta, temperature.toDouble)
        val initYDispl = (dy / delta) * Math.min(delta, temperature.toDouble)

        /* Rounding back to grid coordinates takes place here. The algorithm attempts to avoid 0 displacement, as
         * overlapping vertices otherwise may not be moved.
         */
        val xDisp = if (initXDispl <= 0) initXDispl.floor.toInt else initXDispl.ceil.toInt
        val yDisp = if (initYDispl <= 0) initYDispl.floor.toInt else initYDispl.ceil.toInt

        val xDispWithinFrame = if (rectangle.start.x + xDisp < 0)
            -rectangle.start.x
          else if (rectangle.stopCoordinate.x + xDisp > grid.width)
            Rational(grid.width) - rectangle.stopCoordinate.x
          else
            Rational(xDisp)

        val yDispWithinFrame = if (rectangle.start.y + yDisp < 0)
            -rectangle.start.y
          else if (rectangle.stopCoordinate.y + yDisp > grid.height)
            Rational(grid.height) - rectangle.stopCoordinate.y
          else
            Rational(yDisp)

        updatedGrid = updatedGrid.move(rectangle, xDispWithinFrame.toInt, yDispWithinFrame.toInt)

      }
    }

    val newTemperature = force.cool(temperature, currentIteration, maxIterations)
    val nextDrawing = new FRDrawing(updatedGrid, graph, neighborRange, newTemperature, currentIteration + 1, maxIterations, force)
    nextDrawing.iterate(iterations - 1)
  }


  /**
    * Computes the repulsive forces affecting a single vertex using all vertices within range of it.
    * @param v Vertex to compute repulsion forces on.
    * @return The final (x, y) disposition values that should be applied to the vertex position coordinate.
    */
  private def computeRepulsion(v: GridRectangle): (Rational, Rational) = {
    val adjacent = adjacentVertices(v)

    if (adjacent.isEmpty) {
      (0, 0)

    } else {
      var displacement: (Rational, Rational) = (0, 0)
      for (adj <- adjacent if adj != v) {

        /* If the points overlap the delta will be 0, causing the displacement to also be 0 despite that both
         * vertices should repulse each other. To work around this, one rectangles center will be slightly adjusted
         * to give the both a direction.
         */
        val adjustedCenters = if (v.center == adj.center) force.overlappingCenter(v, adj) else (v.center, adj.center)
        val delta: RPoint = adjustedCenters._1 - adjustedCenters._2 // v.center - adj.center

        /* The diagonal of the rectangle between v's old point and the adjacent one using Pythagora's theorem, giving
         * an absolute value.
         */
        val deltaLength = Math.max(epsilon, Algebraic(delta.x * delta.x + delta.y * delta.y).sqrt.toDouble)

        /* In the original algorithm: (Delta / |Delta|) / f_r(|Delta|) */
        val xFinalDisplacement = (delta.x / deltaLength) * force.computeRepulsingForce(v, adj)
        val yFinalDisplacement = (delta.y / deltaLength) * force.computeRepulsingForce(v, adj)

        displacement = (displacement._1 + xFinalDisplacement, displacement._2 + yFinalDisplacement)
      }

      (displacement._1, displacement._2)
    }
  }

  /**
    * Computes the attracting forces affecting a single vertex using all neighbours connected to its edges.
    * @param edge Edge whose nodes should have their attraction computed.
    * @return Two x/y tuples containing the vertex displacement, where the first tuple belongs to the first vertex
    *         in the edge and vice versa.
    */
  private def computeAttraction(edge: UnDiEdge[Int]): ((Rational, Rational), (Rational, Rational)) = {
    val v1 = grid.vertices(edge._1)
    val v2 = grid.vertices(edge._2)
    val delta = v1.center - v2.center
    val deltaLength = Math.max(epsilon, Algebraic(delta.x * delta.x + delta.y * delta.y).sqrt.toDouble)

    // X/Y displacement
    val v1Displacement: (Rational, Rational) = attractiveDisplacement(delta, deltaLength, v1, v2)
    val v2Displacement: (Rational, Rational) = attractiveDisplacement(delta, deltaLength, v2, v1)

    (v1Displacement, v2Displacement)
  }

  private def attractiveDisplacement(delta: RPoint, deltaLength: Double, v: GridRectangle, neighbor: GridRectangle): (Rational, Rational) = {
    val xDisplacement = (delta.x / deltaLength) * force.computeAttractingForce(v, neighbor)
    val yDisplacement = (delta.y / deltaLength) * force.computeAttractingForce(v, neighbor)
    (xDisplacement, yDisplacement)
  }

  /**
    * Retrieve every vertex rectangle that lies within 2k (where k ir roughly equal to the grids cell size on each axis)
    * distance of the edges belonging to a rectangle. Out of these candidates, the ones that lie within the
    * circumference of an ellipsis are selected in order to avoid having the layout skewed by the orthogonal nature
    * of the rectangle. The radius used when computing the ellipsis is (2k + x/2), where x is the width or height of the
    * rectangle (depending on which radius is being computed).
    */
  private def adjacentVertices(v: GridRectangle): Vector[GridRectangle] = {
    val startX = (v.start.x - neighborRange).max(0)
    val startY = (v.start.y - neighborRange).max(0)
    val stopX = (v.stopCoordinate.x + neighborRange).min(grid.width)
    val stopY = (v.stopCoordinate.y + neighborRange).min(grid.height)

    val adjacent = grid.allVertices(Point(startX.toInt, startY.toInt), Point(stopX.toInt, stopY.toInt))
    adjacent.filter(adj => isWithinEllipse(adj, v))
  }

  /**
    * Checks if a rectangle overlaps the circle generated by a vertex and its rectangle.
    * @param candidate Rectangle to check intersection/overlap for.
    * @param c The vertex whose center and rectangle will be the basis for the circle.
    * @return True if the rectangle intersects or overlaps the circle generated by the other rectangle, otherwise
    *         false.
    */
  private def isWithinEllipse(candidate: GridRectangle, c: GridRectangle): Boolean = {

    /* In the original algorithm each vertex was represented by a single coordinate, thus it was enough to use
     * a circle with radius 2k. Since this algorithm uses rectangular shapes however, the radius must be half
     * the diagonal of the rectangle + 2k to ensure that the rectangle receives repulsing forces from all directions
     * around it.
     */
    val radiusCover = Rational(0.5 * Algebraic(c.width * c.width + c.height * c.height).sqrt.toDouble + neighborRange)
    val ellipse = new Ellipse(c.center, radiusCover, radiusCover)

    ellipse.intersectsRectangle(candidate.start, candidate.stopCoordinate) match {
      case Intersects | Inside => true
      case _ => false
    }
  }

}

object FRDrawing {

  /**
    *
    * @param v A vertex in a graph.
    * @param p The coordinate in the drawing occupied by the vertex.
    * @return Rectangle data of width and height 1.
    */
  def singleCoordinateVertex(v: Int, p: Point) = (v, Rectangle(p, 1, 1))

  /**
    * Creates a drawing in its initial state, with no iterations of the algorithm applied to it.
    * @param graph Graph to draw.
    * @param width Number of coordinates along the drawings x axis to limit the drawing to.
    * @param height Number of coordinates along the drawings y axis to limit the drawing to.
    * @param rectangles Size and starting coordinates for every rectangle in the drawing. Every rectangle must reside
    *                   within the drawings boundaries (where the stop coordinate is exclusive), but may overlap each
    *                   other.
    * @param settings Parameters for the layout.
    * @return A drawing that hasn't been optimized with regards to the algorithm.
    */
  def apply(graph: Graph[Int, UnDiEdge],
            width: Int,
            height: Int,
            rectangles: Vector[(Int, Rectangle)],
            settings: FRSettings): FRDrawing = {
    require(width > 0 && height > 0, "Width ad height must be > 0.")
    require(!rectangles.exists(r => r._2.width < 1 || r._2.height < 1), "Rectangle height and width must be >= 1.")
    validateData(width, height, rectangles) // Once before the rectangles are adjusted..

    // used to compute grid cell size, see original article
    val area = (width * height).toDouble / rectangles.length
    val k = Math.sqrt(area)
    val shortestSide = Math.min(width, height)
    val neighborRange = Math.ceil(2 * k).toInt
    val gridCellSize = Math.min(shortestSide, neighborRange)
    var grid = VertexGrid(width, height, gridCellSize)
    val temperature: Double = width.toDouble / 10

    for (r <- rectangles) {
      val addedRectangleAndGrid = grid.create(r._1, Point(r._2.start), r._2.width.intValue(), r._2.height.intValue())
      grid = addedRectangleAndGrid._2
    }

    new FRDrawing(grid, graph, neighborRange, temperature, 0, settings.maxIterations, settings.forceComputation)
  }

  private def validateData(width: Int, height: Int, rectangles: Vector[(Int, Rectangle)]): Unit = {
    require(width >= 1 && height >= 1, "The specified size of the force-directed drawing must be equal or greater than 1 on each axis.")

    val tooLargeRectangle = rectangles.find(r => r._2.width > width || r._2.height > height)
    require(tooLargeRectangle.isEmpty, "The rectangle " + tooLargeRectangle.get + " has height or width that exceeds the drawings size")

    val outsideUpperBounds = rectangles.find(r => r._2.start.x < 0 || r._2.start.y < 0)
    require(outsideUpperBounds.isEmpty, "The rectangle " + outsideUpperBounds.get + " begins outside the drawing.")

    val outsideLowerBounds = rectangles.find(vAndR => {
      val r = vAndR._2
      val stop = RPoint(r.start.x + r.width, r.start.y + r.height)
      stop.x > width || stop.y > height
    })
    require(outsideLowerBounds.isEmpty, "The rectangle " + outsideLowerBounds.get + " ends outside the drawing.")
  }
}
