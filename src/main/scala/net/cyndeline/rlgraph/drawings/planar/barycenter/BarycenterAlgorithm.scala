package net.cyndeline.rlgraph.drawings.planar.barycenter

import net.cyndeline.rlcommon.collections.{SparseArray, SparseKeyArray}
import net.cyndeline.rlcommon.math.GaussSeidelSolver
import net.cyndeline.rlcommon.math.geom.{DPoint, Point}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.triangulation.naive.FaceTriangulationAlgorithm
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
  * Fixes an outer face of a graph as a convex polygon, then computes the barycentre positions of the inner
  * vertices using a series of linear equations using the Gauss-Seidel algorithm. This is the Tutte algorithm, and
  * thus it requires the graph to be triconnected. The user-supplied graph will be triangulated to achieve the
  * desired triconnectivity inside the outer face.
  *
  * @param outerFaceSelect Function that allows the user to specify which face that should be used as outer
  *                        (non-triangulated) in the final drawing. By default the longest face will be selected.
  */
class BarycenterAlgorithm(outerFaceSelect: Vector[Face[Int]] => Face[Int] = (vs: Vector[Face[Int]]) => vs.maxBy(_.vertexSize)) {
  private val solver = new GaussSeidelSolver(100)

  /**
    * @param g A planar drawing.
    * @return A barycenter drawing of the graph.
    */
  def computeDrawing(g: Graph[Int, UnDiEdge]): StraightLineDrawing[Int] = {
    val n = g.nodes.size

    // Special cases for graphs that the algorithm isn't specified for
    if (g.isEmpty)
      return StraightLineDrawing.empty[Int]
    else if (n == 1)
      return StraightLineDrawing.singleVertex(g.nodes.head)
    else if (n == 2)
      return doubleCase(g)

    val triconnection = triconnect(g)
    val outerFace = triconnection._1
    val extraEdges = triconnection._2
    val graphWithExtraEdges = g ++ extraEdges.map(e => e._1~e._2)
    val coordinates = new mutable.HashMap[Int, Point]()

    /* Embed the outer face on a circle with radius n/2, resulting in a convex polygon. */
    val center = DPoint(0, 0)
    val radius = n
    val angleBetweenPoints = (Math.PI * 2) / outerFace.vertexSize
    val scalar = 10000
    for (i <- 0 until outerFace.vertexSize) yield {
      val x = center.x + radius * Math.cos(i * angleBetweenPoints)
      val y = center.y + radius * Math.sin(i * angleBetweenPoints)
      val p = Point(Math.floor(x * scalar).toInt, Math.floor(y * scalar).toInt)
      coordinates.put(outerFace.vertices(i), p)
    }

    /* The vertices on the outer face aren't a part of the equation, so the inner vertices needs to be mapped
     * to 0->n again.
     */
    val outerSet = outerFace.vertices.toSet
    val innerVertices: Vector[(Int, Int)] = GraphCommons.outerVertices(g).filterNot(outerSet.contains).zipWithIndex // (Original, Inner)
    val innerToOriginalMap = innerVertices.map(_.swap).toMap
    val originalToInnerMap = innerVertices.toMap

    val xCoordinates = finalCoordinates(innerVertices, graphWithExtraEdges, originalToInnerMap, coordinates, x = true)
    val yCoordinates = finalCoordinates(innerVertices, graphWithExtraEdges, originalToInnerMap, coordinates, x = false)

    var inner = 0
    val in = innerVertices.size
    while (inner < in) {
      val point = Point(xCoordinates(inner), yCoordinates(inner))
      coordinates.put(innerToOriginalMap(inner), point)
      inner += 1
    }

    StraightLineDrawing(g, coordinates.toMap).cut
  }

  /**
    * @param innerVertices A vector of tuples where the first value is the original vertex, and the second is the
    *                      vertex-value that was assigned to the original when taking the inner vertices and
    *                      mapping them to 0 ... n.
    * @param graph The final graph containing both inner and outer vertices, as well as all edges that were required
    *              to make the graph triconnected (if any).
    * @param originalToInnerMap Maps original vertices to their inner counterpart.
    * @param x True if the x-coordinates should be computed, false if y-coordinates.
    * @param outerCoordinates Contains the coordinates of all vertices on the outer face.
    * @return Coordinate values for the inner vertices, where each vertex coordinate is stored at the index
    *         corresponding to the inner vertex value.
    */
  private def finalCoordinates(innerVertices: Vector[(Int, Int)],
                               graph: Graph[Int, UnDiEdge],
                               originalToInnerMap: Map[Int, Int],
                               outerCoordinates: mutable.HashMap[Int, Point],
                               x: Boolean): Vector[Int] = {
    val n = innerVertices.size

    /* Since most values in the matrix M(i,j) will be set to 0 (no edge between i and j), a sparse matrix
     * is used to save space. Every index (i,x) represents the equation x for the inner vertex i.
     */
    val default = new SparseKeyArray[Int](n, 0)
    val matrix = new SparseKeyArray[SparseArray[Int]](n, default)
    val rightHandSideConstants = Array.fill(n)(0d)

    /* For every inner vertex i, create a new sparse array for its equation and update the diagonal with its degree,
     * and set (i,j) to -1 for every neighbor j.
     */
    var inner = 0
    while (inner < n) {
      val originalInner = innerVertices(inner)
      val original = originalInner._1
      val degree = graph.get(original).degree
      val equation = new SparseKeyArray[Int](n, 0)
      val neighbors = GraphCommons.outerNeighbors(original, graph)
      val innerNeighbors = neighbors
        .flatMap(n => if (originalToInnerMap.contains(n)) Some(originalToInnerMap(n)) else None) // Skips the outer vertices

      equation.set(inner, degree)
      for (n <- innerNeighbors)
        equation.set(n, -1)

      matrix.set(inner, equation)

      /* The constants on the right-hand-side of each equation should be 0, except for the vertices that connect to
       * vertices on the outer face. For them, the sum of coordinate values for all neighbors on the outer face it is
       * connected to should be used instead. For every inner vertex, set its parameter to 0 unless it has outer
       * neighbors.
       */
      val outerNeighborSum = neighbors.flatMap(n => Option(outerCoordinates.getOrElse(n, null))).map(dp => {
        if (x) dp.x else dp.y
      }).sum
      rightHandSideConstants(inner) = outerNeighborSum

      inner += 1
    }

    solver.solve(matrix, rightHandSideConstants).map(Math.floor(_).toInt)
  }

  /**
    * Returns the edges that were added to make all faces (but the outer) triangular, as well as which face
    * that was selected as outer (the longest).
    */
  private def triconnect(g: Graph[Int, UnDiEdge]): (Face[Int], Vector[(Int, Int)]) = {
    var selected: Face[Int] = null
    def selectOuter(fs: Vector[Face[Int]]): Face[Int] = {
      val f = fs.maxBy(_.vertexSize)
      selected = f
      f
    }
    val triAlg = new FaceTriangulationAlgorithm(selectOuter)
    val edges = triAlg.triangulate(g)
    (selected, edges)
  }

  private def doubleCase(g: Graph[Int, UnDiEdge]) = {
    val vertices = GraphCommons.outerVertices(g)
    StraightLineDrawing[Int](g, Map(vertices(0) -> Point(0, 0), vertices(1) -> Point(1, 0)))
  }

}
