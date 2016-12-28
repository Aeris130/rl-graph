package net.cyndeline.rlgraph.drawings.planar.springEmbedding

import net.cyndeline.rlcommon.math.EuclidianNorm
import net.cyndeline.rlcommon.math.geom.{DPoint, Point}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.triconnectivity.subdivision2D.SubdivisionTriconnector
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.rlgraph.util.graphConverters.IntConverter

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Implementation of the planar force-directed algorithm by B.Plestenjak, described in
  * "An Algorithm for Drawing Planar Graphs". The algorithm is derived from the Fruchterman/Reingold algorithm,
  * and achieves planarity by discarding the repulsive forces of the original algorithm, and fixing the vertices
  * of an outer face. The remaining vertices are all placed in the center of the face, and is drawn outwards
  * until the forces reaches a state of planarity.
  *
  * Although any planar graph may be sent as input, the underlying algorithm requires a triconnected graph. Any edges
  * required to reach this state will be added during the algorithm, which may cause asymmetries in the final drawing.
  *
  * @param iterations The number of times that the algorithm should apply forces to the vertex set.
  */
class SpringEmbeddingAlgorithm(iterations: Int) {

  def computeDrawing(graph: Graph[Int, UnDiEdge]): StraightLineDrawing[Int] = {
    if (graph.isEmpty)
      return StraightLineDrawing.empty[Int]

    val graphAt0Conversion = IntConverter.convert(graph)
    val intToOriginal = graphAt0Conversion._2.zipWithIndex.map(_.swap).toMap
    val graph0 = graphAt0Conversion._1

    /* Step 1: Make sure the graph is triconnected before proceeding, using the subdivision algorithm. */
    var nextExtraVertex: Int = GraphCommons.outerVertices(graph0).max + 1
    def vertexFactory(): Int = {
      nextExtraVertex += 1
      nextExtraVertex - 1
    }
    var outerFace: Face[Int] = null
    def longestFace(fs: Vector[Face[Int]]) = {
      outerFace = fs.maxBy(_.vertexSize)
      outerFace
    }
    val triconnection = new SubdivisionTriconnector().triconnectGraphWithOuterFace(graph0, vertexFactory, longestFace)
    val triconnectedGraph = triconnection.graph
    val allEdges = GraphCommons.outerEdges(triconnectedGraph).map(e => (e._1, e._2))
    val innerVertices = GraphCommons.outerVertices(triconnectedGraph) diff outerFace.vertices

    /* Step 2: Compute constants needed by the algorithm. Variable names matches original article for readability. */
    val n = triconnectedGraph.nodes.size
    val center = DPoint(0, 0) // The initial position of all vertices except the ones on the outer face
    var coordinates = Vector.fill(n)(center)
    var force = Vector.fill(n)(0d)
    val averageSize = Math.PI / n
    val averageSizeSqrt = Math.sqrt(averageSize)
    val C = Math.sqrt(n / Math.PI)
    def cool(i: Int) = averageSizeSqrt / (1 + averageSize * Math.pow(i, 1.5))
    def f(u: Int, v: Int): Double = {
      val uc = coordinates(u)
      val vc = coordinates(v)
      val d = EuclidianNorm(Vector(uc.x - vc.x, uc.y - vc.y))
      C * Math.pow(d, 3)
    }
    def setForce(v: Int, f: Double) = {
      force = force.updated(v, f)
    }
    def nilForce() = {
      force = Vector.fill(n)(0d)
    }
    def setCoordinate(v: Int, p: DPoint) = {
      coordinates = coordinates.updated(v, p)
    }

    /* Step 3: Fix the outer face on the unit circle. The remaining vertices have already been placed at the centre.
     */
    val radius = 1
    val angleBetweenPoints = (Math.PI * 2) / outerFace.vertexSize
    for (i <- 0 until outerFace.vertexSize) yield {
      val x = center.x + radius * Math.cos(i * angleBetweenPoints)
      val y = center.y + radius * Math.sin(i * angleBetweenPoints)
      val p = DPoint(x, y)
      coordinates = coordinates.updated(outerFace.vertices(i), p)
    }

    /* Step 4: Run the algorithm for the specified number of iterations. */
    var i = 0
    while (i < iterations) {

      // a) For all vertices, set their resultant force to 0
      nilForce()

      /* b) For all edges (u, v), calculate the attracting force F(u,v) and update the resultant
       * forces F(u) and F(v).
       */
      val es = allEdges.iterator
      while (es.hasNext) {
        val edge = es.next()
        val u = edge._1
        val v = edge._2
        val Fuv = f(u, v)
        val fu = force(u) + Fuv
        val fv = force(v) - Fuv
        setForce(u, fu)
        setForce(v, fv)
      }

      /* c) For all vertices NOT on the outer face, move the vertex in the direction of the resultant force, but
       * not longer than the value of cool(i).
       */
      val inner = innerVertices.iterator
      val cv = cool(i)
      while (inner.hasNext) {
        val v = inner.next()
        val fv = force(v)
        val newPos = coordinates(v) + (Math.min(Math.abs(fv), cv) * (fv / Math.abs(fv)))
        setCoordinate(v, newPos)
      }

      i += 1
    } // End iterations

    /* Step 5: Compute the drawing using the current coordinate set. */
    val cMap = coordinates
      .zipWithIndex
      .flatMap((pAndIndex: (DPoint, Int)) => if (intToOriginal.contains(pAndIndex._2)) {
        Some(intToOriginal(pAndIndex._2) -> Point(pAndIndex._1))
      } else None)
      .toMap
    val drawing = StraightLineDrawing[Int](graph, cMap)

    drawing.cut
  }

}
