package net.cyndeline.rlgraph.triangulation.naive

import java.util

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

/**
  * Triangulates a planar graph without adhering to constraints on vertex degrees or connectivity. The algorithm
  * runs in O(n) time, and proceeds as follows:
  *
  *  1. Embed the graph, add edges to make it biconnected and compute all faces. For each face F in the resulting graph
  *  with more than 3 vertices:
  *   a. Select the vertex v with the lowest degree
  *   a. Draw an edge from v to every other vertex w in the face except for v's neighbors (w(i-1), w(i+1)).
  *   a. F is now triangular.
  *    a. If no more faces exists with degree > 3, exit.
  *    a. If an edge between v and a vertex w(i) already exists outside the face (with i =/= (i-1) and (i+1)), discard
  * the edges and instead use step 2 for this face.
  *  1. Given a neighbor w(i) that shares an edge with v outside F, split the vertex sequence into two between v and
  * w(i). Example: F=[1,2,3,4,5,6,7,8,9,10,1], v = 1, w(i) = 6. The splits S1 and S2 will be [2,3,4,5] and [7,8,9,10].
  *   a. Add edges between the first vertex of S1 and all vertices of S2, then add edges between the last vertex of
  * S2 and all vertices of S1.
  *
  * Regarding the correctness of part 2: Since an outer edge exists between v and w(i), none of the vertices in
  * either split may contain an outer edge to the other split (doing so would cause that edge to intersect the edge
  * [v, w(i)]. As such, edges can freely be added between the splits without taking potential multi-edges into
  * consideration.
  *
  * @param selectOuter If defined, selects a face to skip triangulation for.
  */
class FaceTriangulationAlgorithm private (selectOuter: Option[Vector[Face[Int]] => Face[Int]]) {

  def this() = this(None)
  def this(outer: Vector[Face[Int]] => Face[Int]) = this(Some(outer))

  /**
    * @param graph A planar graph.
    * @return Every edge that must be added to the graph to make it biconnected.
    */
  def triangulate(graph: Graph[Int, UnDiEdge]): Vector[(Int, Int)] = {
    if (graph.nodes.size < 2)
      return Vector()

    val biconnection = new BiconnectivityOperation[Int]().biconnect(graph)
    val faces = new FaceComputation[Int]().computeFacesFromGraph(biconnection.graph)
    val outer = Option(if (selectOuter.isDefined) selectOuter.get(faces) else null)
    val result = new ArrayBuffer[(Int, Int)]()

    var currentGraph = biconnection.graph
    val fs = faces.iterator
    while (fs.hasNext) {
      val face = fs.next()
      if (outer.isEmpty || outer.get != face) {
        val edges = computeEdges(face, currentGraph)
        currentGraph ++= edges.map(e => e._1~e._2)
        result ++= edges
      }
    }

    result.toVector ++ biconnection.extraEdges
  }

  /**
    * @param face A biconnected face f with no repeating vertices.
    * @param graph The graph that generated f.
    * @return Every edge that must be added inside f to make it triangular.
    */
  private def computeEdges(face: Face[Int], graph: Graph[Int, UnDiEdge]): Vector[(Int, Int)] = if (face.vertexSize > 3) {
    val n = face.vertexSize
    val startVertex = face.vertices.minBy(graph.get(_).degree)
    val startNode = graph.get(startVertex)
    val vertices = GraphCommons.placeElementFirst(startVertex, face.vertices)
    val result = new ArrayBuffer[(Int, Int)]()

    var i = 2 // Skip the start vertex and its first neighbor
    while (i < (n - 1) /* Skip the last neighbor of the start vertex as well */) {
      val vertex = vertices(i)
      if (graph.get(vertex).neighbors.contains(startNode)) {
        return handleSplits(vertices, startVertex, i)
      } else {
        result += ((startVertex, vertex))
      }
      i += 1
    }

    result.toVector
  } else {
    Vector()
  }

  /**
    * @param vertices Every vertex in the face, with the start vertex as head.
    * @param startVertex Start vertex, will not have edges connected to it.
    * @param splitIndex The index in the vertex vector that contains an outer edge to the start vertex.
    *                   Will not have edges connected to it.
    */
  private def handleSplits(vertices: Vector[Int], startVertex: Int, splitIndex: Int): Vector[(Int, Int)] = {
    val splits = vertices.splitAt(splitIndex)
    val s1 = splits._1.drop(1)
    val s2 = splits._2.drop(1)

    // Add an edge from the first vertex in S1 to all vertices in S2
    val s1ToS2 = s2.map((s1.head, _))

    // Add an edge from the last verte in s2 to every vertex in s1, except the first, since that edge has been added
    val last = s2.last
    val s2ToS1 = s1.drop(1).map((last, _))

    s1ToS2 ++ s2ToS1
  }

}
