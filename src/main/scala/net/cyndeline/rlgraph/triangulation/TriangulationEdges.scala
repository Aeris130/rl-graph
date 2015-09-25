package net.cyndeline.rlgraph.triangulation

import net.cyndeline.rlgraph.face.Face

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * @param faceMap Every biconnected face in the graph that was triangulated, mapped to the edges used to triangulate it.
 * @param biconnectivityEdges All edges added to biconnect the graph, if the input graph was not already biconnected.
 */
case class TriangulationEdges[VType](faceMap: Map[Face[VType], Vector[(VType, VType)]], biconnectivityEdges: Vector[(VType, VType)]) {

  /** The edges need to be added to a planar graph in order to triangulate it. */
  val triangulatingEdges: Vector[(VType, VType)] = faceMap.values.toVector.flatten

  /** All edges added to the graph during the triangulation process. */
  val edges: Vector[(VType, VType)] = triangulatingEdges ++ biconnectivityEdges

  /**
   * Adds every edge in this triangulation to a graph.
   * @param graph Graph to add edges to.
   * @return The input graph with triangulation edges added to it.
   */
  def addToGraph(graph: Graph[VType, UnDiEdge]): Graph[VType, UnDiEdge] = {
    var current = graph
    for (edge <- edges)
      current += edge._1~edge._2

    current
  }

  /**
   * Removes the edges used to triangulate a specified face.
   * @param f Face to remove triangulation edges from.
   * @return A copy of this triangulation with the triangulation belonging to the specified face removed.
   */
  def removeFaceTriangulation(f: Face[VType]): TriangulationEdges[VType] = TriangulationEdges(faceMap - f, biconnectivityEdges)

  override def toString: String = {
    val builder = new StringBuilder()
    val sep = System.getProperty("line.separator")
    builder ++= "Biconnected edges: " + biconnectivityEdges.mkString(", ") + sep

    for (f <- faceMap) {
      builder ++= "// Face: " + f._1 + sep
      builder ++= "   Edges: " + f._2.mkString(", ") + sep + sep
    }

    builder.toString()
  }
}
