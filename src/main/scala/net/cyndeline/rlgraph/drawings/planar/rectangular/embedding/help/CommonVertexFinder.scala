package net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.help

import net.cyndeline.rlgraph.util.GraphCommons

import scala.reflect.ClassTag
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Finds and enumerates common vertices connected to edges in a graph. A vertex C is considered common to an edge A~B
 * if both A and B connects to C using an edge, forming a triangular face (A, B, C).
 */
class CommonVertexFinder {

  /**
   * @param edge An undirected edge between two distinct vertices.
   * @param graph The graph that the edge belongs to.
   * @tparam VType Vertex type in the graph.
   * @return A list of all common vertices connected to the edge, or an empty list if no such vertices exist.
   */
  def findCommonVertices[VType : ClassTag, EType[X] <: UnDiEdge[X]](edge: EType[VType], graph: Graph[VType, EType]): Vector[VType] = {
    val startVertex = edge._1
    val stopVertex = edge._2

    /* For every edge connected to one of the vertices in the specified edge, check if the opposite vertex
     * has an edge that points back to the other vertex. Using a while loop since scalas for-loop somehow implodes if
     * a vertex is implicitly converted inside the loop.
     */
    val edges = graph.get(startVertex).edges.iterator
    var commonVertices: Vector[VType] = Vector()
    while (edges.hasNext) {
      val edge = edges.next()
      val oppositeVertex: VType = GraphCommons.oppositeVertex(startVertex, edge.toOuter)

      if (oppositeVertex != stopVertex && graph.get(oppositeVertex).edges.exists(e => e._1 == stopVertex || e._2 == stopVertex)) {
        commonVertices = oppositeVertex +: commonVertices
      }
    }

    commonVertices
  }

  /**
   * @param graph A graph.
   * @tparam VType Vertex type in the graph.
   * @return The total number of common vertices in the graph.
   */
  def numberOfCommonVertices[VType : ClassTag, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]): Int = {
    graph.edges.toVector.map(e => findCommonVertices(e.toOuter, graph).size).sum
  }

}
