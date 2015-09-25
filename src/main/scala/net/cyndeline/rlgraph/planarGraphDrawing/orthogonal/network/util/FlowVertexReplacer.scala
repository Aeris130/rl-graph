package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Replaces a vertex in a graph by storing all edges connected to it, and inserting another vertex in
 * its place with the previous edges using the new vertex instead.
 */
object FlowVertexReplacer {

  /**
   * Creates a new graph with a vertex replaced using a copy.
   *
   * @param graph Graph to replace vertex in.
   * @param oldVertex Old vertex to replace.
   * @param newVertex Vertex to replace it with.
   * @return a new graph with the old vertex replaced by the new.
   */
  def replace(graph: Graph[FlowVertex, FlowEdge],
              oldVertex: FlowVertex,
              newVertex: FlowVertex
              ): Graph[FlowVertex, FlowEdge] = {
    var newGraph: Graph[FlowVertex, FlowEdge] = graph

    val incoming = graph.get(oldVertex).incoming
    val savedIncoming = new ArrayBuffer[FlowEdge[FlowVertex]]
    for (edge <- incoming) {
      savedIncoming += edge.toOuter
      newGraph -= edge
    }

    val outgoing = graph.get(oldVertex).outgoing
    val savedOutgoing = new ArrayBuffer[FlowEdge[FlowVertex]]
    for (edge <- outgoing) {
      savedOutgoing += edge.toOuter
      newGraph -= edge
    }

    newGraph -= oldVertex
    newGraph += newVertex

    for (edge <- savedIncoming) {
      newGraph += copyEdge(edge.from, newVertex, edge)
    }

    for (edge <- savedOutgoing) {
      newGraph += copyEdge(newVertex, edge.to, edge)
    }

    newGraph
  }

  private def copyEdge(from: FlowVertex, to: FlowVertex, edge: FlowEdge[FlowVertex]): FlowEdge[FlowVertex] with OuterEdge[FlowVertex, FlowEdge] = {
    implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
    from ~> to ## (edge.id, edge.lowerBound, edge.capacity, edge.cost, edge.flow, edge.crosses)
  }
}
