package net.cyndeline.rlgraph.triconnectivity

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * @param graph The graph that results from triconnecting another graph.
  * @param extraEdges Every edge that was added to triconnect the graph.
  */
case class GraphTriconnection[VType](graph: Graph[VType, UnDiEdge], extraVertices: Vector[VType], extraEdges: Vector[(VType, VType)]) {

}
