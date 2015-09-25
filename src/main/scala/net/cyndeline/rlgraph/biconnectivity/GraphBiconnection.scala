package net.cyndeline.rlgraph.biconnectivity

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * @param graph The graph that results from biconnecting another graph.
 * @param extraEdges Every edge that was added to biconnect the graph.
 */
case class GraphBiconnection[VType](graph: Graph[VType, UnDiEdge], extraEdges: Vector[(VType, VType)])
