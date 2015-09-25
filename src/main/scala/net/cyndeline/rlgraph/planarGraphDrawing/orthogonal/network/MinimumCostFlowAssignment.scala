package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowVertex}

import scalax.collection.immutable.Graph

/**
 * Computes minimum cost flow using any algorithm.
 */
trait MinimumCostFlowAssignment[GraphType] {

  def computeFlow(network: GraphType): Graph[FlowVertex, FlowEdge]
}
