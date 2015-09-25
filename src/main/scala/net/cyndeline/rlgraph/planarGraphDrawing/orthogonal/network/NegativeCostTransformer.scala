package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex, FlowVertexReplacer}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Removes negative costs from a network by performing the following modifications: The cost of each directed edge e
 * is made positive by multiplying it by -1 and reverses its to/from vertices, pointing it in the opposite direction.
 * The vertex previously having the edge as outgoing has its production p set to (p - maximum capacity of the edge).
 * The vertex previously having the edge as incoming has its production p set to (p + maximum capacity of the edge).
 *
 * If a vertex has multiple negative edges connected to it, the production is modified for each edge.
 *
 * @constructor Creates a new negative cost transformer object.
 */
class NegativeCostTransformer {

  /**
   * Transforms a directed flow network by making negative costs positive.
   *
   * @param network A network with negative costs.
   * @return a new network with no negative costs, and vertex productions modified to reflect this. Any edge that is
   *         reversed this way retains its original ID.
   */
  def removeNegativeCosts(network: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = {
    implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
    var transformedNetwork = network
    val edges = transformedNetwork.edges.toOuter.toSet[FlowEdge[FlowVertex]]

    for (edge: FlowEdge[FlowVertex] <- edges) {
      if (edge.cost < 0) {
        val positiveCost = edge.cost * -1

        /* newProduction can't be called upon the vertices in the edge, since they may have been updated in the
         * graph by a previous edge. The edge and its vertices has to be fetched again, and any references to
         * From and To from here on out must be to the updated versions.
         */
        val from = transformedNetwork.get(edge).toOuter.from
        val to = transformedNetwork.get(edge).toOuter.to
        val newFromProduction = from.newProduction(from.production - edge.capacity)
        val newToProduction = to.newProduction(to.production + edge.capacity)

        // To and From reversed
        val transformedEdge = newToProduction ~> newFromProduction ## (edge.id, edge.lowerBound, edge.capacity, positiveCost, edge.flow, edge.crosses)

        transformedNetwork -= edge

        transformedNetwork = FlowVertexReplacer.replace(transformedNetwork, from, newFromProduction)
        transformedNetwork = FlowVertexReplacer.replace(transformedNetwork, to, newToProduction)

        transformedNetwork += transformedEdge
      }
    }

    transformedNetwork
  }
}
