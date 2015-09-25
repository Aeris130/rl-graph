package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.MaxFlowMinCost

import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowVertex, _}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Computes the maximum flow for a network using the Ford-Fulkerson algorithm and residual network paths.
 * Does not take lower bounds into account by itself.
 *
 * @constructor Constructs a new maximum flow computation.
 */
class FordFulkersonMaximumFlow {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

  /**
   * Augments a network flow until it reaches its maximum.
   *
   * @param network A directed flow network.
   */
  def computeMaximumFlow(network: Graph[FlowVertex, FlowEdge], source: FlowVertex, sink: FlowVertex): Graph[FlowVertex, FlowEdge] = {

    if (network.nodes.filter(n => n.production > 0).isEmpty)
      throw new IllegalArgumentException("Network " + network + " lacks source.")

    if (network.nodes.filter(n => n.production < 0).isEmpty)
      throw new IllegalArgumentException("Network " + network + " lacks sink.")

    if (!network.isConnected)
      throw new IllegalArgumentException("Network " + network + " is not connected.")

    val residualNetwork = new ResidualNetwork(network, source, sink)
    var aPath: Option[Path[Int, ResidualEdge]] = residualNetwork.augmentingPath

    /* Can't pump more flow into the network than what the super-source can produce,
     * or the sink can consume. Begin by setting production and consumption based on
     * the flow that's already present in outgoing edges from the source.
     */
    var currentProduction = 0
    network.get(source).outgoing.foreach(currentProduction += _.flow)
    var totalProduction = source.production - currentProduction
    var totalConsumption = sink.production + currentProduction

    while (aPath.isDefined && totalProduction > 0 && totalConsumption < 0) {
      val flowToAugment: Int = Vector(lowestFlowCapacity(aPath.get.edges), totalProduction, Math.abs(totalConsumption)).min
      totalProduction -= flowToAugment
      totalConsumption += flowToAugment

      aPath.get.edges.foreach(residualEdge => {
        val currentFlow = residualNetwork.getEdgeFlow(residualEdge.owner)
        val flowAdjustedForEdgeType = if (residualEdge.isBackwardEdge) -flowToAugment else flowToAugment
        residualNetwork.setFlow(residualEdge.owner, currentFlow + flowAdjustedForEdgeType)
      })

      aPath = residualNetwork.augmentingPath
    }

    /* Modify the original network (not the one with extra sources and sinks) to reflect the residual flow. */
    residualNetwork.applyToNetwork(network)
  }

  /**
   * Find the lowest common flow in a path. Infinite flow (-1) isn't considered, and at least one
   * bounded edge must be present.
   * @param edges path to check lowest flow capacity for.
   * @return the lowest flow possible to augment along all the supplied edges.
   */
  private def lowestFlowCapacity(edges: Vector[ResidualEdge[Int]]): Int = {
    var lowest = -1
    edges.foreach(x =>
      if (lowest < 0 && x.flow > -1) lowest = x.flow
      else if (lowest > -1 && x.flow > -1 && x.flow < lowest) lowest = x.flow
    )

    if (lowest < 0) throw new IllegalArgumentException("No edge with bounded flow in path " + edges)
    if (lowest == 0) throw new IllegalArgumentException("No edge with positive flow in path " + edges + ", edges with 0 flow shouldn't exist in the network.")

    lowest
  }

}
