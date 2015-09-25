package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.MaxFlowMinCost

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.BellmanFordCycleDetection
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowVertex, ResidualEdge, ResidualNetwork}

import scalax.collection.immutable.Graph

/**
 * Computes a minimum-cost solution to a network flow from a feasible maximal flow.
 *
 * @constructor Constructs a new minimum flow cycle canceler.
 */
class CycleCancelingMinimumFlow {
  private val cycleFinder = new BellmanFordCycleDetection()

  /**
   *
   * @param network A network with a set flow. No further augmentation will be performed prior to cycle canceling.
   * @param source The only source of the network.
   * @param sink The only sink of the network.
   * @return a copy of the supplied network with flow values set such that the cost is minimal, based on negative
   *         cycles of a residual network that can be reached from the sink.
   */
  def computeMinimumCostFlow(network: Graph[FlowVertex, FlowEdge], source: FlowVertex, sink: FlowVertex): Graph[FlowVertex, FlowEdge] = {
    val residualNetwork = new ResidualNetwork(network, source, sink)

    var negativeCycle: Option[Vector[ResidualEdge[Int]]] = cycleFinder.findCycle(residualNetwork.underlyingNetwork, sink.id)

    // The actual cycle cancelling algorithm
    while (negativeCycle.isDefined) {
      val edges = negativeCycle.get
      val lowestCap = lowestCapacity(edges)

      for (edge <- edges) {
        val currentFlow = residualNetwork.getEdgeFlow(edge.owner)

        /* If the edge is directed forward, add more flow. Otherwise subtract it. */
        val flowToAdd = if (edge.dir) lowestCap else -lowestCap
        residualNetwork.setFlow(edge.owner, Math.max(edge.owner.lowerBound, currentFlow + flowToAdd))
      }

      negativeCycle = cycleFinder.findCycle(residualNetwork.underlyingNetwork, sink.id)
    }

    // Apply the values of the residual network the the original network
    residualNetwork.applyToNetwork(network)
  }

  private def lowestCapacity(cycle: Vector[ResidualEdge[Int]]): Int = {
    var lowest = Int.MaxValue

    // Infinite flow = -1, doesn't count as an actual flow value.
    cycle.foreach(edge => if (!edge.hasInfiniteFLow && edge.flow < lowest) lowest = edge.flow)
    lowest
  }
}
