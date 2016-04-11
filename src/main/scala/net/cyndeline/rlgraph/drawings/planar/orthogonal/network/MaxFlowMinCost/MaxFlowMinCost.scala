package net.cyndeline.rlgraph.drawings.planar.orthogonal.network.MaxFlowMinCost

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.PrimeDemandNetworkFactory
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex, SuperSourceSinkInserter}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Computes a saturating flow of minimal cost in a network where each edge has a lower bound, a
 * capacity and a flow. The algorithm proceeds in three steps:
 *
 *    1: Compute a feasable flow where every edge has its lower bounds filled
 *    2: Ford-Fulkerson: Augment the flow to compute maximal flow without lowering the flow on any edge below its
 *        lower bound (bounds are respected by the residual network, not the algorithm).
 *    3: Compute the minimum cost flow by re-routing the flow along negative cycles while taking capacity
 *        and lower bounds into consideration (again through the residual network).
 *
 * @constructor Constructs a new maximum-flow minimum-cost computation.
 */
class MaxFlowMinCost {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val inserter = new SuperSourceSinkInserter()
  private val saturatingNetworkFactory = new PrimeDemandNetworkFactory()
  private val fordFulkerson = new FordFulkersonMaximumFlow()
  private val minimumCost = new CycleCancelingMinimumFlow()

  def computeMaxFlowMinCost(network: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = {

    /* Some of the algorithms require a single source/sink, so make a pair if they don't already exist. */
    var newNetwork = inserter.addSourceAndSink(network)

    // These are needed since scala has some troubles finding the context for n inside the filter
    def incomingIsEmpty(n: FlowVertex, network: Graph[FlowVertex, FlowEdge]) = network.get(n).incoming.isEmpty
    def outgoingIsEmpty(n: FlowVertex, network: Graph[FlowVertex, FlowEdge]) = network.get(n).outgoing.isEmpty
    val source = newNetwork.nodes.toOuter.toSet[FlowVertex].find(n => n.production > 0).get
    val sink = newNetwork.nodes.toOuter.toSet[FlowVertex].find(n => n.production < 0).get

    /* If there's at least one edge with lower bounds, a saturating flow must be computed before
     * the ford-fulkerson can work.
     */
    def hasLowerBound(e: FlowEdge[FlowVertex]) = e.lowerBound > 0
    if (newNetwork.edges.toSet.exists((e: Graph[FlowVertex, FlowEdge]#EdgeT) => hasLowerBound(e.toOuter))) {

      /* Compute a network that saturates flow equal to or above the lower bound of every edge. */
      var saturatingNetwork = saturatingNetworkFactory.augmentNetwork(newNetwork, source, sink)

      /* Saturate that flow by computing the maximum flow for the saturating network, then adding the
       * lower bound of the original network to the flow of every edge.
       */
      val primeSource = saturatingNetwork.nodes.toOuter.toSet[FlowVertex].filter(n => incomingIsEmpty(n, saturatingNetwork)).head
      val primeSink = saturatingNetwork.nodes.toOuter.toSet[FlowVertex].filter(n => outgoingIsEmpty(n, saturatingNetwork)).head
      saturatingNetwork = fordFulkerson.computeMaximumFlow(saturatingNetwork, primeSource, primeSink)
      val originalNewEdges = newNetwork.edges.iterator
      while(originalNewEdges.hasNext) {
        val edge = originalNewEdges.next().toOuter
        val saturatedEdge = saturatingNetwork.get(edge).toOuter
        newNetwork -= edge
        newNetwork += makeEdge(edge.from, edge.to, edge, saturatedEdge.flow + edge.lowerBound)
      }
    }

    /* Now compute the maximum flow on the original network again. This time the flow saturates the lower bound
     * of every edge, so the residual network won't trigger augmentations that violate these bounds.
     */
    newNetwork = fordFulkerson.computeMaximumFlow(newNetwork, source, sink)

    /* Finally, repeat cycle-cancelling until the cost is minimal. */
    newNetwork = minimumCost.computeMinimumCostFlow(newNetwork, source, sink)

    /* Apply the final flow values to the input network in case additional sources or sinks were added. */
    val oldNetworkEdges = network.edges.iterator
    var resultingNetwork = network
    while(oldNetworkEdges.hasNext) {
      val edge = oldNetworkEdges.next().toOuter

      if (!newNetwork.contains(edge)) {
        throw new Error("Edge " + edge + " from " + network + " was not found in " + newNetwork)
      }

      val newEdge = newNetwork.get(edge).toOuter
      resultingNetwork -= edge
      resultingNetwork += makeEdge(edge.from, edge.to, newEdge, newEdge.flow)
    }

    resultingNetwork
  }

  private def makeEdge(from: FlowVertex, to: FlowVertex, oldEdge: FlowEdge[FlowVertex], flow: Int) = {
    from ~> to ## (oldEdge.id, oldEdge.lowerBound, oldEdge.capacity, oldEdge.cost, flow, oldEdge.crosses)
  }
}
