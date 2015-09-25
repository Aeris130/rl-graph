package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex, FlowVertexReplacer}

import scala.collection.mutable
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Used to compute feasible flows that takes its edges lower bounds into consideration.
 *
 * Takes a network N with a single source and sink, and creates a super set N' of the network by
 * attaching the source and sink with a directed edge s -> t with infinite capacity. Further, a new source/sink
 * pair (s', t') is created, and every vertex in the network is connected to both s' and t'.
 *
 * Every edge s' -> v takes the capacity of the combined lower bounds of all incoming edges to v
 * (edges connected s'/t' excluded) and every edge v -> t takes the capacity of the combined lower
 * bounds of every outgoing edge from v.
 *
 * @constructor Creates a new prime demand network factory.
 */
class PrimeDemandNetworkFactory {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

  /**
   * Augment the network with a feasible flow using its lower bounds.
   * @param network Network to augment.
   * @param source Single source of the network.
   * @param sink Single sink of the network.
   * @return A network with its feasible flow set.
   */
  def augmentNetwork(network: Graph[FlowVertex, FlowEdge], source: FlowVertex, sink: FlowVertex): Graph[FlowVertex, FlowEdge] = {
    throwExceptionIfMultipleSourcesOrSinks(network)
    validateSourceSink(source, sink)

    var newNetwork = network

    /* Production and consumption for the new prime source/sink should both be equal to the combined
     * lower bound in the network.
     */
    var totalLowerBound = 0
    network.edges.foreach(totalLowerBound += _.lowerBound)

    var nextAvailableVertexId = 0
    network.nodes.foreach(n => if (n.id >= nextAvailableVertexId) nextAvailableVertexId = n.id + 1)

    var nextAvailableEdgeId = 0
    network.edges.foreach(e => if (e.id >= nextAvailableEdgeId) nextAvailableEdgeId = e.id + 1)

    val primeSource = new FlowVertex(nextAvailableVertexId, totalLowerBound)
    nextAvailableVertexId += 1

    val primeSink = new FlowVertex(nextAvailableVertexId, -totalLowerBound)
    nextAvailableVertexId += 1

    newNetwork += primeSource
    newNetwork += primeSink

    /* Map every vertex to the amount of lower bounds in incoming and outgoing edges, before edges have
     * their lower bound removed. The first value in the mapped tuple is incoming bounds, the second is outgoing.
     */
    val incomingOutgoingBounds = new mutable.HashMap[Graph[FlowVertex, FlowEdge]#NodeT, (Int, Int)]()
    val nodes = newNetwork.nodes.toVector.iterator
    while (nodes.hasNext) {
      val node = nodes.next()
      var incomingLowerBounds = 0
      node.incoming.toSet[Graph[FlowVertex, FlowEdge]#EdgeT].foreach(e => incomingLowerBounds += e.toOuter.lowerBound)

      var outgoingLowerBounds = 0
      node.outgoing.foreach(e => outgoingLowerBounds += e.toOuter.lowerBound)

      incomingOutgoingBounds.put(node, (incomingLowerBounds, outgoingLowerBounds))
    }

    /*
     * Replace every original edge in the graph with a new edge having lower bound 0 and capacity c = c - lb
     */
    val allEdges: Vector[Graph[FlowVertex, FlowEdge]#EdgeT] = newNetwork.edges.toVector
    for (edge: Graph[FlowVertex, FlowEdge]#EdgeT <- allEdges) {
      val e: FlowEdge[FlowVertex] = reducedLowerBoundAndCapacity(edge.toOuter)
      newNetwork -= edge
      newNetwork += e
    }

    /*
     * For every vertex v, add an edge from s' to v with the capacity of all incoming lower bounds
     * to v. Add an edge from v to t' with the capacity of every outgoing lower bound from v.
     */
    for (node <- newNetwork.nodes.toOuter.toSet[FlowVertex]) {

      val sourceFlow = incomingOutgoingBounds(newNetwork.get(node))._1
      if (sourceFlow > 0) {
        newNetwork += primeSource ~> node ## (nextAvailableEdgeId, 0, sourceFlow, 0, 0)
        nextAvailableEdgeId += 1
      }

      val sinkFlow = incomingOutgoingBounds(newNetwork.get(node))._2
      if (sinkFlow > 0) {
        newNetwork += node ~> primeSink ## (nextAvailableEdgeId, 0, sinkFlow, 0, 0)
        nextAvailableEdgeId += 1
      }

    }

    /* Replace the original source and sink with regular vertices having production 0, and add an edge with
     * infinite capacity from the original sink to the original source.
     */
    val mutedSource = source.newProduction(0)
    val mutedSink = sink.newProduction(0)
    newNetwork = FlowVertexReplacer.replace(newNetwork, source, mutedSource)
    newNetwork = FlowVertexReplacer.replace(newNetwork, sink, mutedSink)
    newNetwork += mutedSink ~> mutedSource ## (nextAvailableEdgeId, 0, -1, 0, 0)

    newNetwork
  }

  /**
   * Copies an edge and assigns the copy 0 lower bound and c - lb capacity.
   */
  private def reducedLowerBoundAndCapacity(edge: FlowEdge[FlowVertex]): FlowEdge[FlowVertex] = {
    edge.from ~> edge.to ## (edge.id, 0, edge.capacity - edge.lowerBound, edge.cost, 0, edge.crosses)
  }

  private def throwExceptionIfMultipleSourcesOrSinks(network: Graph[FlowVertex, FlowEdge]) {
    val sources = network.nodes.filter(n => n.production > 0)
    val sinks = network.nodes.filter(n => n.production < 0)
    if (sources.size > 1)
      throw new IllegalArgumentException("The network " + network + " has more than one source: " + sources)
    if (sinks.size > 1)
      throw new IllegalArgumentException("The network " + network + " has more than one sink: " + sinks)
  }

  private def validateSourceSink(source: FlowVertex, sink: FlowVertex) {
    if (source == sink)
      throw new IllegalArgumentException("Source (" + source + ") must differ from sink (" + sink + ")")
    if (source.production <= 0)
      throw new IllegalArgumentException("Source (" + source + ") must have positive production.")
    if (sink.production >= 0)
      throw new IllegalArgumentException("Sink (" + sink + ") must have negative consumption.")
  }
}
