package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Modifies a network by inserting a single pair of super source and sink, linking them
 * to all previous sources/sinks in the network.
 *
 * @constructor Creates a new super source/sink inserter.
 */
class SuperSourceSinkInserter {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

  /**
   * Adds a super source and a super sink to a network.
   * @param network Network to add source and sink to.
   * @return A copy of the network with source and sink added.
   */
  def addSourceAndSink(network: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = addSource(addSink(network))

  /**
   * Adds a source to a flow network connected to all previous sources if there's more than 1. Previous
   * sources will have their production set to 0 and become intermediate vertices in the network.
   *
   * @param network Network to add super source to.
   * @return A new network with a single super source, or the same network as inputted if only one source was found.
   */
  def addSource(network: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = process(network, Source)

  /**
   * Adds a sink to a flow network connected to all previous sinks if there's more than 1. Previous
   * sinks will have their production set to 0 and become intermediate vertices in the network.
   *
   * @param network Network to add super sink to.
   * @return A new network with a single super sink, or the same network as inputted if only one sink was found.
   */
  def addSink(network: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = process(network, Sink)

  /**
   * Generic inserter capable of handling both sources and sinks (both referred to as targets).
   *
   * @param network network to insert source or sink into.
   * @param t which type of vertex (source or sink) that should be inserted.
   * @return a new network with a single super-vertex if the specified type.
   */
  private def process(network: Graph[FlowVertex, FlowEdge], t: VertexType): Graph[FlowVertex, FlowEdge] = {
    var totalProduction = 0
    var newNetwork = network
    val targets: ArrayBuffer[FlowVertex] = findTargetVertices(newNetwork, t)
    targets.foreach( totalProduction += _.production)

    val saught = t match {
      case Source => "sources"
      case Sink => "sinks"
    }
    if (targets.isEmpty) throw new IllegalArgumentException("No " + saught + " found in network " + newNetwork)
    if (targets.size < 2) return network // Need at least 2 sources to make a super-source useful

    /* Save all outgoing/incoming edges in order to copy their data to the new non-sinks that are about to be inserted.
     * This includes edges going from one source/sink to another.
     */
    val savedEdges = saveConnectedEdges(targets, newNetwork, t)

    /* Add a new super-source/sink, a new non-source/sink vertex for every previous target and attach an edge to/from
     * the super-s for each such vertex.
     */
    val superVertex = new FlowVertex(nextAvailableVertexId(newNetwork), totalProduction)
    newNetwork += superVertex

    // Remove all targets and insert new copies, but with +-0 production
    newNetwork = newConnectedTargets(newNetwork, targets, superVertex, t)

    /* Add all the previously deleted edges (removed when their vertex owners were removed). */
    for (edge <- savedEdges) {

      // If the owner/target was a non-source vertex, it'll still be in the network. Otherwise the new copy with the
      // same id should be there.
      val newOwner = findVertex(edge.from.id, newNetwork)
      val newTarget = findVertex(edge.to.id, newNetwork)

      newNetwork += (newOwner ~> newTarget ## (edge.id, edge.lowerBound, edge.capacity, edge.cost, edge.flow, edge.crosses))
    }

    newNetwork
  }

  /**
   * Finds a vertex with a given id.
   * @param id Id to search for.
   * @param network Network to serach in.
   * @return The vertex having the id 'id.
   */
  private def findVertex(id: Int, network: Graph[FlowVertex, FlowEdge]): FlowVertex = {
    for (node <- network.nodes) if (node.id == id) return node

    throw new IllegalArgumentException("No vertex with id " + id + " found.")
  }

  /**
   * Adds every source/sink to a collection
   * @param network Network containing sources/sinks
   * @param t Type of vertex
   */
  private def findTargetVertices(network: Graph[FlowVertex, FlowEdge], t: VertexType): ArrayBuffer[FlowVertex] = {
    val targets = new ArrayBuffer[FlowVertex]()
    def productionValid(p: Int) = t match {
      case Source => p > 0
      case Sink => p < 0
    }

    for (node <- network.nodes.toOuter.toSet[FlowVertex]) {
      if (productionValid(node.production)) {
        targets += node
      }
    }

    targets
  }

  /**
   * Finds every ingoing/outgoing edge connected to a set of vertices and stores them in a buffer.
   * @param vertices Set of vertices that edges should be connected to.
   * @param network Network containing edges and vertices.
   * @param t Vertex type. Source for outgoing edges, Sink for incoming.
   * @return A buffer containing all found edges.
   */
  private def saveConnectedEdges(vertices: ArrayBuffer[FlowVertex], network: Graph[FlowVertex, FlowEdge], t: VertexType): ArrayBuffer[FlowEdge[FlowVertex]] = {
    val found = new ArrayBuffer[FlowEdge[FlowVertex]]()

    for (vertex <- vertices) {
      val node = network.get(vertex)
      val foundEdges = (node.incoming ++ node.outgoing).toVector
      var n = 0
      while (n < foundEdges.size) {
        found += foundEdges(n).toOuter
        n += 1
      }
    }

    found
  }

  /**
   * Replaces a set of vertices (sources or sinks) with copies using 0 production. Also adds an edge
   * to/from (depending on source/sink) each target to the super vertex, having flow equal to the total
   * out/in-flow of the target.
   * @param network Network to modify.
   * @param targets All vertices to copy and connect to the super vertex and give production 0
   * @param superV The super vertex to connect to
   * @param t Source/sink
   * @return A network with the super vertex connected to copies of the previous targets, with each copy having
   *         production 0 and the original target removed from the network.
   */
  private def newConnectedTargets(network: Graph[FlowVertex, FlowEdge], targets: ArrayBuffer[FlowVertex], superV: FlowVertex, t: VertexType): Graph[FlowVertex, FlowEdge] = {
    var nextEdgeId = nextAvailableEdgeId(network)
    var resultNetwork = network

    def edgeConstructor(newTarget: FlowVertex, edgeId: Int, capacity: Int, flow: Int) = t match {
      case Source => superV ~> newTarget ## (edgeId, 0, capacity, 0, flow)
      case Sink => newTarget ~> superV ## (edgeId, 0, capacity, 0, flow)
    }

    for (target <- targets) {

      /* If there's previous flow coming in to the target from its edges, add it to the super-edge. */
      var incomingFlow = 0
      network.get(target).incoming.foreach(e => incomingFlow += e.flow)

      var outgoingFlow = 0
      network.get(target).outgoing.foreach(e => outgoingFlow += e.flow)

      val finalFlow = Math.abs(incomingFlow - outgoingFlow)

      resultNetwork -= target
      val newTarget = new FlowVertex(target.id, 0)
      resultNetwork += newTarget

      /* Add edge to/from super with a new id, 0 lower bound, capacity equal to the production (in absolutes, since
       * edges can't have negative capacity), 0 cost and 0 flow.
       */
      resultNetwork += edgeConstructor(newTarget, nextEdgeId, Math.abs(target.production), finalFlow)
      nextEdgeId += 1
    }

    resultNetwork
  }

  private def nextAvailableVertexId(network: Graph[FlowVertex, FlowEdge]): Int = {
    var highestVertexId = 0
    for (node <- network.nodes.toOuter.toSet[FlowVertex]) if (node.id > highestVertexId) highestVertexId = node.id
    highestVertexId + 1
  }

  private def nextAvailableEdgeId(network: Graph[FlowVertex, FlowEdge]): Int = {
    var highestEdgeId = 0
    for (edge <- network.edges) if (edge.id > highestEdgeId) highestEdgeId = edge.id
    highestEdgeId + 1
  }

  private sealed trait VertexType
  private case object Source extends VertexType
  private case object Sink extends VertexType
}
