package net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util

import net.cyndeline.rlgraph.pathfinding.{BFSPathfinder, Path}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Provides methods for augmenting flow on a network by also adding
 * residual data whenever the graph is changed.
 *
 * @constructor Creates a new residual network.
 */
class ResidualNetwork(network: Graph[FlowVertex, FlowEdge], source: FlowVertex, sink: FlowVertex) {
  private implicit def edge2ResidualEdgeAssoc[F <: Int](e: DiEdge[F]) = new ResidualEdgeAssoc[F](e)
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val pathfinder = new BFSPathfinder()
  private var edgeId = 0 // Adjusted in setup, so always declare it on top
  private var residualNetwork: Graph[Int, ResidualEdge] = Graph[Int, ResidualEdge]()

  /* This is needed to store backwards flow on edges with infinite forward capacity (no option to compute flow
   * using edge capacity - forward flow) and a backwards flow that is less than the edges lower bounds (thus no
   * backward edge in the residual network). Values will only be stored here if this is the case.
   */
  private var flowValues = Map[FlowEdge[FlowVertex], Int]()
  setup()

  /**
   * Returns a graph representation of the network.
   * @return a graph representation of the network.
   */
  def underlyingNetwork = residualNetwork

  /**
   * Computes a path through the original network that still allows additional flow to be inserted.
   * The start and stop is the source and sink of the network.
   *
   * @return A list of residual edges as they are traversed in the residual network along the path.
   */
  def augmentingPath: Option[Path[Int, ResidualEdge]] = pathfinder.computePath(source.id, sink.id, residualNetwork)

  /**
   * Retrieves the residual edges associated with some edge in the regular network.
   *
   * @param edge The owner of the residual edges.
   * @return A forward and backward edge associated with the regular edge. Both are not guaranteed to exist.
   */
  def residualValues(edge: FlowEdge[FlowVertex]): (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = {
    var forward: Option[ResidualEdge[Int]] = None
    var back: Option[ResidualEdge[Int]] = None

    val edges: Set[Graph[Int, ResidualEdge]#EdgeT] = residualNetwork.edges.toSet
    for (e <- edges) {
      val eInner = e.toOuter
      if (eInner.owner.id == edge.id) {
        if (eInner.dir) forward = Option(eInner)
        else back = Option(eInner)
      }
    }

    (forward, back)
  }

  /**
   * Returns the current flow for an edge based on the residual network.
   *
   * @param edge Edge to check flow amount for. The capacity and lower bound of this edge is used in conjuction with
   *             the residual flow to compute the final flow amount.
   */
  def getEdgeFlow(edge: FlowEdge[FlowVertex]): Int = {
      flowValues.get(edge).getOrElse(throw new IllegalArgumentException("No backward flow value stored for " + edge))
  }

  /**
   * Updates the backward and forward pointing residual edges in the graph to reflect a new
   * amount of flow.
   *
   * @param edge Edge that the residual edge pair were based on.
   * @param amount Amount of flow to set.
   */
  def setFlow(edge: FlowEdge[FlowVertex], amount: Int): Unit = {
    val rp: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residualValues(edge)
    val forward = rp._1
    val back = rp._2
    val infiniteCapacity = edge.capacity < 0

    flowValues += (edge -> amount)

    if (forward.isDefined) residualNetwork -= forward.get
    if (back.isDefined) residualNetwork -= back.get

    if (amount < edge.capacity || infiniteCapacity) {
      val newForwardFlow = if (!infiniteCapacity) edge.capacity - amount else -1
      val id = if (forward.isDefined) forward.get.id else {
        edgeId += 1
        edgeId - 1
      }

      val newForwardEdge = makeEdge(id, edge.cost, newForwardFlow, true, edge.from.id, edge.to.id, edge)
      residualNetwork += newForwardEdge
    }

    if (amount - edge.lowerBound > 0) {
      val id = if (back.isDefined) back.get.id else {
        edgeId += 1
        edgeId - 1
      }

      val newBackEdge = makeEdge(id, -edge.cost, amount - edge.lowerBound, false, edge.to.id, edge.from.id, edge)
      residualNetwork += newBackEdge
    }
  }

  /**
   * Applies the computed flow from this residual network onto a regular flow network.
   * Edges not present in the residual representation will have their values set to 0.
   *
   * @param flowNetwork Network containing owner-edges of this residual network.
   * @return A copy of the supplied network with flow values set according to the residual values.
   */
  def applyToNetwork(flowNetwork: Graph[FlowVertex, FlowEdge]): Graph[FlowVertex, FlowEdge] = {
    var finalNetwork = flowNetwork
    val allEdges = network.edges.toVector
    var n = 0
    while (n < allEdges.size) {
      val edge: FlowEdge[FlowVertex] = allEdges(n).toOuter
      val flow = flowValues.get(edge).getOrElse(0)
      finalNetwork -= edge
      finalNetwork += copyEdge(edge, flow)

      n += 1
    }

    finalNetwork
  }

  /**
   * Computes a
   * @return a network that contains backward edges even if the flow on that edge only equals the lower bound. This
   *         is needed for algorithms that wants to compute shortest paths on the network using all edges, including
   *         the ones that can't have their flow canceled out.
   */
  def networkThatIgnoresLowerBounds:  Graph[Int, ResidualEdge] = {
    var residualEdges = Vector[ResidualEdge[Int]]()

    for (edgeAndFlow: (FlowEdge[FlowVertex], Int) <- flowValues.iterator) {
      val edge = edgeAndFlow._1
      val flow = edgeAndFlow._2

      if (flow > 0) {
        // Back edge
        residualEdges = residualEdges :+ makeEdge(edgeId, -edge.cost, flow, false, edge.to.id, edge.from.id, edge)
        edgeId += 1
      }

      if (flow < edge.capacity) {
        // Forward edge
        residualEdges = residualEdges :+ makeEdge(edgeId, edge.cost, edge.capacity - flow, true, edge.from.id, edge.to.id, edge)
        edgeId += 1
      }
    }

    Graph.from(Nil, residualEdges)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "Network: " + residualNetwork
    builder.toString()
  }

  /* Creates a pair of residual edges for every edge in the network. */
  private def setup(): Graph[Int, ResidualEdge] = {
    val residualNetwork = Graph[Int, ResidualEdge]()

    for (edge <- network.edges) {
      setFlow(edge.toOuter, edge.flow)
    }

    residualNetwork
  }

  private def makeEdge(id: Int, cost: Int, flow: Int, dir: Boolean, from: Int, to: Int, owner: FlowEdge[FlowVertex]): ResidualEdge[Int] = {
    from ~> to ## (id, cost, flow, dir, owner)
  }

  private def copyEdge(edge: FlowEdge[FlowVertex], newFLow: Int): FlowEdge[FlowVertex] = {
    edge.from ~> edge.to ## (edge.id, edge.lowerBound, edge.capacity, edge.cost, newFLow, edge.crosses)
  }

}
