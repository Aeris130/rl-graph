package net.cyndeline.rlgraph.drawings.planar.orthogonal.network.DualMinimumCostFlow

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.MaxFlowMinCost.MaxFlowMinCost
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowVertex, _}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.{BellmanFordShortestPath, NegativeCostTransformer}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes the dual minimum cost assignment for a horizontal or vertical constraint graph.
 *
 * @constructor Constructs a new dual minimum cost flow computation.
 */
class DualMinimumCostFlow {
  private val dualFactory = new DualNetworkFactory()
  private val negativeCostTransform = new NegativeCostTransformer()
  private val maxFlowMinCost = new MaxFlowMinCost()
  private val superSourceSink = new SuperSourceSinkInserter()
  private val shortestPath = new BellmanFordShortestPath()

  /**
   * Computes the dual minimum cost assignment of a segment shape-graph according to the algorithm:
   *
   * # Create the dual network of the constraint graph
   * # Transform negative costs to positive
   * # Compute minimum cost flow of the dual, using a super-sink if needed
   * # Transform the flow back onto the original dual with negative costs
   * # Construct the residual network of the flow
   * # Compute shortest distance to every vertex from the sink, using edge costs as distance. </br>
   * The distance to each vertex is the coordinate assignment for the segment corresponding to that vertex.
   *
   * @param shapeGraph Shape graph with segments for which to assign coordinate values.
   * @return a set of segment with their coordinate values set according to the assignment.
   */
  def assignSegmentCoordinates[VType](shapeGraph: Graph[Segment[VType], WLkDiEdge]): Set[Segment[VType]] = {
    implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

    if (shapeGraph.isEmpty)
      throw new IllegalArgumentException("The shape graph was empty.")

    weightsMustBeOne(shapeGraph) // Some computations below hinge on all edge weights being 1
    val dual: (Graph[FlowVertex, FlowEdge], Map[FlowVertex, Segment[VType]], Map[FlowEdge[FlowVertex], WLkDiEdge[Segment[VType]]])
      = dualFactory.computeDual(shapeGraph)
    val flowNetwork = dual._1

    /* Store the edges having negative cost so they can be transformed back again. */
    val negativeCostEdges = for {
      edge <- flowNetwork.edges.toOuter.toSet[FlowEdge[FlowVertex]]
      if edge.cost < 0
    } yield edge


    val flowWithNoNegativeCosts: Graph[FlowVertex, FlowEdge] = negativeCostTransform.removeNegativeCosts(flowNetwork)

    /* A special case: If the network consists of a single source and sink with a single path of negative costs
     * between them, both source and sink will have their production set to 0 during transformation. No flow can
     * be sent, but the residual network can still be used to compute distances from the sink, since all path-edges
     * are reversed (creating a path from sink -> source, and a path from source -> all other vertices not a part of
     * the path).
     */

    /* If no sink exists, no source exists either. */
    val minimumCostFlow: Graph[FlowVertex, FlowEdge] = if (!flowWithNoNegativeCosts.nodes.exists(_.production < 0)) {
      flowWithNoNegativeCosts // 0 flow
    } else {
      maxFlowMinCost.computeMaxFlowMinCost(flowWithNoNegativeCosts)
    }

    /* We now have a min-cost max-flow in the network with no negative costs. Applying this flow back onto the original
     * dual is done as follows: Applying the flow from the transformed network simply means taking the capacity of each
     * negative edge, and creating an edge pointing in the opposite direction with negative cost and flow
     * (capacity - flow_in_transformed_network). Normally this would also mean reversing the modification done on vertex
     * productions during transformation, but since doing so would only yield the original productions, it's simpler
     * to use the original networks vertices instead (since they're immutable anyway).
     */
    var finalNetworkFlow = flowNetwork

    /* For each edge, if the edge had negative cost in the original dual, revert its flow. If not, apply it as is. */
    for (e: minimumCostFlow.EdgeT <- minimumCostFlow.edges) {
      val edge = e.toOuter
      finalNetworkFlow -= edge // removes the original edge using the ID of the new one

      if (!negativeCostEdges.contains(edge)) {

        /* If the edge was not among the negative-cost ones, it's pointing in the right direction, so all that's
         * left is to add it.
         */
        finalNetworkFlow += edge

      } else {
        val flow = edge.capacity - edge.flow
        val restoredEdge = edge.to ~> edge.from ## (edge.id, edge.lowerBound, edge.capacity, -edge.cost, flow, edge.crosses)
        finalNetworkFlow += restoredEdge
      }
    }

    /* If there's more than one sink, we need a super-sink to guarantee that all vertices can be reached from it.
     * Sources don't matter though, so we'll just use an arbitrary one.
     */
    val nodes = finalNetworkFlow.nodes.toOuter.toSet[FlowVertex]
    val source = nodes.find(_.production > 0).get

    /* Add a super-sink if more than one sink exists.
     *
     * It should be noted that doing this won't affect the coordinate assignment of the regular vertices.
     * Since all of the edges from the super-sink to the regular sinks have weight 0 (per specification of the
     * insertion), the distance from the super sink (0) to the regular sinks will be (0 + distance(0)) == 0, so both
     * the ss and the regular sinks will have coordinates == 0. And since the ss doesn't have a corresponding segment
     * in the shape-graph, having duplicate coordinates for it and its children doesn't matter.
     */
    finalNetworkFlow = superSourceSink.addSink(finalNetworkFlow)

    val sink = finalNetworkFlow.nodes.toOuter.toSet[FlowVertex].find(_.production < 0).get
    val residualNetwork = new ResidualNetwork(finalNetworkFlow, source, sink)
    val distances: Map[Int, Int] = shortestPath.findDistanceWithNoCycle(sink.id, residualNetwork.underlyingNetwork)

    val flowIdToSegment: Map[Int, Segment[VType]] = dual._2.map(kv => (kv._1.id, kv._2))

    /* Set the coordinate of each segment as the distance in the residual network. */
    val segmentsWithCoordinates: Set[Segment[VType]] = (for {
      idDistance <- distances.iterator
      id = idDistance._1
      if flowIdToSegment.contains(id) // If a super-source/sink exists, it doesn't have a segment
      distance = idDistance._2
      segment = flowIdToSegment(id)
    } yield segment.newCoordinate(distance)).toSet

    segmentsWithCoordinates
  }

  /**
   * Throws an exception if every weight in a shape-graph isn't 1.
   */
  private def weightsMustBeOne[VType](shapeGraph: Graph[Segment[VType], WLkDiEdge]) {
    val edges = shapeGraph.edges.iterator
    while (edges.hasNext)
      if (edges.next().weight != 1)
        throw new IllegalArgumentException("All edge weights must be 1")
  }
}
