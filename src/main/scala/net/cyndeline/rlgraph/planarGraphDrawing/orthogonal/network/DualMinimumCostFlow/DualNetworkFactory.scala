package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.DualMinimumCostFlow

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

/**
 * Creates the dual solution network of a constraint graph, where:
 *
 *  - The production/consumption of every vertex is the negative net degree (-(indegree - outdegree)) of the vertex edges
 *  - The cost of each edge is equal to the negative weight of the equivalent edge in the constraint graph
 *  - The capacity of each edge is the total production of all sources
 *  - The lower bound of each edge is 0
 *
 *  @constructor Creates a new dual network factory.
 */
class DualNetworkFactory {

  /**
   * Creates the dual network of a shape graph.
   * @param shapeGraph Graph to base dual of.
   * @return the dual solution, and twp mappings: Flow vertex id's of the dual and their shape graph equivalents, and
   *         the same for flow edges.
   */
  def computeDual[VType](shapeGraph: Graph[Segment[VType], WLkDiEdge]
                          ): (Graph[FlowVertex, FlowEdge], Map[FlowVertex, Segment[VType]], Map[FlowEdge[FlowVertex], WLkDiEdge[Segment[VType]]]) = {
    implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
    var totalProduction = 0
    var nodeMapping = Map[FlowVertex, Segment[VType]]()
    var edgeMapping = Map[FlowEdge[FlowVertex], WLkDiEdge[Segment[VType]]]()
    var nodeId = 0
    var edgeId = 0

    // Sorted to make debugging easier
    for (inner <- shapeGraph.nodes.toOuter.toSet[Segment[VType]].toVector.sortWith(_.hashCode < _.hashCode)) {
      val outer = shapeGraph.get(inner)
      val netDegree = outer.inDegree - outer.outDegree
      val flowVertex = new FlowVertex(nodeId, -netDegree)
      nodeId += 1
      nodeMapping += (flowVertex -> inner)

      if (-netDegree > 0)
        totalProduction += -netDegree
    }

    val segmentToFlow = nodeMapping.map(_.swap)
    for (edge <- shapeGraph.edges.toOuter.toSet[WLkDiEdge[Segment[VType]]]) {
      val from: FlowVertex = segmentToFlow.get(shapeGraph.get(edge.from)).get
      val to: FlowVertex = segmentToFlow.get(shapeGraph.get(edge.to)).get
      val flowEdge = from ~> to ## (edgeId, 0, totalProduction, -edge.weight.toInt, 0)
      edgeId += 1
      edgeMapping += (flowEdge -> edge)
    }

    (Graph.from(nodeMapping.keys, edgeMapping.keys), nodeMapping, edgeMapping)
  }

}
