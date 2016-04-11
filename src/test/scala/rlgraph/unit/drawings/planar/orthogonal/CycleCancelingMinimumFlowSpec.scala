package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.MaxFlowMinCost.{CycleCancelingMinimumFlow, FordFulkersonMaximumFlow}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex, SuperSourceSinkInserter}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class CycleCancelingMinimumFlowSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val minimumCostFlowComputation = new CycleCancelingMinimumFlow()
  private val superVertexComputation = new SuperSourceSinkInserter()
  private val maxFlow = new FordFulkersonMaximumFlow()

  def singlePath = new {
    val v1 = new FlowVertex(1, 2) // Source
    val v2 = new FlowVertex(2, 0)
    val v3 = new FlowVertex(3, -2) // Sink
    val e1 = v1 ~> v2 ## (12, 0, 2, 0, 2) // Capacity 2
    val e2 = v2 ~> v3 ## (23, 0, 2, 0, 2) // Capacity 2
    val network = Graph.from(List(v1, v2, v3), List(e1, e2))
  }

  describe("CycleCancelingMinimumFlow") {

    it ("should shift flow from one path to a cheaper") {

      Given("a network with two paths going from source to sink, with one path having less cost than the other")
      val v1 = new FlowVertex(1, 1)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -1)

      // path with lower bound 0, capacity 1, cost 5 and flow 1
      val e1 = v1 ~> v2 ## (12, 0, 1, 5, 1)
      val e2 = v2 ~> v4 ## (24, 0, 1, 5, 1)

      // Cheaper but unused path
      val e3 = v1 ~> v3 ## (13, 0, 1, 2, 0)
      val e4 = v3 ~> v4 ## (34, 0, 1, 2, 0)

      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3, e4))

      When("computing the minimum cost flow")
      val minimumCostFlow = minimumCostFlowComputation.computeMinimumCostFlow(network, v1, v4)

      Then("the edges with flow should have its flow reduced to 0")
      minimumCostFlow.get(e1).flow should be (0)
      minimumCostFlow.get(e2).flow should be (0)

      And("the unused edges should receive 1 unit of flow")
      minimumCostFlow.get(e3).flow should be (1)
      minimumCostFlow.get(e4).flow should be (1)

    }

    /**
     * Since the planned application for this is orthogonalization, something that always has 0 as lower bound for
     * the face-vertices that induces loops, it's ok to leave it at 0 regardless. Maybe in the future  this can be looked
     * at again.
     */
    it ("should set self loops to 0") {

      Given("a network that allows 2 units of flow along a path, with a self loop on an intermediate vertex")
      val f = singlePath
      import f._
      val loop = v2 ~> v2 ## (22, 0, 2, 5, 2)
      val loopedNetwork = network + loop

      When("computing the minimum cost flow")
      val minimumCostFlow = minimumCostFlowComputation.computeMinimumCostFlow(loopedNetwork, v1, v3)

      Then("the loop should have flow 0")
      minimumCostFlow.get(loop).flow should be (0)

    }

    /**
     * Example taken from figure 2(a):
     *    http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=minimumCostFlow2
     *
     * Edges are set to max flow value.
     */
    it ("should compute minim cost flow for a complex example") {

      Given("a graph with a sub-optimal cost flow")
      val v1 = new FlowVertex(1, 5)
      val v2 = new FlowVertex(2, 2)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -2)
      val v5 = new FlowVertex(5, -4)
      val v6 = new FlowVertex(6, -1)

      val a = v1 ~> v2 ## (12, 0, 3, 1, 2)
      val b = v1 ~> v3 ## (13, 0, 3, 4, 3)
      val c = v2 ~> v3 ## (23, 0, 7, 2, 4)
      val d = v3 ~> v5 ## (35, 0, 7, 5, 4)
      val e = v3 ~> v4 ## (34, 0, 5, 2, 2)
      val f = v4 ~> v5 ## (45, 0, 3, 1, 0)
      val g = v3 ~> v6 ## (36, 0, 1, 8, 1)

      val network = Graph.from(List(v1, v2, v3, v4, v5, v6), List(a, b, c, d, e, f, g))
      val superSourceSinkNetwork = superVertexComputation.addSourceAndSink(network)
      val source = superSourceSinkNetwork.nodes.find(n => n.production > 0).get
      val sink = superSourceSinkNetwork.nodes.find(n => n.production < 0).get

      When("computing the minimum cost flow")
      val minimumCostFlow = minimumCostFlowComputation.computeMinimumCostFlow(superSourceSinkNetwork, source, sink)

      Then("the flow on each edge should match the optimal solution (see figure 2(f) in the link)")
      minimumCostFlow.get(a).flow should be (3)
      minimumCostFlow.get(b).flow should be (2)
      minimumCostFlow.get(c).flow should be (5)
      minimumCostFlow.get(d).flow should be (1)
      minimumCostFlow.get(e).flow should be (5)
      minimumCostFlow.get(f).flow should be (3)
      minimumCostFlow.get(g).flow should be (1)

    }

    it ("should respect lower bounds when reducing flow if flow is already above or equal lower bound") {
      val capacity = 3
      val lowerBound = 1
      val flow = capacity


      Given("a network with a lower bound on an edge with high cost")
      val v1 = new FlowVertex(1, flow)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -flow)

      // Lowe bound 1 means one out of three units of flow should remain after cycle canceling despite the high cost.
      val e1 = v1 ~> v2 ## (12, lowerBound, capacity, 100, flow)
      val e2 = v2 ~> v3 ## (23, lowerBound, capacity, 100, flow)

      // Low cost, enough capacity to transport all the flow, this path should be selected it it weren't for
      // the lower bound in the expensive path.
      val e3 = v1 ~> v3 ## (13, 0, capacity, 1, 0)

      val network = Graph.from(List(v1, v2, v3), List(e1, e2, e3))

      When("computing the minimum cost flow")
      val minimumCostFlow = minimumCostFlowComputation.computeMinimumCostFlow(network, v1, v3)

      Then("the path e1 and e2 should have 1 flow")
      minimumCostFlow.get(e1).flow should be (1)
      minimumCostFlow.get(e2).flow should be (1)

      And("the cheap edge e3 should still have 0 flow")
      minimumCostFlow.get(e3).flow should be (2)

    }

    it ("should cancel flow on edges with infinite capacity and a cost") {

      Given("a network with a cycle of infinite capacity and a set amount of flow (causing backwards edges with negative flow to exist)")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      /* This induces a negative cycle v1 -> v3 (backward) v3 -> v2 (backward) v2 -> v1 (forward) */
      val e1 = v2 ~> v1 ## (21, 0, -1, 1, 1)
      val e2 = v2 ~> v3 ## (23, 0, -1, 1, 1)
      val e3 = v3 ~> v1 ## (31, 0, -1, 1, 1)

      val network = Graph.from(Nil, List(e1, e2, e3))

      When("computing the minimum cost flow")
      val minimumCostFlow = minimumCostFlowComputation.computeMinimumCostFlow(network, v1, v3)

      Then("e2 and e2 should have their flow decreased to 0")
      minimumCostFlow.get(e2).flow should be (0)
      minimumCostFlow.get(e3).flow should be (0)

      And("e1 should have its flow increased to 2")
      minimumCostFlow.get(e1).flow should be (2)

    }

  }
}
