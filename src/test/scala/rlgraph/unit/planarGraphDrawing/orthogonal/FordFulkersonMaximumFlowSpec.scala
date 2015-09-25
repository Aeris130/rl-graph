package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.MaxFlowMinCost.FordFulkersonMaximumFlow
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex, SuperSourceSinkInserter}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FordFulkersonMaximumFlowSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val flowComputation = new FordFulkersonMaximumFlow()
  private val sourceSinkInserter = new SuperSourceSinkInserter()

  def singlePath = new {
    val v1 = new FlowVertex(1, 2) // Source
    val v2 = new FlowVertex(2, 0)
    val v3 = new FlowVertex(3, -2) // Sink
    val e1 = v1 ~> v2 ## (12, 0, 2, 0, 0) // Capacity 2
    val e2 = v2 ~> v3 ## (23, 0, 2, 0, 0) // Capacity 2
    val network = Graph.from(List(v1, v2, v3), List(e1, e2))
  }

  describe("FordFulkersonMaximumFlow") {

    it ("should compute maximum flow for every edge along a path with the same capacity") {

      Given("a network consisting of a single path with capacity 2 in each edge")
      val f = singlePath
      import f._

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, v1, v3)

      Then("every edge in the network should have flow 2")
      maximumFlowNetwork.get(e1).flow should be (2)
      maximumFlowNetwork.get(e2).flow should be (2)

    }

    it ("should limit the flow along a path to the lowest common capacity") {

      Given("a network consisting of a single path of capacity 4, except for 1 edge with capacity 3")
      val v1 = new FlowVertex(1, 4) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -4) // Sink

      val e1 = v1 ~> v2 ## (12, 0, 4, 0, 0)
      val e2 = v2 ~> v3 ## (23, 0, 3, 0, 0) // Bottleneck with flow = 3
      val e3 = v3 ~> v4 ## (34, 0, 4, 0, 0)

      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3))

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, v1, v4)

      Then("every edge in the network should have flow 3")
      maximumFlowNetwork.get(e1).flow should be (3)
      maximumFlowNetwork.get(e2).flow should be (3)

    }

    it ("should limit the flow based on how much the source can supply") {

      Given("a graph with less production/consumption (3) in its source than capacity (5) along the edges")
      val v1 = new FlowVertex(1, 3) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v2 ## (12, 0, 5, 0, 0) // Capacity 5
      val e2 = v2 ~> v3 ## (23, 0, 5, 0, 0) // Capacity 5
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, v1, v3)

      Then("every edge in the network should have flow 3")
      maximumFlowNetwork.get(e1).flow should be (3)
      maximumFlowNetwork.get(e2).flow should be (3)

    }

    it ("should skip augmenting flow in loops") {

      Given("a graph with a self loop")
      val f = singlePath
      import f._

      /* Adding a loop to the intermediate node with higher capacity (4) as the rest of the network. */
      val loop = v2 ~> v2 ## (22, 0, 4, 0, 0)
      val selfLoopNetwork = network + loop

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(selfLoopNetwork, v1, v3)

      Then("the flow going through the loop should be 0")
      maximumFlowNetwork.get(loop).flow should be (0)

    }

    /*
     * The same graph used to demonstrate the algorithm at:
     *  http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=maxFlow
     */
    it ("should compute maximum flow in a complex example") {

      Given("the example network")
      val X = new FlowVertex(1, 3) // Source
      val Y = new FlowVertex(2, -3) // Sink

      val A = new FlowVertex(3, 0)
      val B = new FlowVertex(4, 0)
      val C = new FlowVertex(5, 0)
      val D = new FlowVertex(6, 0)
      val E = new FlowVertex(7, 0)

      val e1 = X ~> A ## (11, 0, 3, 0, 0)
      val e2 = X ~> B ## (12, 0, 1, 0, 0)
      val e3 = A ~> C ## (13, 0, 3, 0, 0)
      val e4 = B ~> C ## (14, 0, 5, 0, 0)
      val e5 = B ~> D ## (15, 0, 4, 0, 0)
      val e6 = D ~> E ## (16, 0, 2, 0, 0)
      val e7 = E ~> Y ## (17, 0, 3, 0, 0)
      val e8 = C ~> Y ## (18, 0, 2, 0, 0)

      val network = Graph.from(List(A, B, C, D, E, X, Y), List(e1, e2, e3, e4, e5, e6, e7, e8))

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, X, Y)

      Then("the edges should have the maximum flow computed (heh)")
      maximumFlowNetwork.get(e1).flow should be (2)
      maximumFlowNetwork.get(e2).flow should be (1)
      maximumFlowNetwork.get(e3).flow should be (2)
      maximumFlowNetwork.get(e4).flow should be (0)
      maximumFlowNetwork.get(e5).flow should be (1)
      maximumFlowNetwork.get(e6).flow should be (1)
      maximumFlowNetwork.get(e7).flow should be (1)
      maximumFlowNetwork.get(e8).flow should be (2)

    }

    it ("should compute a maximum flow when an edge has infinite capacity") {

      Given("a network where one edge has infinite capacity")
      val v1 = new FlowVertex(1, 3) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v2 ## (12, 0, 5, 0, 0) // Capacity 5
      val e2 = v2 ~> v3 ## (23, 0, -1, 0, 0) // Infinite capacity
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, v1, v3)

      Then("every edge in the network should have flow 3, the bottleneck created by other vertices")
      maximumFlowNetwork.get(e1).flow should be (3)
      maximumFlowNetwork.get(e2).flow should be (3)

    }

    /**
     * NOTE! This tests takes known test-data into account. Two paths of equal capacity goes from source to
     * vertex. The test was written by checking which path the computation choses given 0 initial flow,
     * then filling the other path with flow, thereby producing an optimal flow that is different from what would be
     * computed from a fresh network with 0 flow.
     *
     * If the test succeeds, the computation should leave the flow as it is.
     */
    it ("should take the current flow of the supplied network into account when augmenting") {

      Given("a network with two paths from source to sink, with equal capacity")

      // With only 2 production, there's only enough flow for one path
      val v1 = new FlowVertex(1, 2) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -2) // Sink

      // With 0 flow, path 1->3->4 is chosen, so we'll set the flow into 1 -> 2 -> 4
      val e1 = v1 ~> v2 ## (12, 0, 2, 0, 2)
      val e2 = v1 ~> v3 ## (13, 0, 2, 0, 0)
      val e3 = v2 ~> v4 ## (24, 0, 2, 0, 2)
      val e4 = v3 ~> v4 ## (34, 0, 2, 0, 0)

      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3, e4))

      When("computing the maximum flow")
      val maximumFlowNetwork = flowComputation.computeMaximumFlow(network, v1, v4)

      Then("edge e1 and e3 should still have flow 2")
      maximumFlowNetwork.get(e1).flow should be (2)
      maximumFlowNetwork.get(e3).flow should be (2)

    }

    /**
     * Test found during deployment (don't bother trying to read the graph). This graph triggers augmenting paths where
     * flow is reduced on some edges. The graph was lifted directly from the program, which is why an inserter is used
     * to add source/sink, keeping the graph data intact.
     *
     * The test has no validation, simply having the edges not throw exceptions from filling an edge with too much flow
     * is considered a success.
     */
    it ("should augment paths with negative flow") {
      val v0 = new FlowVertex(0, 6)
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, -12)
      val v3 = new FlowVertex(3, -24)
      val v4 = new FlowVertex(4, 12)
      val v5 = new FlowVertex(5, 0)
      val v6 = new FlowVertex(6, 24)
      val v7 = new FlowVertex(7, -6)
      //val id, lowerBound, capacity, cost, flow
      val e1 = v0 ~> v1 ## (0, 0, 7, 1, 0)
      val e2 = v0 ~> v1 ## (3, 0, 7, 1, 0)
      val e3 = v0 ~> v2 ## (2, 0, 7, 1, 0)
      val e4 = v0 ~> v7 ## (1, 0, 7, 1, 0)
      val e5 = v1 ~> v3 ## (12, 0, 7, 1, 0)
      val e6 = v1 ~> v7 ## (6, 0, 7, 1, 0)
      val e7 = v1 ~> v7 ## (17, 0, 7, 1, 0)
      val e8 = v1 ~> v7 ## (18, 0, 7, 1, 0)
      val e9 = v2 ~> v1 ## (14, 0, 7, 1, 0)
      val e10 = v2 ~> v1 ## (13, 0, 7, 1, 0)
      val e11 = v4 ~> v0 ## (22, 0, 7, 1, 0)
      val e12 = v4 ~> v0 ## (16, 0, 7, 1, 0)
      val e13 = v4 ~> v0 ## (4, 0, 7, 1, 0)
      val e14 = v4 ~> v5 ## (8, 0, 7, 1, 0)
      val e15 = v5 ~> v2 ## (10, 0, 7, 1, 0)
      val e16 = v5 ~> v2 ## (23, 0, 7, 1, 0)
      val e17 = v5 ~> v2 ## (15, 0, 7, 1, 0)
      val e18 = v6 ~> v4 ## (20, 0, 7, 1, 0)
      val e19 = v6 ~> v4 ## (7, 0, 7, 1, 0)
      val e20 = v6 ~> v5 ## (9, 0, 7, 1, 0)
      val e21 = v6 ~> v5 ## (19, 0, 7, 1, 0)
      val e22 = v7 ~> v3 ## (5, 0, 7, 1, 0)
      val e23 = v7 ~> v3 ## (21, 0, 7, 1, 0)
      val e24 = v7 ~> v3 ## (11, 0, 7, 1, 0)

      val network = Graph.from(Nil, List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24))
      val singleSourceSink = sourceSinkInserter.addSourceAndSink(network)
      val source = singleSourceSink.nodes.toOuter.toSet[FlowVertex].find(_.production > 0).get
      val sink = singleSourceSink.nodes.toOuter.toSet[FlowVertex].find(_.production < 0).get
      flowComputation.computeMaximumFlow(singleSourceSink, source, sink)
    }
  }
}
