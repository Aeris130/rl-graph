package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.MaxFlowMinCost.MaxFlowMinCost
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Since the class is mostly a compilation of other algorithms, only a few general test cases are covered.
 */
class MaxFlowMinCostSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val maxFlowMinCost = new MaxFlowMinCost()

  describe("MaxFlowMinCost") {

    it ("should fill every edge up to its minimum bound") {

      Given("a network with multiple paths, where all flow can be sent along either path")
      val v1 = new FlowVertex(1, 6) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -6) // Sink

      /* All edges has capacity 6, the amount being sent. There are three paths to sink v4:
       *
       *  1 -> 2 -> 4 | Minimum bound 1 in e1
       *  1 -> 3 -> 4 | Minimum bound 2 in e3
       *  1 ------> 4 | Minimum bound 3 in e5
       *
       *  Since the total bound D is 6, each edge must receive the exact amount expected.
       */
      val e1 = v1 ~> v2 ## (12, 1, 6, 0, 0)
      val e2 = v1 ~> v3 ## (13, 0, 6, 0, 0)
      val e3 = v3 ~> v4 ## (34, 2, 6, 0, 0)
      val e4 = v2 ~> v4 ## (24, 0, 6, 0, 0)
      val e5 = v1 ~> v4 ## (14, 3, 6, 0, 0)

      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3, e4, e5))

      When("computing the maximum flow using edges whose minimum bound forces that exact amount of flow through it")
      val maximumFlowNetwork = maxFlowMinCost.computeMaxFlowMinCost(network)

      Then("edges e1, e3 and e5 should have the same amount of flow in them as their minimum bound")
      maximumFlowNetwork.get(e1).flow should be (1)
      maximumFlowNetwork.get(e3).flow should be (2)
      maximumFlowNetwork.get(e5).flow should be (3)

    }

    /**
     * Example taken from figure 2(a):
     *    http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=minimumCostFlow2
     *
     * This test is also present in the cycle-canceling spec, but this time all edges start at 0 flow,
     * checking if the algorithm can find the solution from scratch instead of a maximal non-optimal flow.
     */
    it ("should compute minim cost flow for a complex example") {

      Given("a graph with a sub-optimal cost flow")
      val v1 = new FlowVertex(1, 5)
      val v2 = new FlowVertex(2, 2)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, -2)
      val v5 = new FlowVertex(5, -4)
      val v6 = new FlowVertex(6, -1)

      val a = v1 ~> v2 ## (12, 0, 3, 1, 0)
      val b = v1 ~> v3 ## (13, 0, 3, 4, 0)
      val c = v2 ~> v3 ## (23, 0, 7, 2, 0)
      val d = v3 ~> v5 ## (35, 0, 7, 5, 0)
      val e = v3 ~> v4 ## (34, 0, 5, 2, 0)
      val f = v4 ~> v5 ## (45, 0, 3, 1, 0)
      val g = v3 ~> v6 ## (36, 0, 1, 8, 0)

      val network = Graph.from(List(v1, v2, v3, v4, v5, v6), List(a, b, c, d, e, f, g))

      When("computing the minimum cost flow")
      val minimumCostFlow = maxFlowMinCost.computeMaxFlowMinCost(network)

      Then("the flow on each edge should match the optimal solution (see figure 2(f) in the link)")
      minimumCostFlow.get(a).flow should be (3)
      minimumCostFlow.get(b).flow should be (2)
      minimumCostFlow.get(c).flow should be (5)
      minimumCostFlow.get(d).flow should be (1)
      minimumCostFlow.get(e).flow should be (5)
      minimumCostFlow.get(f).flow should be (3)
      minimumCostFlow.get(g).flow should be (1)

    }

    it ("should keep loops at flow 0 if no minimum bound exists") {

      Given("a network with a loop")
      val v1 = new FlowVertex(1, 6) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -6) // Sink

      val e1 = v1 ~> v2 ## (12, 1, 6, 0, 0)
      val e2 = v2 ~> v3 ## (23, 0, 6, 0, 0)
      val loop = v2 ~> v2 ## (34, 0, 6, 0, 0)

      val network = Graph.from(List(v1, v2, v3), List(e1, e2, loop))

      When("computing the minimum cost flow")
      val minimumCostFlow = maxFlowMinCost.computeMaxFlowMinCost(network)

      Then("the loop should have 0 flow")
      minimumCostFlow.get(loop).flow should be (0)

    }

    it ("should keep loops at flow == minimum bound, if one exists") {

      Given("a network with a loop")
      val v1 = new FlowVertex(1, 6) // Source
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -6) // Sink

      val e1 = v1 ~> v2 ## (12, 1, 6, 0, 0)
      val e2 = v2 ~> v3 ## (23, 0, 6, 0, 0)
      val loop = v2 ~> v2 ## (34, 3, 6, 0, 0)

      val network = Graph.from(List(v1, v2, v3), List(e1, e2, loop))

      When("computing the minimum cost flow")
      val minimumCostFlow = maxFlowMinCost.computeMaxFlowMinCost(network)

      Then("the loop should have 0 flow")
      minimumCostFlow.get(loop).flow should be (3)

    }

  }
}
