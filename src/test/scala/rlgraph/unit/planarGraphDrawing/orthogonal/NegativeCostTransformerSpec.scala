package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.NegativeCostTransformer
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class NegativeCostTransformerSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val negativeCostTransformer = new NegativeCostTransformer()

  def flowNetwork = new {

    val v1 = new FlowVertex(1)
    val v2 = new FlowVertex(2)
    val v3 = new FlowVertex(3, 10)
    val v4 = new FlowVertex(4, 6)
    val v5 = new FlowVertex(5, 12)
    val v6 = new FlowVertex(6, 9)

    //                   (id, lower bound, capacity, cost, flow)
    val e1 = v1 ~> v2 ## (12, 0, 5, 10, 0)
    val e2 = v1 ~> v3 ## (13, 0, 3, -4, 0)
    val e3 = v2 ~> v3 ## (23, 0, 6, 1, 0)
    val e4 = v3 ~> v5 ## (35, 0, 7, -2, 0)
    val e5 = v5 ~> v3 ## (53, 0, 8, 3, 0)
    val e6 = v5 ~> v6 ## (56, 0, 7, -3, 0)
    val e7 = v4 ~> v3 ## (43, 0, 3, -2, 0)

    val network = Graph.from(Nil, List(e1, e2, e3, e4, e5, e6, e7))
  }

  describe("NegativeCostTransformer") {

    it ("should modify the cost of edges with negative cost") {

      Given("a network with edge e1 having positive cost 10, and e2 having cost -4")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("the edge e1 in the transformed network should still have cost 10")
      transformedNetwork.get(e1).cost should be (10)

      And("the edge e2 in the transformed network should have cost 4")
      transformedNetwork.get(e2).cost should be (4)

    }

    it ("should reverse edges with negative cost") {

      Given("a network with edge e1 having positive cost 10, and e2 having cost -4")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("the edge e2 should have its vertices reversed (v2 -> v1 instead of v1 -> v2)")
      val reversedE2 = transformedNetwork.get(e2)
      reversedE2.from should equal (transformedNetwork.get(v3))
      reversedE2.to should equal (transformedNetwork.get(v1))

      And("the edge e1 should still go from v1 to v2")
      val nonReversedE1 = transformedNetwork.get(e1)
      nonReversedE1.from should equal (transformedNetwork.get(v1))
      nonReversedE1.to should equal (transformedNetwork.get(v2))

    }

    it ("should subtract the capacity of an edge from its From-vertex production when reversing the edge cost") {

      Given("a network with vertex v4 having capacity 6 and an outgoing edge with negative cost and capacity 3")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("the vertex v4 should have production 6 - 3")
      transformedNetwork.get(v4).production should be (3)

    }

    it ("should add the capacity of an edge to its To-vertex production when reversing the edge cost") {

      Given("a network with vertex v6 having capacity 9 and an incoming edge with negative cost and capacity 7")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("the vertex v6 should have production 9 + 7")
      transformedNetwork.get(v6).production should be (16)

    }

    it ("should set the production of a vertex to the combined additions and subtractions when revering multiple edges connected to a single vertex") {

      Given("a network with vertex v3 having production 10, two negative incoming edges with capacity 3 and one outgoing negative edge with capacity 7")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("vertex v3 should have production 10 - 7 + 3 + 3 = 9")
      transformedNetwork.get(v3).production should be (9)

    }

    it ("should maintain the production for a vertex with an equal amount of incoming and outgoing capacity") {

      Given("a network with vertex v5 having production 12 and one incoming and one outgoing edge with capacity 7")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("vertex v5 should still have production 12")
      transformedNetwork.get(v5).production should be (12)

    }

    it ("should assign the ID of the edge with negative cost to the reversed edge") {

      Given("a network with the edge e2 having a negative cost")
      val f = flowNetwork
      import f._

      When("transforming negative cost edges")
      val transformedNetwork = negativeCostTransformer.removeNegativeCosts(network)

      Then("the reversed edge of e2 should have the same id as e2")
      val reversedE2 = transformedNetwork.get(e2)
      reversedE2.id should equal (e2.id)

    }
  }

}
