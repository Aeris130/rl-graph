package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowVertex, _}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class ResidualNetworkSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

  def infiniteCapacity = new {
    val v1 = new FlowVertex(1, 1)
    val v2 = new FlowVertex(2, -1)
    val e1 = v1 ~> v2 ## (12, 0, -1, 0, 0)
    val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2), List(e1))
  }

  def singlePathWithFlowEqualToLowerBound = new {
    val v1 = new FlowVertex(1, 1)
    val v2 = new FlowVertex(2, -1)
    val e1 = v1 ~> v2 ## (12, 1, 3, 10, 1)
    val g: Graph[FlowVertex, FlowEdge] = Graph.from(Nil, List(e1))
    val residual = new ResidualNetwork(g, v1, v2)
  }

  describe("ResidualNetwork") {

    it ("should create forward and backwards edges when initializing a network") {

      Given("a graph with two edges, one of which has flow")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // 2 edges, with capacity 5 and 8
      val e1 = v1 ~> v2 ## (12, 0, 5, 1, 0)
      val e2 = v2 ~> v3 ## (13, 0, 8, 1, 2)

      val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("initializing a residual network")
      val residualNetwork = new ResidualNetwork(g, v1, v3)

      Then("edge 2 should have flow = 2")
      residualNetwork.getEdgeFlow(e2) should be (2)

      And("the no-flow edge e1 should have a single forward entry with value 5")
      // The first edge is the forward one
      val residualEdgesE1: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residualNetwork.residualValues(e1)
      val forwardE1 = residualEdgesE1._1

      assert(residualEdgesE1._2.isEmpty, "the backwards edge was not empty despite there being no flow in edge " + e1)
      assert(forwardE1.isDefined, "The edge " + e1 + " did not have a forward residual edge.")
      forwardE1.get.flow should be (5)

      And("the edge e2 should have a forward edge of value 8 - 2, and a backward edge with value 2")
      val residualEdgesE2 = residualNetwork.residualValues(e2)
      val forwardE2 = residualEdgesE2._1
      val backE2 = residualEdgesE2._2

      assert(forwardE2.isDefined, "The edge " + e2 + " did not have a forward residual edge.")
      assert(backE2.isDefined, "The edge " + e2 + " did not have a backward residual edge.")

      forwardE2.get.flow should be (6)
      backE2.get.flow should be (2)

    }

    it ("should set up edges with infinite capacity using flow -1") {

      Given("a network with an edge having infinite capacity")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // 2 edges, one with infinite capacity, and one with 8 . Network is still legal since the entire path isn't unbounded
      val e1 = v1 ~> v2 ## (12, 0, -1, 1, 0)
      val e2 = v2 ~> v3 ## (13, 0, 8, 1, 0)

      val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("constructing the residual network")
      val residualNetwork = new ResidualNetwork(g, v1, v3)

      Then("edge e1 should have forward flow -1")
      val residualPairs: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residualNetwork.residualValues(e1)

      assert(residualPairs._1.isDefined, "No forward edge present.")
      residualPairs._1.get.flow should be (-1)

    }

    it ("should set flow for a specified edge") {

      Given("a residual network made from a graph with an edge and no flow")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val e1 = v1 ~> v2 ## (12, 0, 5, 0, 0) // Capacity (5) and flow (last 0) is all that matters
      val graph = Graph.from(List(v1, v2), List(e1))
      val residualNetwork = new ResidualNetwork(graph, v1, v2)

      When("setting a new flow")
      residualNetwork.setFlow(e1, 4)

      Then("the network should report a new flow")
      residualNetwork.getEdgeFlow(e1) should be (4)

      And("new forward and backward edges should exist")
      val residualEdgesE1: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residualNetwork.residualValues(e1)
      val forwardE1 = residualEdgesE1._1
      val backE1 = residualEdgesE1._2

      assert(forwardE1.isDefined, "The edge " + e1 + " did not have a forward residual edge.")
      assert(backE1.isDefined, "The edge " + e1 + " did not have a backward residual edge.")

      forwardE1.get.flow should be (1)
      backE1.get.flow should be (4)

    }

    it ("should not set backwards edges if the flow amount is equal or lower than the lower bound") {

      Given("a network with two edges having lower bounds 2 and 3")
      val v1 = new FlowVertex(1, 4)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, -4)

      val e1 = v1 ~> v2 ## (12, 2, 10, 0, 0)
      val e2 = v2 ~> v3 ## (13, 3, 10, 0, 0)

      val graph = Graph.from(List(v1, v2, v3), List(e1, e2))
      val residualNetwork = new ResidualNetwork(graph, v1, v2)

      When("adding 2 units of flow to e1 and 1 unit of flow to e2")
      residualNetwork.setFlow(e1, 2)
      residualNetwork.setFlow(e2, 1)

      Then("e1 should not have a backward edge")
      residualNetwork.residualValues(e1)._2 should be (None)

      And("e2 should not have a backward edge")
      residualNetwork.residualValues(e2)._2 should be (None)

    }

    /**
     * Since no backward edge will be present if the flow is <= the lower bound, this tests ensures that the
     * residual network doesn't rely on backwards edges to report flow.
     */
    it ("should report flow correctly if an edge contains a lower bound") {

      Given("a network with an edge having a lower bound and a flow that is higher than the bound")
      val v1 = new FlowVertex(1, 10)
      val v2 = new FlowVertex(2, -10)

      // flow = 6, lb = 2
      val e1 = v1 ~> v2 ## (12, 2, 10, 0, 6)

      val graph = Graph.from(List(v1, v2), List(e1))
      val residualNetwork = new ResidualNetwork(graph, v1, v2)

      When("retrieving the flow from the edge")
      val flow = residualNetwork.getEdgeFlow(e1)

      Then("the flow should be 10 - (10 - 6)")
      flow should be (6)

    }

    /**
     * If lb == capacity, the flow must have the same value. Since no more flow can be sent across the edge, there
     * won't be any forward arcs in the residual network. Since no flow can be canceled, there won't be any arcs
     * backward either. The network must be able to report correct flow without either edge.
     */
    it ("should report flow correctly when lower bound of an edge == capacity") {

      Given("a network with an edge having a lower bound and a flow that is equal to the capacity and the lower bound")
      val edgeValue = 10
      val v1 = new FlowVertex(1, edgeValue)
      val v2 = new FlowVertex(2, -edgeValue)

      // lb = capacity = flow = 10
      val e1 = v1 ~> v2 ## (12, edgeValue, edgeValue, 0, edgeValue)

      val graph = Graph.from(List(v1, v2), List(e1))
      val residualNetwork = new ResidualNetwork(graph, v1, v2)

      When("retrieving the flow from the edge")
      val flow = residualNetwork.getEdgeFlow(e1)

      Then("the flow should be 10")
      flow should be (edgeValue)

    }

    it ("should set negative costs for backwards edges") {

      Given("a network with an edge that has cost 4")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val e1 = v1 ~> v2 ## (12, 0, 0, 4, 2) // Flow 2 means there will be a backwards edge
      val graph = Graph.from(List(v1, v2), List(e1))
      val residualNetwork = new ResidualNetwork(graph, v1, v2)

      When("retrieving the cost for the backwards edge")
      val residualEdgesE1: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residualNetwork.residualValues(e1)
      val back = residualEdgesE1._2
      back.get.cost should be (-4)

    }

    it ("should compute a path with available capacity from source to sink if one exists") {

      Given("a network with a path 1->2->3 and a dead end 2->4")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, 0)

      // 2 edges, with capacity 4 and no flow
      val e1 = v1 ~> v2 ## (12, 0, 4, 0, 0)
      val e2 = v2 ~> v3 ## (13, 0, 4, 0, 0)

      // And a dead end
      val e3 = v2 ~> v4 ## (14, 0, 4, 0, 0)

      val graph = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3))
      val residualNetwork = new ResidualNetwork(graph, v1, v3)

      When("coputing a path")
      val path = residualNetwork.augmentingPath

      Then("the path should be 1->2->3")
      assert(path.isDefined, "No path was found")
      val p = path.get
      p.edges.size should be (2)

      p.edges(0).owner should equal (e1)
      p.edges(0).dir should be (true)
      p.edges(1).owner should equal (e2)
      p.edges(1).dir should be (true)

    }

    it ("should return no path if one isn't available") {

      Given("a graph with only an outgoing edge from the source")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // 2 edges, with capacity 5 and 8
      val e1 = v1 ~> v2 ## (12, 0, 5, 0, 0)
      val e2 = v2 ~> v3 ## (13, 0, 8, 0, 8) // Causes there to only be a backwards edge between 2 and 3

      val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2, v3), List(e1, e2))
      val residualNetwork = new ResidualNetwork(g, v1, v3)

      When("computing a path between 1 and 3")
      val path = residualNetwork.augmentingPath

      Then("no path should be found")
      path should be (None)

    }

    it ("should apply its flow onto a flow network") {

      Given("a flow network with no flow and its residual representation")
      val v1 = new FlowVertex(1, 2)
      val v2 = new FlowVertex(2, -1)
      val v3 = new FlowVertex(3, -1)

      // 2 edges, with capacity 5 and 8
      val e1 = v1 ~> v2 ## (12, 0, 5, 0, 0)
      val e2 = v2 ~> v3 ## (23, 0, 8, 0, 0)
      val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2, v3), List(e1, e2))
      val residual = new ResidualNetwork(g, v1, v2)

      When("adding a flow to the residual network and applying it")
      residual.setFlow(e1, 2)
      residual.setFlow(e2, 5)
      val missingEdge = v1 ~> v3 ## (13, 0, 8, 0, 0)
      val toApply = g + missingEdge
      val appliedNetwork = residual.applyToNetwork(toApply)

      Then("the resulting network should have flow added onto the edges that are present in the residual representation")
      appliedNetwork.get(e1).flow should be (2)
      appliedNetwork.get(e2).flow should be (5)

      And("the missing edge should still be present and have flow 0")
      appliedNetwork.get(missingEdge).flow should be (0)

    }

    it ("should set residual edges with infinite capacity as flow -1") {

      Given("a network with an edge having infinite capacity")
      val f = infiniteCapacity
      import f._

      When("constructing the residual network")
      val residual = new ResidualNetwork(g, v1, v2)
      val residualEdges: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residual.residualValues(e1)

      Then("the infinite edge should have forward flow -1")
      val forwardEdge: Option[ResidualEdge[Int]] = residualEdges._1
      forwardEdge.get.flow should be (-1)

      And("a backward edge shouldn't exist")
      residualEdges._2 should be (None)

    }

    it ("should set and get flow for edges with infinite capacity") {
      val setAmount = 24

      Given("a network with an edge having infinite capacity")
      val v1 = new FlowVertex(1, 1)
      val v2 = new FlowVertex(2, -1)
      val e1 = v1 ~> v2 ## (12, 0, -1, 0, 0)
      val g: Graph[FlowVertex, FlowEdge] = Graph.from(List(v1, v2), List(e1))
      val residual = new ResidualNetwork(g, v1, v2)

      When("adding flow to the residual network")
      residual.setFlow(e1, setAmount)
      val residualEdges: (Option[ResidualEdge[Int]], Option[ResidualEdge[Int]]) = residual.residualValues(e1)

      Then("the flow of edge e1 should be equal to the amount set")
      residual.getEdgeFlow(e1) should be (setAmount)

      And("the potential for more flow (the forward edge v1 -> v2) should still be -1")
      val forward = residualEdges._1
      assert(residualEdges._1.isDefined, "No forward edge found.")
      assert(forward.get.isInfinite, "The forward residual edge " + forward.get + " was not infinite.")

      And("the backward edge should reflect the set flow")
      assert(residualEdges._2.isDefined, "No backward edge found.")
      residualEdges._2.get.flow should be (setAmount)

    }

    it ("should compute a residual network where edges with flow equal to the lower bound still have backward edges") {

      Given("a network with an edge having flow equal to the lower bound")
      val f = singlePathWithFlowEqualToLowerBound
      import f._

      When("computing forward and backwards edges")
      val residualWithoutLowerBound: Graph[Int, ResidualEdge] = residual.networkThatIgnoresLowerBounds

      Then("there should be a backward edge from v2 to v1 representing the amount of flow that can be canceled until it reaches 0")
      val backward = residualWithoutLowerBound.edges.find(e => e.from == v2.id && e.to == v1.id).get
      backward.cost should be (-10)
      backward.dir should be (false)
      backward.flow should be (1)
      backward.owner should be (e1)

    }

    it ("should add forward edges to networks where edges with flow equal to the lower bound still have backward edges") {

      Given("a network with an edge having capacity 3 and flow 1")
      val f = singlePathWithFlowEqualToLowerBound
      import f._

      When("computing forward and backwards edges")
      val residualWithoutLowerBound: Graph[Int, ResidualEdge] = residual.networkThatIgnoresLowerBounds

      Then("there should be a forward edge from v1 to v2 with the amount of flow that can still be sent (3 - 1)")
      val forward = residualWithoutLowerBound.edges.find(e => e.from == v1.id && e.to == v2.id).get
      forward.cost should be (10)
      forward.dir should be (true)
      forward.flow should be (2)
      forward.owner should be (e1)
    }

  }
}
