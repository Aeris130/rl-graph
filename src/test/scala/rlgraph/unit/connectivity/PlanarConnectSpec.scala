package rlgraph.unit.connectivity

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.connectivity.PlanarConnect
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class PlanarConnectSpec extends SpecImports {

  describe("PlanarConnect") {

    it ("should not modify a connected graph") {

      Given("a connected graph")
      val graph = Graph(1~2, 2~3)

      When("connecting the graph")
      val connected = new PlanarConnect(graph)

      Then("the resulting graph should equal the input")
      connected.graph should equal (graph)

      And("no extra edges should be added")
      connected.extraEdges should be (Vector())

    }

    it ("should connect two disconnected vertices") {

      Given("a graph with two vertices")
      val graph = Graph[Int, UnDiEdge](1, 2)

      When("connecting the graph")
      val connected = new PlanarConnect(graph)

      Then("the resulting graph should contain the edge (1, 2)")
      connected.graph should be (Graph(1~2))

      And("the added edges should be (1, 2)")
      connected.extraEdges.map(e => UnorderedPair(e)) should be (Vector(UnorderedPair(1, 2)))

    }

    it ("should connect two disconnected biconnected components") {

      Given("a graph with two biconnected components with no shared vertices")
      val graph = Graph(1~2, 3~4)

      When("connecting the graph")
      val connected = new PlanarConnect(graph)

      Then("the resulting graph should be connected")
      connected.graph.isConnected should be (true)

      Then("the resulting graph should have 3 edges")
      connected.graph.edges.size should be (3)
      connected.extraEdges should have size 1


    }

    it ("should prioritize vertices with low degree when adding edges") {

      Given("a graph with two biconnected components, and vertices 1 and 6 having the lowest degrees")
      val component1 = Graph(1~2, 2~3, 2~4, 2~5, 3~4, 3~5, 4~5, 5~1)
      val component2 = Graph(6~7, 7~8, 7~9, 7~10, 7~11, 8~9, 8~10, 8~11, 9~10, 10~11, 11~6)
      val graph = component1 ++ component2

      When("connecting the graph")
      val connected = new PlanarConnect(graph)

      Then("the resulting graph should have an edge between 1 and 6")
      connected.graph should equal (graph + 1~6)

    }
  }
}
