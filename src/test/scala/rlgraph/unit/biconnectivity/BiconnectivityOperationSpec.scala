package rlgraph.unit.biconnectivity

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BiconnectivityOperationSpec extends SpecImports {
  private val planarity = new DemoucronEmbedding[Int, UnDiEdge]()
  private val componentSearch = new DFSComponentSearch[Int, UnDiEdge]()
  private val connecter = new BiconnectivityOperation[Int]()

  describe("BiconnectivityOperation") {

    it ("shouldn't modify an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the resulting graph should be empty")
      biconnectedGraph should equal (graph)

      And("no extra edges should be registered")
      result.extraEdges should be ('empty)

    }

    it ("shouldn't modify a graph with a single vertex") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the resulting graph should be unchanged")
      biconnectedGraph should equal (graph)

      And("no extra edges should be registered")
      result.extraEdges should be ('empty)

    }

    it ("shouldn't modify a graph with a single edge") {

      Given("a graph with a single edge")
      val graph = Graph(1~2)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the resulting graph should be unchanged")
      biconnectedGraph should equal (graph)

      And("no extra edges should be registered")
      result.extraEdges should be ('empty)

    }

    it ("shouldn't modify a graph with a single biconnected component containing multiple edges") {

      Given("a biconnected component")
      val graph = Graph(1~2, 1~3, 2~4, 3~4, 2~3)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the resulting graph should be unchanged")
      biconnectedGraph should equal (graph)

      And("no extra edges should be registered")
      result.extraEdges should be ('empty)

    }

    it ("should add a single edge between two neighbors of a cut vertex") {

      Given("a graph with a single cut vertex 1 having two neighbors 2 and 3")
      val graph = Graph(1~2, 1~3)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("there should be an edge between 2 and 3")
      biconnectedGraph should equal (graph + 2~3)

      And("the edge (2,3) should be registered as an extra")
      result.extraEdges.map(UnorderedPair(_)) should be (Vector(UnorderedPair(2, 3)))

    }

    it ("should biconnect a chain of vertices") {

      Given("a chain of vertices")
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 5~6, 6~7)

      When("biconnecting the graph")
      val biconnectedGraph = connecter.biconnect(graph).graph

      Then("the graph should pass validity conditions guaranteed by the algorithm")
      shouldBePlanar(biconnectedGraph)
      maximumDegreeIncreaseIs2(graph, biconnectedGraph)
      biconnected(biconnectedGraph)

    }

    it ("should biconnect a tree") {

      Given("a tree")
      val graph = Graph(1~2, 1~3, 2~4, 2~5, 3~6, 3~7, 4~8)

      When("biconnecting the graph")
      val biconnectedGraph = connecter.biconnect(graph).graph

      Then("the graph should pass validity conditions guaranteed by the algorithm")
      shouldBePlanar(biconnectedGraph)
      maximumDegreeIncreaseIs2(graph, biconnectedGraph)
      biconnected(biconnectedGraph)

    }

    it ("should biconnect a graph where cut points aren't adjacent") {

      Given("three biconnected components forming a chain with no adjacent cut points")
      val comp1 = Graph(1~2, 1~3, 2~4, 3~4, 2~3)
      val comp2 = Graph(4~5, 4~6, 5~7, 6~7, 5~6)
      val comp3 = Graph(7~8, 7~9, 8~10, 9~10, 8~9)
      val graph = comp1 ++ comp2 ++ comp3

      When("biconnecting the graph")
      val biconnectedGraph = connecter.biconnect(graph).graph

      Then("the graph should pass validity conditions guaranteed by the algorithm")
      shouldBePlanar(biconnectedGraph)
      maximumDegreeIncreaseIs2(graph, biconnectedGraph)
      biconnected(biconnectedGraph)

    }

    it ("should connect a tree with more than 2 regular edges connected to a cut point") {

      Given("a cut point with 5 edges (components) attached")
      val graph = Graph(1~2, 1~3, 1~4, 1~5)

      When("biconnecting the graph")
      val biconnectedGraph = connecter.biconnect(graph).graph

      Then("the graph should pass validity conditions guaranteed by the algorithm")
      shouldBePlanar(biconnectedGraph)
      maximumDegreeIncreaseIs2(graph, biconnectedGraph)
      biconnected(biconnectedGraph)

    }

    it ("should biconnect two disconnected vertices") {

      Given("a graph with two vertices and no edges")
      val graph = Graph[Int, UnDiEdge](1, 2)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the resulting graph should contain the edge 1~2")
      biconnectedGraph should equal (Graph(1~2))

      And("the edge (1,2) should be registered as an extra edge")
      result.extraEdges.map(UnorderedPair(_)) should equal (Vector(UnorderedPair(1, 2)))

    }

    it ("should biconnect two disconnected biconnected components") {

      Given("two disconnected biconnected components")
      val graph = Graph[Int, UnDiEdge](1~2, 3~4)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)
      val biconnectedGraph = result.graph

      Then("the graph should pass validity conditions guaranteed by the algorithm")
      shouldBePlanar(biconnectedGraph)
      maximumDegreeIncreaseIs2(graph, biconnectedGraph)
      biconnected(biconnectedGraph)

      And("every new edge in the graph should be in the extra edge set")
      val oldEdges = graph.edges.map(_.toOuter)
      val oldGraphEdges = oldEdges.map(e => UnorderedPair[Int](e._1, e._2)).toSet
      val newEdges = biconnectedGraph.edges.map(_.toOuter)
      val newGraphEdges = newEdges.map(e => UnorderedPair[Int](e._1, e._2)).toSet
      val extraEdges = result.extraEdges.map(UnorderedPair(_)).toSet
      (newGraphEdges -- oldGraphEdges) should equal (extraEdges)

    }

    it ("should return the biconnecting edges") {

      Given("a connected graph")
      val graph = Graph(1~2, 2~3, 3~4)

      When("biconnecting the graph")
      val result = connecter.biconnect(graph)

      Then("the result should contain the edge 1~4 or 1~3, 2~4")
      val edges = result.extraEdges.map(UnorderedPair(_))
      if (edges.size == 1)
        edges should be (Vector(UnorderedPair(1, 4)))
      else if (edges.size == 2) {
        edges should contain (UnorderedPair(1, 3))
        edges should contain (UnorderedPair(2, 4))
      } else {
        fail("The expected edges were not present: " + edges)
      }

    }

    it ("should construct the resulting graph deterministically on the same graph constructed multiple times") {

      Given("a non-biconnected graph that has triggered randomness prior")
      //val graph = Graph[Int, UnDiEdge](1, 2, 3, 4, 5, 6, 7, 8, 9, 5~2, 6~1, 6~3, 6~5, 8~7, 9~4, 9~6, 9~8)
      val graph = Graph(4~1, 4~3, 6~4, 7~2, 7~5, 7~6)

      When("biconnecting the graph 50 times with a new graph instance every time")
      val firstBiconnection = connecter.biconnect(graph)

      Then("the graph, nodeset and edgeset should be equal every time")
      for (i <- 0 to 100) {
        val newBiconnection = connecter.biconnect(Graph(4~1, 4~3, 6~4, 7~2, 7~5, 7~6))
        firstBiconnection.graph should equal (newBiconnection.graph)
        firstBiconnection.graph.nodes.toVector should equal (newBiconnection.graph.nodes.toVector)
        firstBiconnection.graph.edges.toVector should equal (newBiconnection.graph.edges.toVector)
      }

    }

  }

  /*
   * Validity checks that every graph must pass after becoming biconnected:
   *  - Still planar
   *  - No vertex has its degree increased by more than 2
   *  - Biconnected
   */

  private def shouldBePlanar(graph: Graph[Int, UnDiEdge]) {
    assert(planarity.isPlanar(graph), "The biconnected graph was not planar: " + graph)
  }
  private def maximumDegreeIncreaseIs2(oldGraph: Graph[Int, UnDiEdge], newGraph: Graph[Int, UnDiEdge]) {
    for (newNode <- newGraph.nodes) {
      val oldDegree = oldGraph.get(newNode).degree
      val newDegree = newGraph.get(newNode).degree
      assert(newDegree - oldDegree <= 2 && newDegree - oldDegree >= 0, "The degree in vertex " + newNode + " increased by more than 2 (" + (newDegree - oldDegree) + ").")
    }
  }
  private def biconnected(graph: Graph[Int, UnDiEdge]) {
    val biconnectedComponents = componentSearch.components(graph)
    assert(biconnectedComponents.size == 1, "The graph was not biconnected, the following components was found: " + biconnectedComponents.mkString(", "))
  }
}
