package rlgraph.unit.pathfinding

import net.cyndeline.rlgraph.pathfinding.TopologicalDAGSort
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TopologicalDAGSortSpec extends SpecImports {
  private val sort = new TopologicalDAGSort[Int]()

  describe("TopologicalDAGSort") {

    it ("should throw an exception if the supplied graph has vertices with no in-degree, but also a cycle") {

      Given("a graph with vertex 1 having in-degree 0 and 2,3,4 forming a cycle")
      val graph = Graph(1~>2, 2~>3, 3~>4, 4~>2)

      When("sorting the graph")
      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        sort.order(graph)
      }

      thrown.getMessage should be ("The supplied graph " + graph + " was not acyclic.")
    }

    it ("should throw an exception if the supplied graph has no vertices with 0 in-degree") {

      Given("a cycle 1, 2, 3")
      val graph = Graph(1~>2, 2~>3, 3~>1)

      When("sorting the graph")
      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        sort.order(graph)
      }

      thrown.getMessage should be ("The supplied graph " + graph + " was not acyclic.")
    }

    it ("should order a graph with no vertices") {

      Given("an empty graph")
      val graph = Graph[Int, DiEdge]()

      When("sorting the graph")
      val order = sort.order(graph)

      Then("the order should be empty")
      order should be ('empty)

    }

    it ("should order a graph with a single vertex") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, DiEdge](1)

      When("sorting the graph")
      val order = sort.order(graph)

      Then("the single vertex should be returned")
      order should be (Vector(1))

    }

    it ("should order a graph with multiple vertices") {

      Given("a graph with three topological levels [7, 5, 3] [8, 11] [2, 9, 10]")
      val graph = Graph(7~>11, 7~>8, 5~>11, 3~>8, 11~>2, 11~>9, 11~>10, 8~>9, 3~>10)

      When("sorting the graph")
      val order = sort.order(graph)

      Then("the first three elements should be 3, 5 and 7")
      Set(order(0), order(1), order(2)) should equal (Set(3, 5, 7))

      And("the next two elements should be 8 and 11")
      Set(order(3), order(4)) should equal (Set(8, 11))

      And("the last three elements should be 2, 9 and 10")
      Set(order(5), order(6), order(7)) should equal (Set(2, 9, 10))

    }
  }
}
