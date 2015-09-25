package rlgraph.unit.subgraph

import net.cyndeline.rlgraph.subgraph.stronglyConnectedComponents.TarjansAlgorithm
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TarjansAlgorithmSpec extends SpecImports {
  private val algorithm = new TarjansAlgorithm()

  describe("TarjansAlgorithm") {

    it ("should return an empty result if the input graph is empty") {

      Given("an empty graph")
      val graph = Graph[Int, DiEdge]()

      When("computing the strongly connected components")
      val sccs = algorithm.sccComponents(graph)

      Then("the result should be empty")
      sccs should be ('empty)

    }

    it ("should assign a single vertex to its own component") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, DiEdge](1)

      When("computing the strongly connected components")
      val sccs = algorithm.sccComponents(graph)

      Then("the vertex should be its own component")
      sccs should be (Vector(Vector(1)))

    }

    it ("should assign multiple vertices to their own components") {

      Given("a graph with the vertices 1, 2 and 3 not having paths to each other")
      val graph = Graph(1~>2, 2~>3)

      When("computing the strongly connected components")
      val sccs = algorithm.sccComponents(graph)

      Then("three components should be found")
      sccs should have size 3

      And("each vertex should be in its own component")
      sccs should contain (Vector(1))
      sccs should contain (Vector(2))
      sccs should contain (Vector(3))

    }

    it ("should assign vertices of a cycle to a single component") {

      Given("a cycle 1, 2, 3")
      val graph = Graph(1~>2, 2~>3, 3~>1)

      When("computing the strongly connected components")
      val sccs = algorithm.sccComponents(graph)

      Then("a single component should be found")
      sccs should have size 1

      And("the component should contain 1, 2 and 3")
      sccs.head should contain (1)
      sccs.head should contain (2)
      sccs.head should contain (3)

    }

    it ("should assign vertices of disjoint cycles to different components") {

      Given("the cycles 1, 2, 3 and 4, 5, 6, with an edge from 4 to 3")
      val graph = Graph(1~>2, 2~>3, 3~>1, 4~>3, 4~>5, 5~>6, 6~>4)

      When("computing the strongly connected components")
      val sccs = algorithm.sccComponents(graph)

      Then("two components should be found")
      sccs should have size 2

      And("one of them should contain 1, 2, 3")
      assert(sccs.exists(c => c.toSet == Set(1, 2, 3)))

      And("one of them should contain 4, 5, 6")
      assert(sccs.exists(c => c.toSet == Set(4, 5, 6)))

    }

  }
}
