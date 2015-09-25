package rlgraph.unit.cycles

import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.cycles.directedCycles.JohnsonSimpleCycles
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class JohnsonSimpleCyclesSpec extends SpecImports {
  private val cycleFinder = new JohnsonSimpleCycles[Int]()

  describe("JohnsonSimpleCycles") {

    it ("shouldn't find cycles in an acyclic graph") {

      Given("an acyclic graph")
      val graph = Graph(1~>2, 2~>3, 3~>4, 1~>4)

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the result should be empty")
      cycles should be ('empty)

    }

    it ("should not find cycles in an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, DiEdge]()

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the result should be empty")
      cycles should be ('empty)

    }

    it ("should find a single cycle in a graph containing a single cycle") {

      Given("a graph consisting of a single cycle")
      val graph = Graph(1~>2, 2~>3, 3~>4, 4~>1)

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the single cycle 1, 2, 3, 4 should be found")
      cycles should have size 1
      cycles.head should be (Cycle(1, 2, 3, 4))

    }

    it ("should find two disconnected cycles") {

      Given("a graph with two disconnected cycles")
      val graph = Graph(1~>2, 2~>3, 3~>1, 4~>5, 5~>6, 6~>4)

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the cycles 1, 2, 3 and 4, 5, 6 should be found")
      cycles should have size 2
      cycles.toSet should be (Set(Cycle(1, 2, 3), Cycle(4, 5, 6)))

    }

    it ("should not find cycles in a tree") {

      Given("a tree")
      val graph = Graph(1~>2, 2~>3, 3~>4, 2~>5, 5~>6, 6~>7, 6~>8)

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the result should be empty")
      cycles should be ('empty)

    }

    it ("should find cycles sharing vertices") {

      Given("4 cycles sharing the vertices 1 and 5")
      val c1 = Graph(1~>2, 2~>5, 5~>3, 3~>1)
      val c2 = Graph(1~>6, 6~>5, 5~>7, 7~>1)
      val graph = c1 ++ c2

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the cycles [1, 2, 5, 3], [6, 5, 7, 1], [1, 2, 5, 7] and [1, 6, 5, 3] should be found")
      cycles should have size 4
      cycles.toSet should be (Set(Cycle(1, 2, 5, 3), Cycle(6, 5, 7, 1), Cycle(1, 2, 5, 7), Cycle(1, 6, 5, 3)))

    }

    it ("should discard cycles above a certain size") {

      Given("a graph with two cycles of length 3 and 4, and an algorithm with max size 3")
      val graph = Graph(1~>2, 2~>3, 3~>1, 3~>4, 4~>1)
      val cycleFinder = new JohnsonSimpleCycles[Int](3)

      When("parsing all cycles of the graph")
      val cycles = cycleFinder.findCycles(graph)

      Then("the single cycle 1, 2, 3 should be found")
      cycles should have size 1
      cycles.head should be (Cycle(1, 2, 3))

    }

  }

}
