package rlgraph.unit.cycles

import net.cyndeline.rlgraph.cycles.{Cycle, DirectedCycleFinder}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DirectedCycleFinderSpec extends SpecImports {
  private val cycleFinder = new DirectedCycleFinder[Int, DiEdge](100)

  describe("DirectedCycleFinder") {

    it ("should find a cycle in a graph containing a single cycle") {

      Given("a cycle 1, 2, 3")
      val graph = Graph(1~>2, 2~>3, 3~>1)

      When("finding all cycles")
      val allCycles = cycleFinder.findCycles(graph)

      Then("the single cycle should be found")
      allCycles should have size 1
      allCycles.head should be (Cycle(1, 2, 3))

    }

    it ("should find cycles sharing an edge") {

      Given("a graph with two cycles sharing the edge 1~2")
      val graph = Graph(1~>2, 2~>3, 3~>1, 2~>4, 4~>1)

      When("finding all cycles")
      val allCycles = cycleFinder.findCycles(graph)

      Then("the both cycles should be found")
      allCycles should have size 2
      allCycles.toSet should be (Set(Cycle(1, 2, 3), Cycle(1, 2, 4)))

    }

    it ("should return an empty result when parsing an acyclic graph") {

      Given("an acyclic graph")
      val graph = Graph(1~>2, 2~>3)

      When("finding all cycles")
      val allCycles = cycleFinder.findCycles(graph)

      Then("no cycles should be found")
      allCycles should be ('empty)

    }

    it ("should compute cycles deterministically") {

      Given("a graph with multiple cycles")
      val graph = Graph(1~>2, 2~>3, 3~>4, 4~>1, 2~>5, 5~>6, 6~>7, 7~>5, 1~>8, 8~>9, 9~>1, 9~>10, 10~>5, 10~>2)

      When("computing the cycle set multiple times")
      val initialCycleSet = cycleFinder.findCycles(graph)

      Then("each set should equal the first")
      for (i <- 0 to 100) {
        val cycles = cycleFinder.findCycles(graph)

        for (i <- 0 until initialCycleSet.size) {
          val initialCycle = initialCycleSet(i)
          val newCycle = cycles(i)

          // Important to check vertices and not equality, as it doesn't take vertex order into consideration
          initialCycle.vertices should equal (newCycle.vertices)
        }
      }

    }
  }

}
