package rlgraph.unit.util

import net.cyndeline.rlgraph.util.DeadEndTrimmer
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DeadEndTrimmerSpec extends SpecImports {

  describe("DeadEndTrimmer") {

    it ("should clear a graph with no cycles") {

      Given("a tree")
      val graph = Graph(1~2, 2~3, 3~4, 3~5)

      When("trimming the edges")
      val trimmedGraph = DeadEndTrimmer.trim(graph)

      Then("the result should be empty")
      assert(trimmedGraph.isEmpty, "The graph " + trimmedGraph + " was not empty.")

    }

    it ("should trim dead ends") {

      Given("a cycle with two dead ends connected")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)
      val deadEnd1 = Graph(1~5, 5~6)
      val deadEnd2 = Graph(3~7, 7~8)
      val graph = cycle ++ deadEnd1 ++ deadEnd2

      When("trimming the edges")
      val trimmedGraph = DeadEndTrimmer.trim(graph)

      Then("the result should only contain the cycle")
      trimmedGraph should equal (cycle)

    }
  }
}
