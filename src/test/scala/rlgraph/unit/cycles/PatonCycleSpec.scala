package rlgraph.unit.cycles

import net.cyndeline.rlgraph.cycles.cycleBase.jGraphT.PatonCycle
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class PatonCycleSpec extends SpecImports {
  private val cycleDetector = new PatonCycle()
  private val orderMatcher = new CycleOrderMatcher()

  describe("PatonCycle") {

    it ("should detect a single cycle") {

      Given("a graph with a single cycle")
      val graph = Graph(1~2, 2~3, 3~1)

      When("computing all cycles")
      val cycles = cycleDetector.findCycleBase(graph)

      Then("a cycle containing the vertices 1, 2 and 3 should be found")
      cycles.size should be (1)
      cycles.head.size should be (3)
      val expected = Vector(1, 2, 3)
      assert(orderMatcher.compareBothDirections(cycles.head, expected), "The cycle " + cycles.head + "did not contain vertices in any order of " + expected)

    }

    it ("should detect cycles that share vertices and edges") {

      Given("a graph with the cycles 1, 2, 3, 4 and 1, 2, 3 and 2, 3, 4")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 2~4)

      When("computing all cycles")
      val cycles = cycleDetector.findCycleBase(graph)

      Then("3 cycles should be found")
      cycles should have size 2

      And("a cycle 1, 2, 3 should exist")
      val expected1 = Vector(1, 2, 4)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expected1)), "The list of cycles " + cycles + " did not contain a cycle with order " + expected1)

      And("a cycle 2, 3, 4 should exist")
      val expected2 = Vector(2, 4, 3)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expected2)), "The list of cycles " + cycles + " did not contain a cycle with order " + expected2)

    }

    it ("should detect cycles on other cycles") {

      Given("a cycle with additional cycles on it, sharing two vertices")
      val mainCycle = Graph(1~2, 2~3, 3~4, 4~5, 5~6, 6~7, 7~1)
      val additionalCycles = Graph(1~10, 2~10, 2~11, 3~11, 5~12, 6~12)
      val finalGraph = mainCycle union additionalCycles

      When("computing all cycles")
      val cycles = cycleDetector.findCycleBase(finalGraph)

      Then("4 cycles should be found")
      cycles should have size 4

      And("the main cycles 1 -> 7 should be present")
      val expectedMain = Vector(1, 2, 3, 4, 5, 6, 7)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expectedMain)), "The list of cycles " + cycles + " did not contain a cycle with order " + expectedMain)

      And("the cycles 1, 2, 10 & 2, 3, 11 and 5, 6, 12 should be present")
      val expected1 = Vector(1, 2, 10)
      val expected2 = Vector(2, 3, 11)
      val expected3 = Vector(5, 6, 12)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expected1)), "The list of cycles " + cycles + " did not contain a cycle with order " + expected1)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expected2)), "The list of cycles " + cycles + " did not contain a cycle with order " + expected2)
      assert(cycles.exists(c => orderMatcher.compareBothDirections(c, expected3)), "The list of cycles " + cycles + " did not contain a cycle with order " + expected3)

    }

  }
}
