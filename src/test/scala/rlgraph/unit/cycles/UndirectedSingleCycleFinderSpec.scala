package rlgraph.unit.cycles

import net.cyndeline.rlgraph.cycles.UndirectedSingleCycleFinder
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class UndirectedSingleCycleFinderSpec extends SpecImports {
  private val cycleFinder = new UndirectedSingleCycleFinder[Int, UnDiEdge]()
  private val cycleOrder = new CycleOrderMatcher()

  // Should return true if a vertex is not in the seq.
  private def createFilter(f: Int*)(vertex: Int): Boolean = !f.contains(vertex)

  describe("UndirectedSingleCycleFinder") {

    it ("should find a cycle with arbitrary start in a graph with only a single cycle") {

      Given("a cycle")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle")
      val c = cycleFinder.findCycle(cycle)

      Then("the cycle 1, 2, 3, 4 should be found")
      c should be ('defined)
      assert(cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 2, 3, 4)))

    }

    it ("should find a cycle with arbitrary start in a graph with multiple cycles") {

      Given("a graph with two cycles 1, 2, 3, 4 and 1, 5, 6, 7")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 5~6, 6~7, 7~1)

      When("finding a cycle")
      val c = cycleFinder.findCycle(graph)

      Then("the cycle 1, 2, 3, 4 or 1, 5, 6, 7 should be found")
      c should be ('defined)
      assert(cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 2, 3, 4)) || cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 5, 6, 7)))

    }

    it ("should find a cycle with arbitrary start in a graph with non-closed paths") {

      Given("a cycle 1, 2, 3, 4 and the path 1, 5, 6")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 5~6)

      When("finding a cycle")
      val c = cycleFinder.findCycle(graph)

      Then("the cycle 1, 2, 3, 4 should be found")
      c should be ('defined)
      assert(cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 2, 3, 4)))

    }

    it ("should return None if a graph has no cycle with a arbitrary start") {

      Given("a tree")
      val graph = Graph(1~2, 2~3, 3~4, 3~5, 5~6)

      When("finding a cycle")
      val c = cycleFinder.findCycle(graph)

      Then("no cycle should be found")
      c should be (None)

    }

    it ("should return a cycle from a graph with a specified start") {

      Given("a cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle that starts with 3")
      val c = cycleFinder.findCycleFromVertex(3, cycle)

      Then("the cycle 1, 2, 3, 4 should be found")
      c should be ('defined)
      assert(cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 2, 3, 4)), "The cycle " + c.get + " did not match the graph topology in either direction.")

    }

    it ("should not return a cycle when starting at a vertex that isn't a member of one") {

      Given("a cycle 1, 2, 3, 4 and a path 1~5, 5~6")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 5~6)

      When("finding a cycle that starts with 6")
      val c = cycleFinder.findCycleFromVertex(6, graph)

      Then("no cycle should be found")
      c should be (None)

    }

    it ("should not traverse vertices that fails the vertex filter when finding arbitrary cycles") {

      Given("a cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle using a filter that forbids the vertex 4")
      val cycleFinder = new UndirectedSingleCycleFinder[Int, UnDiEdge](createFilter(4))
      val c = cycleFinder.findCycle(cycle)

      Then("no cycle should be found")
      c should be (None)

    }

    it ("should not traverse vertices that fails the vertex filter when finding cycles with a given start") {

      Given("a cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle using a filter that forbids the vertex 4, starting at vertex 2")
      val cycleFinder = new UndirectedSingleCycleFinder[Int, UnDiEdge](createFilter(4))
      val c = cycleFinder.findCycleFromVertex(2, cycle)

      Then("no cycle should be found")
      c should be (None)

    }

    it ("should traverse vertices that fails the vertex filter if the vertex is the start value") {

      Given("a cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle using a filter that forbids the vertex 4, starting at vertex 4")
      val cycleFinder = new UndirectedSingleCycleFinder[Int, UnDiEdge](createFilter(4))
      val c = cycleFinder.findCycleFromVertex(4, cycle)

      c should be ('defined)
      assert(cycleOrder.compareBothDirections(c.get.vertices, Vector(1, 2, 3, 4)), "The cycle " + c.get + " did not match the graph topology in either direction.")

    }

    it ("should position the start vertex first in the list when finding cycles with a specified start") {

      Given("a cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding a cycle that starts with 3")
      val c = cycleFinder.findCycleFromVertex(3, cycle)

      Then("the first element in the cycle list should be 3")
      assert(c.get.vertices.head == 3, "The first element in the cycle " + c.get + " was not the start vertex 3.")

    }

    it ("sdsd") {

      val g = Graph[Int, UnDiEdge](1, 2, 3, 4, 5, 1~2, 1~3, 1~4, 1~5, 2~3, 2~4, 2~5, 3~4, 3~5, 4~5)
      val c = cycleFinder.findCycle(g)
      assert(c.isDefined)
    }

    it ("should be deterministic when using a starting vertex") {

      Given("a graph with two cycles 1, 2, 3, 4 and 1, 2, 5, 6")
      When("finding a cycle that starts with 1 repeated 100 times")
      val allCycles = for (i <- 0 until 100) yield {
        val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~2, 2~5, 5~6, 6~1) // new graph created every time in case instantiation is random in some aspect
        cycleFinder.findCycleFromVertex(1, graph).get
      }
      val cyclePairs = allCycles.zip(allCycles.tail)

      Then("every cycle should be equal")
      for (pair <- cyclePairs) {
        if (pair._1 != pair._2)
          fail(pair._1 + " was not equal to " + pair._2)
      }

    }

    it ("should be deterministic when using an arbitrary starting vertex") {

      Given("a graph with two cycles 1, 2, 3, 4 and 1, 2, 5, 6")
      When("finding a cycle 100 times")
      val allCycles = for (i <- 0 until 100) yield {
        val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~2, 2~5, 5~6, 6~1) // new graph created every time in case instantiation is random in some aspect
        cycleFinder.findCycle(graph).get
      }
      val cyclePairs = allCycles.zip(allCycles.tail)

      Then("every cycle should be equal")
      for (pair <- cyclePairs) {
        if (pair._1 != pair._2)
          fail(pair._1 + " was not equal to " + pair._2)
      }

    }

  }
}
