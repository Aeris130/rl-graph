package rlgraph.unit.pathfinding.disjointElementaryCycle

import net.cyndeline.rlgraph.cycles.{Cycle, SingleCycleFinder}
import net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help.ElementaryCycleFinder
import net.cyndeline.rlgraph.triangulation.minimumDegree.faceTriangulation.multiEdgePrevention.help.VisitedVertex
import net.cyndeline.rlgraph.util.{GraphConverter, GraphConverterI}
import org.scalamock.scalatest.MockFactory
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class ElementaryCycleFinderSpec extends SpecImports with MockFactory {
  private val elementaryCycleFinder = new ElementaryCycleFinder[Int, UnDiEdge]()
  private val cycleOrder = new CycleOrderMatcher()
  private val graphTypeConv = GraphConverter.toUndirectedWithNodes[Int, VisitedVertex[Int], UnDiEdge]((v: Int) => VisitedVertex(v))

  describe("ElementaryCycleFinder") {

    it ("should find a cycle in a graph consisting of a single cycle") {

      Given("a graph with the cycle 1, 2, 3, 4")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)

      When("finding an elementary cycle")
      val elementaryCycle = elementaryCycleFinder.findCycle(cycle)

      Then("the cycle 1, 2, 3, 4 should be found")
      elementaryCycle should be ('defined)
      val expected = Vector(1, 2, 3, 4)
      assert(cycleOrder.compareBothDirections(elementaryCycle.get, expected), "The cycle " + elementaryCycle.get + " did not match " + expected)

    }

    it ("should return None when no cycle is present") {

      Given("a graph with no cycles")
      val graph = Graph(1~2, 2~3, 3~4)

      When("finding an elementary cycle")
      val elementaryCycle = elementaryCycleFinder.findCycle(graph)

      Then("the result should be None")
      elementaryCycle should be (None)

    }

    it ("should ignore dead ends") {

      Given("a graph with a cycle with dead ends connected to it")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)
      val deadEnds = Graph(1~5, 5~6, 4~7, 3~8)
      val graph = cycle ++ deadEnds

      When("finding an elementary cycle")
      val elementaryCycle = elementaryCycleFinder.findCycle(graph)

      Then("the cycle 1, 2, 3, 4 should be found")
      elementaryCycle should be ('defined)
      val expected = Vector(1, 2, 3, 4)
      assert(cycleOrder.compareBothDirections(elementaryCycle.get, expected), "The cycle " + elementaryCycle.get + " did not match " + expected)

    }

    it ("should append sub cycles to the main cycle") {

      Given("a graph with the cycle 1, 2, 3, 4 and the sub cycle 2, 5, 6")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 2~5, 5~6, 6~2)

      When("finding an elementary cycle")
      val elementaryCycle = elementaryCycleFinder.findCycle(graph)

      Then("the resulting cycle should contain both cycles")
      // Doesn't matter which cycles ends up as main first, the result is the same. But since either cycle can go in
      // two directions, two results are possible.
      elementaryCycle should be ('defined)
      val expected1 = Vector(1, 2, 5, 6, 2, 3, 4)
      val expected2 = Vector(1, 2, 6, 5, 2, 3, 4) // sub list reversed

      assert(cycleOrder.compareBothDirections(elementaryCycle.get, expected1)
             || cycleOrder.compareBothDirections(elementaryCycle.get, expected2), "The cycle " + elementaryCycle.get + " did not match " + expected1 + " or " + expected2)

    }

    it ("should not append cycles that includes more than one vertex from the main cycle") {

      Given("a graph with two cycles 1, 2, 3, 4 and 1, 2, 5, 6")
      val cycle1 = Graph(1~2, 2~3, 3~4, 4~1)
      val cycle2 = Graph(1~2, 2~5, 5~6, 6~1)
      val graph = cycle1 ++ cycle2

      When("finding an elementary cycle")
      val elementaryCycle = elementaryCycleFinder.findCycle(graph)

      Then("the elementary cycle should be one of the two cycles, but not the other")
      elementaryCycle should be ('defined)
      val expected1 = Vector(1, 2, 3, 4)
      val expected2 = Vector(1, 2, 5, 6)
      val expected3 = Vector(1, 6, 5, 2, 3, 4)

      assert(cycleOrder.compareBothDirections(elementaryCycle.get, expected1)
             || cycleOrder.compareBothDirections(elementaryCycle.get, expected2)
             || cycleOrder.compareBothDirections(elementaryCycle.get, expected3), "The cycle " + elementaryCycle.get + " did not match " + expected1 + " or " + expected2 + " or " + expected3)

    }

    it ("should handle graphs where some vertices matches the condition for sub cycles, but doesn't have any") {

      Given("a graph that results in a main cycle 1, 2, 3 having the vertex 3 connected to two vertices not part of any cycle")
      // Technically 3,4,2 and 3,5,1 are cycles, but they contain 2 vertices from the main cycle and won't be returned.
      // This is needed to avoid those edges from being pruned as dead ends
      val graph = Graph(1~2, 2~3, 3~1, 3~4, 4~2, 3~5, 5~1)

      // Needs to inject the converted graph so we can access the vertices when providing the return from the cycle finder
      val convGraph = graphTypeConv.convert(graph)
      val graphConvMock = mock[GraphConverterI[Int, VisitedVertex[Int], UnDiEdge, UnDiEdge]]
      (graphConvMock.convert(_: Graph[Int, UnDiEdge])) expects(*) returns (convGraph) once()
      val vv1 = convGraph.nodes.find(_.vertex == 1).get
      val vv2 = convGraph.nodes.find(_.vertex == 2).get
      val vv3 = convGraph.nodes.find(_.vertex == 3).get

      // Finds a single cycle
      val cycleFinderMock = mock[SingleCycleFinder[VisitedVertex[Int], UnDiEdge]]
      (cycleFinderMock.findCycleFromVertex _) expects(*, convGraph) returns(Some(Cycle(vv1, vv2, vv3))) once()
      (cycleFinderMock.findCycleFromVertex _) expects(*, *) returns (None) anyNumberOfTimes()

      When("finding an elementary cycle")
      val finder = new ElementaryCycleFinder[Int, UnDiEdge](cycleFinderMock, graphConvMock)
      val elementaryCycle = finder.findCycle(graph)

      Then("the cycle 1, 2, 3 should be found")
      elementaryCycle should be ('defined)
      val expected = Vector(1, 2, 3)
      assert(cycleOrder.compareBothDirections(elementaryCycle.get, expected), "The cycle " + elementaryCycle.get + " did not match " + expected)

    }
  }

}
