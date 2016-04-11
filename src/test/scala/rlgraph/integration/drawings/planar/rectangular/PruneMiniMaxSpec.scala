package rlgraph.integration.drawings.planar.rectangular

import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.PruneMiniMax
import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help.{BubbleGraph, BubbleVertex}
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import rlgraph.SpecImports

class PruneMiniMaxSpec extends SpecImports {
  private val pruneMinimax = new PruneMiniMax()

  describe("PruneMiniMax") {

    it ("should return a solution for a single 0-bubble") {

      Given("a bubble-graph with a single bubble in it")
      val graph = BubbleGraph(Vector(Triangle(1, 2, 3))).head
      val vertices = graph.bubbleVertices

      When("pruning the graph")
      val solution = pruneMinimax.applyHeuristic(graph)

      Then("the solution should contain a single bubble-vertex and no subgraphs")
      solution should have size 1

      And("the vertex solution should be one of the initial bubble vertices")
      vertices should contain (solution.head)

    }

    it ("should return a solution for a graph containing 1-, 2- and 3-bubbles.") {

      Given("a bubble-graph containing five 2-bubbles connected to each other, with a single 1-bubble connected to one of them")
      val twoBubble1 = Triangle(1, 2, 3)
      val twoBubble2 = Triangle(2, 3, 4) // Shares 2,3 with #1
      val twoBubble3 = Triangle(1, 3, 4) // Shares 1,3 with #1, 3,4 with #2
      val twoBubble4 = Triangle(3, 4, 5) // Shares 3,4 with #2 and #3, 5,3 with #5
      val twoBubble5 = Triangle(2, 3, 5) // Shares 5,3 with #4, 2,3 with #1 and #2
      val oneBubble = Triangle(2, 4, 7) // Shares 2,4 with #2
      val graph = BubbleGraph(Vector(twoBubble1, twoBubble2, twoBubble3, twoBubble4, twoBubble5, oneBubble)).head
      val bubbles = graph.bubbles

      When("pruning the graph")
      val solution: Vector[BubbleVertex[Int]] = pruneMinimax.applyHeuristic(graph)

      Then("every bubble in the original graph should have one of its vertices present in the solution")
      bubbles.filter(b => b.vertices.intersect(solution).isEmpty) should be ('empty)

    }

  }

}
