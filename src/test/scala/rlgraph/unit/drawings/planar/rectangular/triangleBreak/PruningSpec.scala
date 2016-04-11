package rlgraph.unit.rectangular.triangleBreak

import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help.{BubbleGraph, Pruning}
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import rlgraph.SpecImports

class PruningSpec extends SpecImports {
  private val pruner = new Pruning()

  describe("Pruning") {

    it ("should return a solution for a single 0-bubble") {

      Given("a bubble-graph with a single bubble in it")
      val graph = BubbleGraph(Vector(Triangle(1, 2, 3))).head
      val vertices = graph.bubbleVertices

      When("pruning the graph")
      val solution = pruner.prune(graph)

      Then("the solution should contain a single bubble-vertex and no subgraphs")
      solution.vertices should have size 1
      solution.graphsRemaining should be ('empty)

      And("the vertex solution should be one of the initial bubble vertices")
      vertices should contain (solution.vertices.head)

    }

    it ("should return a solution for two 1-bubbles") {

      Given("a bubble-graph with two 1-bubbles sharing one edge 2~3")
      val graph = BubbleGraph(Vector(Triangle(1, 2, 3), Triangle(2, 3, 4))).head
      val commonVertex = graph.bubbleVertices.find(v => Set(v._1, v._2) == Set(2, 3)).get

      When("pruning the graph")
      val solution = pruner.prune(graph)

      Then("the solution should contain a single bubble-vertex and no subgraphs")
      solution.vertices should have size 1
      solution.graphsRemaining should be ('empty)

      And("the vertex solution should be the one representing the common edge 2~3")
      solution.vertices.head should be (commonVertex)

    }

    it ("should return graphs that doesn't contain 0- or 1-bubbles") {

      Given("a bubble-graph containing five 2-bubbles connected to each other, with a single 1-bubble connected to one of them")
      val twoBubble1 = Triangle(1, 2, 3)
      val twoBubble2 = Triangle(2, 3, 4) // Shares 2,3 with #1
      val twoBubble3 = Triangle(1, 3, 4) // Shares 1,3 with #1, 3,4 with #2
      val twoBubble4 = Triangle(3, 4, 5) // Shares 3,4 with #2 and #3, 5,3 with #5
      val twoBubble5 = Triangle(2, 3, 5) // Shares 5,3 with #4, 2,3 with #1 and #2
      val oneBubble = Triangle(2, 4, 7) // Shares 2,4 with #2
      val graph = BubbleGraph(Vector(twoBubble1, twoBubble2, twoBubble3, twoBubble4, twoBubble5, oneBubble)).head
      val vertex2_4 = graph.bubbleVertices.find(v => Set(v._1, v._2) == Set(2, 4)).get

      When("pruning the graph")
      val solution = pruner.prune(graph)

      Then("the solution should contain the single vertex 1,4")
      solution.vertices should be (Vector(vertex2_4))

      And("the subgraph containing the two-bubbles should be returned")
      solution.graphsRemaining should have size 1
      val b1 = findBubble(twoBubble1, graph)
      val b3 = findBubble(twoBubble3, graph)
      val b4 = findBubble(twoBubble4, graph)
      val b5 = findBubble(twoBubble5, graph)

      // Bubble #2 gets popped along with the 1-bubble.
      solution.graphsRemaining.head.bubbles.toSet should be (Set(b1, b3, b4, b5))

    }
  }

  private def findBubble(t: Triangle[Int], graph: BubbleGraph[Int]) = graph.bubbles.find(_.vertices.map(e => Vector(e._1, e._2)).flatten.toSet == Set(t._1, t._2, t._3)).get
}
