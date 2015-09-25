package rlgraph.unit.pathfinding.disjointElementaryCycle

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.BlackWhiteGraph
import rlgraph.SpecImports

class BlackWhiteGraphSpec extends SpecImports {

  describe("BlackWhiteGraph") {

    // Most functionality is delegated to helper classes

    it ("should compute alternating edges from added vertices") {

      Given("a graph with the edges B1 - W1 - B2 - W2 - B3")
      val w1 = UndirectedEmbedding[Int]().embed(1)
      val w2 = UndirectedEmbedding[Int]().embed(2)
      val b1 = 11
      val b2 = 22
      val b3 = 33
      val bwGraph = new BlackWhiteGraph[Int]()
      bwGraph.addNodes(w1, b1, b2)
      bwGraph.addNodes(w2, b2, b3)

      When("computing alternating edges")
      val edges = bwGraph.computeIncidentEdges

      Then("every other node should be black and white")
      edges.map(p => (p._1.isBlack, p._2.isBlack)) should be (Vector((true, false), (true, false)))

      And("the edges should be B1 - W1, B2 - W2 or B3 - W2, B2 - W1")
      val expected1 = Vector((b1, w1), (b2, w2))
      val expected2 = Vector((b3, w2), (b2, w1))
      val result = edges.map(p => (p._1.vertex, p._2.subgraph))
      assert(result == expected1 || result == expected2, "The result " + result + " did not match " + expected1 + " or " + expected2)

    }
  }
}
