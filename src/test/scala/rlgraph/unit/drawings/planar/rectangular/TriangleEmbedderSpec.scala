package rlgraph.unit.drawings.planar.rectangular

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.drawings.planar.rectangular.embedding.help.TriangleEmbedder
import rlgraph.SpecImports

class TriangleEmbedderSpec extends SpecImports {
  private val embedder = new TriangleEmbedder()

  def triangleEmbedding = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2)
      .embed(2, 3)
      .embed(3, 1)
  }

  describe("TriangleEmbedder") {

    it ("should embed a new triangle given a new vertex and an edge") {

      Given("an embedding with a triangle having a clockwise face 1, 2, 3")
      val embedding = triangleEmbedding.embedding

      When("embedding the vertex 4 between 1 and 2")
      val result = embedder.embedNewTriangle(4, 1, 2, embedding)

      Then("the neighbors around vertex 1 should be 3, 4, 2")
      val adjacency1 = result.embeddingFor(1).entryFor(3)
      adjacency1.next.adjacentVertex should be (4)
      adjacency1.next.next.adjacentVertex should be (2)

      And("the neighbors around vertex 2 should be 1, 4, 3")
      val adjacency2 = result.embeddingFor(2).entryFor(1)
      adjacency2.next.adjacentVertex should be (4)
      adjacency2.next.next.adjacentVertex should be (3)

      And("the neighbors around 4 should be 1, 2")
      val adjacency4 = result.embeddingFor(4).entryFor(1)
      adjacency4.next.adjacentVertex should be (2)

    }

  }
}
