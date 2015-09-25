package rlgraph.integration.planarGraphDrawing.rectangular

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.TriangleBreaker
import rlgraph.SpecImports

class TriangleBreakerSpec extends SpecImports {
  private val triangleBreaker = new TriangleBreaker()

  describe("TriangleBreaker") {

    it ("should break triangles deterministically") {

      Given("an embedding with a triangle <1, 2, 3> that isn't a face")
      val v1 = RVertex(1)
      val v2 = RVertex(2)
      val v3 = RVertex(3)
      val v4 = RVertex(4)
      val v5 = RVertex(5)
      val embedding = UndirectedEmbedding[RVertex[Int]]().embed(v1, v2).embed(v2, v3).embed(v3, v1)
        .embedEdge(Vertex(v4) withInsertPosition v1 inVertex v3 withDefaultInsertPosition)
          .embedEdge(Vertex(v4) withInsertPosition v2 inVertex v1 withInsertPosition v3)
        .embedEdge(Vertex(v5) withInsertPosition v2 inVertex v3 withDefaultInsertPosition)
          .embedEdge(Vertex(v5) withInsertPosition v1 inVertex v2 withInsertPosition v3)

      When("breaking triangles in the embedding 100 times")
      val firstNeighborSet = neighborsOfSingleSplit(embedding)

      Then("every neighbor set should be equal to the first")

      for (i <- 0 until 100) {
        neighborsOfSingleSplit(embedding) should equal (firstNeighborSet)
      }

    }
  }

  private def neighborsOfSingleSplit(e: Embedding[RVertex[Int]]): Vector[RVertex[Int]] = {
    val splitEmbedding = triangleBreaker.breakTriangles(e)
    val split = splitEmbedding.embeddedVertices.find(_.isSplit).get
    splitEmbedding.embeddingFor(split).toVector.map(_.adjacentVertex)
  }
}
