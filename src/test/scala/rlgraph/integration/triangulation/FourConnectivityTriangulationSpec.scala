package rlgraph.integration.triangulation

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import net.cyndeline.rlgraph.triangulation.fourConnectivity.FourConnectivityTriangulation
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FourConnectivityTriangulationSpec extends SpecImports {
  private val faceComp = new FaceComputation[Int]()
  private val triangulation = new FourConnectivityTriangulation[Int]()

  describe("FourConnectivityTriangulation") {

    it ("shouldn't add edges to a face of size 3") {

      Given("a graph with the face 1, 2, 3")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 1)

      When("triangulating the graph")
      val resultingEmbedding = triangulation.triangulate(embedding)

      Then("every face in the resulting embedding should have size 3")
      for (f <- faceComp.computeFaces(resultingEmbedding)) {
        f.vertices should have size 3
      }

    }

    it ("should triangulate a graph with faces of size 4") {

      Given("a graph with the face 1, 2, 3, 4")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 1)

      When("triangulating the graph")
      val resultingEmbedding = triangulation.triangulate(embedding)

      Then("every face in the resulting embedding should have size 3")
      for (f <- faceComp.computeFaces(resultingEmbedding)) {
        f.vertices should have size 3
      }

    }

    it ("should triangulate multiple faces") {

      Given("a graph with two faces having size 5, and an outer face")
      val embedding = UndirectedEmbedding[Int]()
        .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 1)
        .embedEdge(Vertex(6) withInsertPosition 4 inVertex 5 withDefaultInsertPosition)
        .embedEdge(Vertex(6) withDefaultPositionInVertex 7 withInsertPosition 5)
        .embed(7, 8)
        .embedEdge(Vertex(8) withInsertPosition 2 inVertex 3 withDefaultInsertPosition)

      When("triangulating the graph")
      val resultingEmbedding = triangulation.triangulate(embedding)

      Then("every face in the resulting embedding should have size 3")
      for (f <- faceComp.computeFaces(resultingEmbedding)) {
        f.vertices should have size 3
      }
    }

    it ("should ignore a face when adding edges") {

      Given("a graph having faces 1, 2, 3, 4 and 4, 3, 2, 1")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 1)

      When("triangulating the graph and specifying that face 1, 2, 3, 4 should remain unaltered")
      val resultingEmbedding = triangulation.triangulateWithoutFaces(embedding, Set(Face(1, 2, 3, 4)))

      Then("the resulting embedding should contain the face 1, 2, 3, 4")
      val allFaces = faceComp.computeFaces(resultingEmbedding)
      val f = Face(1, 2, 3, 4)
      allFaces should contain (f)

      And("the remaining faces should have size 3")
      for (f <- allFaces diff Vector(f)) {
        f.vertices should have size 3
      }

    }

    it ("should add edges deterministically") {

      Given("a biconnected graph without inner triangles")
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 5~6, 6~7, 7~1, 2~8, 8~9, 9~3, 9~10, 10~11, 11~12, 12~5, 12~13, 13~6, 13~7, 7~14, 14~1, 14~16, 16~8, 1~15, 15~2)
      val embedding = new BoyerMyrwoldEmbedder[Int]().embed(graph).get

      When("triangulating the graph 100 times")
      val resultingEmbedding = triangulation.triangulate(embedding)

      Then("every triangulation should be equal to the first")
      for (i <- 0 to 100) {
        val additionalEmbedding = triangulation.triangulate(embedding)
        additionalEmbedding.edges should equal (resultingEmbedding.edges)
      }

    }

    // Trigger spec, case found during test runs
    it ("should not induce multiple inner triangles") {

      Given("a graph with two non-triangular faces [1, 2, 7, 5, 4], [2, 8, 5, 7] and an existing triangle <4, 6, 5>")
      var embedding = UndirectedEmbedding[Int]()
        .embed(1, 2).embed(2, 7).embed(7, 5).embed(5, 4).embed(4, 1)
        .embedEdge(Vertex(2) withDefaultPositionInVertex 8 withInsertPosition 1)
          .embedEdge(Vertex(8) withInsertPosition 7 inVertex 5 withDefaultInsertPosition)
        .embedEdge(Vertex(6) withInsertPosition 8 inVertex 5 withDefaultInsertPosition)
          .embedEdge(Vertex(6) withInsertPosition 5 inVertex 4 withDefaultInsertPosition)

      // Add dummy triangular faces to make vertex 5 and 4 have the highest degree: 6
      embedding = embedding
        .embedEdge(Vertex(5) withDefaultPositionInVertex 9 withInsertPosition 8)
        .embedEdge(Vertex(5) withDefaultPositionInVertex 3 withInsertPosition 9)
          .embedEdge(Vertex(9) withInsertPosition 5 inVertex 3 withDefaultInsertPosition)
        .embedEdge(Vertex(3) withInsertPosition 5 inVertex 6 withInsertPosition 9)
        .embedEdge(Vertex(6) withDefaultPositionInVertex 10 withInsertPosition 3)
        .embedEdge(Vertex(4) withDefaultPositionInVertex 10 withInsertPosition 6)
        .embedEdge(Vertex(4) withDefaultPositionInVertex 11 withInsertPosition 10)
        .embedEdge(Vertex(4) withInsertPosition 1 inVertex 2 withInsertPosition 11)
        .embedEdge(Vertex(2) withDefaultPositionInVertex 11 withInsertPosition 4)

      When("triangulating the graph without the outer face")
      val resultingEmbedding = triangulation.triangulateWithoutFaces(embedding, Set(Face(5, 8, 2, 11, 4, 10, 3, 9)))

      Then("at most two vertices should have both 5 and 4 as a neighbor")
      val neighbors = resultingEmbedding.embeddedVertices.filter(v => (v != 5 && resultingEmbedding.embeddingFor(v).containsEntryFor(5)) && (v != 4 && resultingEmbedding.embeddingFor(v).containsEntryFor(4)))
      assert(neighbors.size < 3, "Inner triangles were induced between the edge 5~4 and neighbors " + neighbors)

    }

    //Trigger spec
    it ("should add an edge from v to w if the only common adjacencies of the two are the neighbors of w on F") {

      Given("a graph with a face that gets splits into a sub-face with 4 vertices, and the maximum-degree vertex having common adjacencies with w")
      val embedding = UndirectedEmbedding[Int]()
        .embed(1, 4).embed(4, 5).embed(5, 6).embed(6, 2).embed(2, 1)
        .embedEdge(Vertex(3) withInsertPosition 1 inVertex 4 withDefaultInsertPosition)
          .embedEdge(Vertex(3) withInsertPosition 4 inVertex 5 withDefaultInsertPosition)
        .embedEdge(Vertex(7) withInsertPosition 4 inVertex 3 withDefaultInsertPosition)
        .embedEdge(Vertex(7) withInsertPosition 1 inVertex 4 withDefaultInsertPosition)
        .embedEdge(Vertex(10) withInsertPosition 1 inVertex 4 withDefaultInsertPosition)
        .embedEdge(Vertex(10) withInsertPosition 2 inVertex 1 withDefaultInsertPosition)
        .embedEdge(Vertex(9) withInsertPosition 2 inVertex 1 withDefaultInsertPosition)
          .embedEdge(Vertex(9) withInsertPosition 6 inVertex 2 withDefaultInsertPosition)
          .embedEdge(Vertex(9) withInsertPosition 5 inVertex 6 withInsertPosition 2)
        .embedEdge(Vertex(8) withInsertPosition 5 inVertex 6 withDefaultInsertPosition)
          .embedEdge(Vertex(8) withInsertPosition 3 inVertex 5 withDefaultInsertPosition)
          .embedEdge(Vertex(8) withInsertPosition 7 inVertex 3 withInsertPosition 5)

      When("triangulating the graph without the outer face")
      val outerFace = Face(9, 1, 10, 4, 7, 3, 8, 6)
      val resultingEmbedding = triangulation.triangulateWithoutFaces(embedding, Set(outerFace))

      Then("the only non-triangular face should be the outer")
      val allFaces = faceComp.computeFaces(resultingEmbedding)
      (allFaces diff Vector(outerFace)).filter(_.vertexSize > 3) should be ('empty)

    }

  }

}
