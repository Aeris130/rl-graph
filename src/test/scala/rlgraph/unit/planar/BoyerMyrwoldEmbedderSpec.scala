package rlgraph.unit.planar

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BoyerMyrwoldEmbedderSpec extends SpecImports {
  private val embedder = new BoyerMyrwoldEmbedder[Int]()
  private val cycleMatcher = new CycleOrderMatcher()
  private val faceComp = new FaceComputation[Int]()

  private def nonPlanarGraph = Graph(1~2, 1~3, 1~4, 1~5, 2~3, 2~4, 2~5, 3~4, 3~5, 4~5)

  describe("BoyerMyrwoldEmbedder") {

    it ("should reject a non-planar graph") {

      Given("a non-planar graph")
      val graph = nonPlanarGraph

      When("embedding the graph")
      val result = embedder.embed(graph)

      Then("the result should be None")
      result should be (None)

    }

    it ("should return false for a non-planar graph") {

      Given("a non-planar graph")
      val graph = nonPlanarGraph

      When("checking if the graph is planar")
      val isPlanar = embedder.isPlanar(graph)

      Then("the result should be false")
      isPlanar should be (false)

    }

    it ("should embed single vertices") {

      Given("a graph with vertices and no edges")
      val graph = Graph[Int, UnDiEdge](1, 2)

      When("embedding the graph")
      val embedding = embedder.embed(graph).get

      Then("the embedding should contain two empty entries for 1 and 2")
      cycleMatcher.compareBothDirections(embedding.embeddedVertices, Vector(1, 2)) should be (true)
      embedding.embeddingFor(1).isEmpty should be (true)
      embedding.embeddingFor(2).isEmpty should be (true)

    }

    it ("should embed edges") {

      Given("a graph with edges forming faces 1, 2, 3 / 2, 4, 3 / 1, 2, 4, 3")
      val graph = Graph(1~2, 1~3, 2~3, 2~4, 3~4)

      When("embedding the graph")
      val embedding = embedder.embed(graph).get

      Then("the embedding should have the faces of the original graph")
      val faces = faceComp.computeFaces(embedding)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 3))))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 3))))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 4, 3))))

    }

    it ("should generate embeddings deterministically") {

      Given("a planar graph and an initial embedding derived from it")
      def constructGraph: Graph[Int, UnDiEdge] = {
        Graph[Int, UnDiEdge](1, 2, 3, 4, 5, 6, 7, 8, 9, 1~5, 3~1, 4~3, 5~2, 6~1, 6~2, 6~3, 6~5, 8~4, 8~7, 9~4, 9~6, 9~7, 9~8)
      }


      val firstEmbedder = new BoyerMyrwoldEmbedder[Int]()
      val embedding = firstEmbedder.embed(constructGraph).get

      When("embedding the same graph multiple times")
      Then("every embedding should equal the initial embedding")

      for (i <- 0 to 1000) {
        val newEmbedding = firstEmbedder.embed(constructGraph).get
        testGraphEquality(embedding, newEmbedding)
      }

    }

  }

  private def testGraphEquality[E](e1: Embedding[E], e2: Embedding[E]) {
    e1.embeddedVertices should equal (e2.embeddedVertices)
    for (v <- e1.embeddedVertices) {
      val e1Adj = e1.embeddingFor(v).toVector.map(_.adjacentVertex)
      val e2Adj = e2.embeddingFor(v).toVector.map(_.adjacentVertex)

      e1Adj should equal (e2Adj)
    }
    e1.edges should equal (e2.edges)

  }

}
