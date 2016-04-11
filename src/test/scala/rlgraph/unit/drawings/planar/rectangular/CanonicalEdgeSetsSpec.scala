package rlgraph.unit.drawings.planar.rectangular

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.regularEdgeLabeling.factories.help.CanonicalEdgeSets
import rlgraph.SpecImports

class CanonicalEdgeSetsSpec extends SpecImports {

  private def testEmbedding = new {
    val vN = 8
    val vE = 7
    val vS = 6
    val vW = 5
    val embedding = UndirectedEmbedding[Int]()
      .embed(vW, vN).embed(vW, 3).embed(vW, 2).embed(vW, vS)
      .embed(vS, 2).embed(vS, 4).embed(vS, vE)
      .embedEdge(Vertex(2) withInsertPosition vW inVertex 3 withInsertPosition vW)
      .embedEdge(Vertex(2) withInsertPosition vS inVertex 4 withInsertPosition 3)
      .embedEdge(Vertex(4) withInsertPosition vW inVertex 3 withInsertPosition 2)
      .embedEdge(Vertex(4) withDefaultPositionInVertex 1 withInsertPosition 3)
      .embedEdge(Vertex(4) withInsertPosition vS inVertex vE withInsertPosition 1)
      .embedEdge(Vertex(3) withInsertPosition vW inVertex vN withInsertPosition vW)
      .embedEdge(Vertex(3) withInsertPosition 4 inVertex 1 withInsertPosition vN)
      .embedEdge(Vertex(1) withInsertPosition vW inVertex vN withInsertPosition 3)
      .embedEdge(Vertex(1) withInsertPosition 4 inVertex vE withInsertPosition vN)
      .embedEdge(Vertex(vE) withInsertPosition vW inVertex vN withInsertPosition 1)
  }

  describe("CanonicalEdgeSets") {

    it ("should compute T1 and T2 from an embedding where the outer face goes in an opposite direction: N, W, S, E") {

      Given("an embedding with 4 inner vertices and 4 external vertices and the outer face N, W, S, E")
      val f = testEmbedding
      import f._

      When("constructing a REL")
      val rel = new CanonicalEdgeSets(embedding, vS, vN, vW, vE)

      Then("every inner vertex 1 to 4 should have an incoming and an outgoing edge in T1")
      val innerVertices = Vector(1, 2, 3, 4)
      val failureT1 = innerVertices.filterNot(v => rel.T1.exists(_._1 == v) && rel.T1.exists(_._2 == v))
      assert(failureT1.isEmpty, failureT1.mkString(", ") + " did not contain both incoming and outgoing edges in T1")
      failureT1 should be ('empty)


      And("every inner vertex 1 to 4 should have an incoming and an outgoing edge in T2")
      val failureT2 = innerVertices.filterNot(v => rel.T2.exists(_._1 == v) && rel.T2.exists(_._2 == v))
      assert(failureT2.isEmpty, failureT2.mkString(", ") + " did not contain both incoming and outgoing edges in T2")

    }

    it ("should embed edges S~W, S~E, N~W, N~E and S~N in T1") {

      Given("an embedding")
      val f = testEmbedding
      import f._

      When("constructing a REL")
      val rel = new CanonicalEdgeSets(embedding, vS, vN, vW, vE)

      Then("the embedding of T1 should contain edges S~W, S~E, N~W, N~E and S~N")
      rel.embeddingT1.embeddingFor(vS).containsEntryFor(vW) should be (true)
      rel.embeddingT1.embeddingFor(vS).containsEntryFor(vE) should be (true)
      rel.embeddingT1.embeddingFor(vN).containsEntryFor(vW) should be (true)
      rel.embeddingT1.embeddingFor(vN).containsEntryFor(vE) should be (true)
      rel.embeddingT1.embeddingFor(vS).containsEntryFor(vN) should be (true)

    }

    it ("should not add the edges S~W, S~E, N~W, N~E and S~N to the edge set of T1") {

      Given("an embedding")
      val f = testEmbedding
      import f._

      When("constructing a REL")
      val rel = new CanonicalEdgeSets(embedding, vS, vN, vW, vE)

      Then("the edges in T1 should not contain edges S~W, S~E, N~W, N~E and S~N")
      rel.T1 should not.contain (vS, vW)
      rel.T1 should not.contain (vW, vS)
      rel.T1 should not.contain (vS, vE)
      rel.T1 should not.contain (vE, vS)
      rel.T1 should not.contain (vN, vW)
      rel.T1 should not.contain (vW, vN)
      rel.T1 should not.contain (vN, vE)
      rel.T1 should not.contain (vE, vN)
      rel.T1 should not.contain (vS, vN)
      rel.T1 should not.contain (vN, vS)

    }

    it ("should embed edges S~W, S~E, N~W, N~E and W~E in T2") {

      Given("an embedding")
      val f = testEmbedding
      import f._

      When("constructing a REL")
      val rel = new CanonicalEdgeSets(embedding, vS, vN, vW, vE)

      Then("the embedding of T1 should contain edges S~W, S~E, N~W, N~E and W~E")
      rel.embeddingT2.embeddingFor(vS).containsEntryFor(vW) should be (true)
      rel.embeddingT2.embeddingFor(vS).containsEntryFor(vE) should be (true)
      rel.embeddingT2.embeddingFor(vN).containsEntryFor(vW) should be (true)
      rel.embeddingT2.embeddingFor(vN).containsEntryFor(vE) should be (true)
      rel.embeddingT2.embeddingFor(vW).containsEntryFor(vE) should be (true)

    }

    it ("should not add the edges S~W, S~E, N~W, N~E and W~E to the edge set of T2") {

      Given("an embedding")
      val f = testEmbedding
      import f._

      When("constructing a REL")
      val rel = new CanonicalEdgeSets(embedding, vS, vN, vW, vE)

      Then("the edges in T1 should not contain edges S~W, S~E, N~W, N~E and W~E")
      rel.T2 should not.contain (vS, vW)
      rel.T2 should not.contain (vW, vS)
      rel.T2 should not.contain (vS, vE)
      rel.T2 should not.contain (vE, vS)
      rel.T2 should not.contain (vN, vW)
      rel.T2 should not.contain (vW, vN)
      rel.T2 should not.contain (vN, vE)
      rel.T2 should not.contain (vE, vN)
      rel.T2 should not.contain (vW, vE)
      rel.T2 should not.contain (vE, vW)

    }

  }
}
