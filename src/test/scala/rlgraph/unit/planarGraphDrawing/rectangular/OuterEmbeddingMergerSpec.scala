package rlgraph.unit.rectangular

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.embedding.help.OuterEmbeddingMerger
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

class OuterEmbeddingMergerSpec extends SpecImports {
  private val outerMerger = new OuterEmbeddingMerger[Int]((f: Vector[Face[Int]]) => f.find(face => face.vertexSize > 3))

  /* Tests circular lists to see if they match each other using some cyclical order (1,2 3 == 2, 3, 1 is true). */
  private val cycleMatcher = new CycleOrderMatcher()

  /* Easiest way to check that the embedding preserves edges in the correct positions is to check that the correct
   * faces still exists.
   */
  private val faceComp = new FaceComputation[Int]()

  /**
   * Creates four vertices from the start id to startid + 3 and joins them as two internal faces of length
   * 3, and an external face of length 4.
   */
  private def twoTriangularFaces(v1: Int, v2: Int, v3: Int, v4: Int): Embedding[Int] = {
    val embedding = UndirectedEmbedding[Int]()
      .embed(v1, v2)
      .embed(v1, v3, None, Some(v2))
      .embed(v2, v4)
      .embed(v3, v4)
      .embed(v2, v3, Some(v1), Some(v4))

    embedding
  }

  /* A collection of embeddings with a rectangular outer face and two triangular inner faces. */
  private def triangularInnerFaces = new {
    // Connected around vertex 1
    val embedding1 = twoTriangularFaces(1, 2, 3, 4)
    val embedding2 = twoTriangularFaces(1, 5, 6, 7)
    val embedding3 = twoTriangularFaces(1, 8, 9, 10)

    // Connects to embedding 3 at vertex 10
    val embedding4 = twoTriangularFaces(10, 11, 12, 13)
  }

  describe("OuterEmbeddingMerger") {

    it ("should merge two embeddings at a common vertex") {

      Given("two embeddings with a common vertex 1")
      val f = triangularInnerFaces
      import f._
      val cutpointToEmbedding = Map(1 -> Set(embedding1, embedding2))
      val embeddingToCutpoint = Map(embedding1 -> Set(1), embedding2 -> Set(1))
      val embeddings = Vector(embedding1, embedding2)

      When("merging the embeddings")
      val finalEmbedding = outerMerger.mergeEmbeddings(embeddings, cutpointToEmbedding, embeddingToCutpoint)

      Then("there should be 5 faces in the embedding")
      val faces = faceComp.computeFaces(finalEmbedding).map(f => f.vertices)
      faces should have size 5

      And("the final embedding should preserve the inner faces of both embeddings")
      val innerF1Emb1 = Vector(1, 2, 3)
      val innerF2Emb1 = Vector(2, 3, 4)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb1)), "The face " + innerF1Emb1 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb1)), "The face " + innerF2Emb1 + " was not found in " + faces)

      val innerF1Emb2 = Vector(1, 5, 6)
      val innerF2Emb2 = Vector(5, 6, 7)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb2)), "The face " + innerF1Emb2 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb2)), "The face " + innerF2Emb2 + " was not found in " + faces)

      And("the final embedding should have the outer face 1, 3, 4, 2, 1, 6, 7, 5")
      // Note that this assumes that the faces are traversed in order 1, 3, 4, 2 and 1, 6, 7, 5 instead of 2, 4, 3 and 5, 7, 6
      val outerFace = Vector(1, 3, 4, 2, 1, 6, 7, 5)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, outerFace)), "The outer face " + outerFace + " was not found in " + faces)

    }

    it ("should preserve the order of edges added when 3 or more embeddings are merged") {

      Given("three embeddings with two faces, sharing a single vertex (1)")
      val f = triangularInnerFaces
      import f._
      val embeddings = Vector(embedding1, embedding2, embedding3)
      val cutpointsToEmbedding = Map(1 -> Set(embedding1, embedding2, embedding3))
      val embeddingToCutpoints = Map(embedding1 -> Set(1), embedding2 -> Set(1), embedding3 -> Set(1))

      When("merging the two embeddings")
      val finalEmbedding = outerMerger.mergeEmbeddings(embeddings, cutpointsToEmbedding, embeddingToCutpoints)

      Then("there should be 7 faces in the embedding")
      val faces = faceComp.computeFaces(finalEmbedding).map(f => f.vertices)
      faces should have size 7

      And("the internal faces of every embedding should be preserved")
      val innerF1Emb1 = Vector(1, 2, 3)
      val innerF2Emb1 = Vector(2, 3, 4)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb1)), "The face " + innerF1Emb1 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb1)), "The face " + innerF2Emb1 + " was not found in " + faces)

      val innerF1Emb2 = Vector(1, 5, 6)
      val innerF2Emb2 = Vector(5, 6, 7)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb2)), "The face " + innerF1Emb2 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb2)), "The face " + innerF2Emb2 + " was not found in " + faces)

      val innerF1Emb3 = Vector(1, 8, 9)
      val innerF2Emb3 = Vector(8, 9, 10)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb3)), "The face " + innerF1Emb3 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb3)), "The face " + innerF2Emb3 + " was not found in " + faces)

      And("The outer face should traverse the outer faces of all three embeddings")
      // This result is dependent on which order the embeddings are merged in
      val outerFace = Vector(1, 3, 4, 2, 1, 6, 7, 5, 1, 9, 10, 8)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, outerFace)), "The outer face " + outerFace + " was not found in " + faces)

    }

    it ("should embed a chain of components") {

      Given("three embeddings connected as a chain around vertex 1 and 10")
      val f = triangularInnerFaces
      import f._
      val embeddings = Vector(embedding1, embedding3, embedding4)
      val cutpointsToEmbedding = Map(1 -> Set(embedding1, embedding3), 10 -> Set(embedding3, embedding4))
      val embeddingToCutpoints = Map(embedding1 -> Set(1), embedding3 -> Set(1, 10), embedding4 -> Set(10))

      When("merging the two embeddings")
      val finalEmbedding = outerMerger.mergeEmbeddings(embeddings, cutpointsToEmbedding, embeddingToCutpoints)

      Then("there should be 7 faces in the embedding")
      val faces = faceComp.computeFaces(finalEmbedding).map(f => f.vertices)
      faces should have size 7

      And("the internal faces of every embedding should be preserved")
      val innerF1Emb1 = Vector(1, 2, 3)
      val innerF2Emb1 = Vector(2, 3, 4)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb1)), "The face " + innerF1Emb1 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb1)), "The face " + innerF2Emb1 + " was not found in " + faces)

      val innerF1Emb3 = Vector(1, 8, 9)
      val innerF2Emb3 = Vector(8, 9, 10)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb3)), "The face " + innerF1Emb3 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb3)), "The face " + innerF2Emb3 + " was not found in " + faces)

      val innerF1Emb4 = Vector(10, 12, 11)
      val innerF2Emb4 = Vector(11, 13, 12)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb4)), "The face " + innerF1Emb3 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb4)), "The face " + innerF2Emb3 + " was not found in " + faces)

      And("The outer face should traverse the outer faces of all three embeddings")
      // This result is dependent on which order the embeddings are merged in
      val outerFace = Vector(1, 3, 4, 2, 1, 9, 10, 12, 13, 11, 10, 8)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, outerFace)), "The outer face " + outerFace + " was not found in " + faces)

    }

    it ("should return an embedding as-is if specifying a single component and no cut vertices") {

      Given("a single embedded component with faces 1, 2, 3 and 2, 3, 4 and 1, 2, 4, 3")
      val f = triangularInnerFaces
      import f._
      val embedding = embedding1

      When("merging the single embedding")
      val finalEmbedding = outerMerger.mergeEmbeddings(Vector(embedding), Map(), Map())

      Then("the single embedding should persist")
      val faces = faceComp.computeFaces(finalEmbedding).map(f => f.vertices)
      faces should have size 3

      val innerF1Emb1 = Vector(1, 2, 3)
      val innerF2Emb1 = Vector(2, 3, 4)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF1Emb1)), "The face " + innerF1Emb1 + " was not found in " + faces)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, innerF2Emb1)), "The face " + innerF2Emb1 + " was not found in " + faces)

      val outerFace = Vector(1, 2, 4, 3)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f, outerFace)), "The face " + innerF1Emb1 + " was not found in " + faces)

    }
  }
}
