package rlgraph.unit.planarGraphDrawing.rectangular

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.help.OuterFaceAugmentation
import net.cyndeline.rlgraph.util.GraphCommons
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

class OuterFaceAugmentationSpec extends SpecImports {
  private val faceComp = new FaceComputation[Int]()
  private val cycleOrder = new CycleOrderMatcher()

  // Outer face 1, 2, 3, 4 with an edge from 2 to 4
  private def fourVertexOuter = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 1).embedEdge(Vertex(2) withInsertPosition 1 inVertex 4 withInsertPosition 3)
    val outerFace = faceComp.computeFaces(embedding).find(_.vertexSize == 4).get
  }

  private def threeVertexOuter = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 1)
    val outerFace = faceComp.computeFaces(embedding).find(_.vertexSize == 3).get // Either works
  }

  describe("OuterFaceAugmentation") {

    it ("should augment a face with four vertices") {

      Given("an outer face with 4 vertices")
      val f = fourVertexOuter
      import f._
      val v1 = outerFace.vertices(0)
      val v2 = outerFace.vertices(1)
      val v3 = outerFace.vertices(2)
      val v4 = outerFace.vertices(3)

      When("augmenting the face with 4 additional vertices")
      val north = 5
      val east = 6
      val south = 7
      val west = 8
      val augmentation = new OuterFaceAugmentation(outerFace, north, east, south, west)
      val newEmbedding = augmentation.augmentEmbedding(embedding)
      val newFaces = faceComp.computeFaces(newEmbedding)

      Then("triangular edges between the new outer face and corners of the old one should be added")
      newFaces should contain (Face(south, v1, v2))
      newFaces should contain (Face(east, v2, v3))
      newFaces should contain (Face(north, v3, v4))
      newFaces should contain (Face(west, v4, v1))

    }

    it ("should add edges between external vertices") {

      Given("an outer face and four external vertices")
      val f = fourVertexOuter
      import f._
      val v1 = 5
      val v2 = 6
      val v3 = 7
      val v4 = 8

      When("augmenting the face with the external vertices")
      val augmentation = new OuterFaceAugmentation(outerFace, v1, v2, v3, v4)
      val newEmbedding = augmentation.augmentEmbedding(embedding)
      val newFaces = faceComp.computeFaces(newEmbedding)

      Then("the face v1 -> v4 should exists")
      assert(newFaces.exists(f => cycleOrder.compares(f.vertices, Vector(v4, v3, v2, v1))))

    }

    it ("should give the southern vertex three edges (as opposed to four) when the old outer face only has 3 vertices in it") {

      Given("a face with three vertices")
      val f = threeVertexOuter
      import f._
      val southOuter = 6
      val westOuter = 7
      val eastOuter = 5
      val northOuter = 4
      val inner1 = outerFace.vertices(0)
      val inner2 = outerFace.vertices(1)
      val inner3 = outerFace.vertices(2)

      When("augmenting the face with vertex 6 as the southern vertex")
      val augmentation = new OuterFaceAugmentation(outerFace, northOuter, eastOuter, southOuter, westOuter)
      val newEmbedding = augmentation.augmentEmbedding(embedding)
      val newFaces = faceComp.computeFaces(newEmbedding)

      Then("the faces South, 1, West and South, 1, East should exist")
      assert(newFaces.exists(f => cycleOrder.compareBothDirections(f.vertices, Vector(northOuter, inner3, westOuter))))
      assert(newFaces.exists(f => cycleOrder.compareBothDirections(f.vertices, Vector(northOuter, inner3, eastOuter))))

      And("the remaining corner faces should exist")
      assert(newFaces.exists(f => cycleOrder.compareBothDirections(f.vertices, Vector(westOuter, inner1, inner3))))
      assert(newFaces.exists(f => cycleOrder.compareBothDirections(f.vertices, Vector(southOuter, inner1, inner2))))
      assert(newFaces.exists(f => cycleOrder.compareBothDirections(f.vertices, Vector(eastOuter, inner2, inner3))))

    }

    it ("should augment edges deterministically") {

      Given("an embedded face")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 6).embed(6, 1)

      When("augmenting the face 1 ... 6 100 times")
      val outerFace = Face(1, 2, 3, 4, 5, 6)
      val firstAugment = new OuterFaceAugmentation(outerFace, 7, 8, 9, 10)
      val firstNewEmbedding = firstAugment.augmentEmbedding(embedding)

      Then("the same edges should be added every time")
      for (i <- 0 to 500) {
        val aug = new OuterFaceAugmentation(outerFace, 7, 8, 9, 10)
        val additionalEmbedding = aug.augmentEmbedding(embedding)

        GraphCommons.embeddingAsGraph(additionalEmbedding) should equal (GraphCommons.embeddingAsGraph(firstNewEmbedding))
      }

    }

    it ("should embed the neighbors of north such that east lies clockwise of west when the embedding has more than 3 vertices") {

      Given("an embedding")
      val f = fourVertexOuter
      import f._

      When("augmenting the embedding with four outer vertices")
      val north = 5
      val west = 6
      val east = 7
      val south = 8

      val augmentation = new OuterFaceAugmentation(outerFace, north, east, south, west)
      val newEmbedding = augmentation.augmentEmbedding(embedding)

      Then("the vertex east should lie clockwise from west")
      newEmbedding.embeddingFor(north).entryFor(west).next.adjacentVertex should be (east)

    }

    it ("should embed the neighbors of north such that east lies clockwise of west when the embedding only has 3 vertices") {

      Given("an embedding with 3 vertices")
      val f = threeVertexOuter
      import f._

      When("augmenting the embedding with four outer vertices")
      val north = 5
      val west = 6
      val east = 7
      val south = 8

      val augmentation = new OuterFaceAugmentation(outerFace, north, east, south, west)
      val newEmbedding = augmentation.augmentEmbedding(embedding)

      Then("the vertex east should lie clockwise from west")
      newEmbedding.embeddingFor(north).entryFor(west).next.adjacentVertex should be (east)

    }

    it ("should embed the neighbors of south such that west lies clockwise of east when the embedding has more than 3 vertices") {

      Given("an embedding")
      val f = fourVertexOuter
      import f._

      When("augmenting the embedding with four outer vertices")
      val north = 5
      val west = 6
      val east = 7
      val south = 8

      val augmentation = new OuterFaceAugmentation(outerFace, north, east, south, west)
      val newEmbedding = augmentation.augmentEmbedding(embedding)

      Then("the vertex west should lie clockwise from east")
      newEmbedding.embeddingFor(south).entryFor(east).next.adjacentVertex should be (west)

    }

    it ("should embed the neighbors of south such that west lies clockwise of east when the embedding only has 3 vertices") {

      Given("an embedding")
      val f = threeVertexOuter
      import f._

      When("augmenting the embedding with four outer vertices")
      val north = 5
      val west = 6
      val east = 7
      val south = 8

      val augmentation = new OuterFaceAugmentation(outerFace, north, east, south, west)
      val newEmbedding = augmentation.augmentEmbedding(embedding)

      Then("the vertex west should lie clockwise from east")
      newEmbedding.embeddingFor(south).entryFor(east).next.adjacentVertex should be (west)

    }

  }
}
