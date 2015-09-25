package rlgraph.unit.planar

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.planar.PlanarFaceAugmentation
import rlgraph.SpecImports

class PlanarFaceAugmentationSpec extends SpecImports {
  private val faceComp = new FaceComputation[String]()
  private val faceAug = new PlanarFaceAugmentation()

  def circularFaceAtoI = new {
    val embedding = UndirectedEmbedding[String]()
      .embed("A", "B").embed("B", "C").embed("C", "D").embed("D", "E").embed("E", "F").embed("F", "G").embed("G", "H").embed("H", "I").embed("I", "A")
    val face = Face("A", "B", "C", "D", "E", "F", "G", "H", "I")
  }

  describe("PlanarFaceAugmentation") {

    it ("should embed a single edge inside a face") {

      Given("a face with vertices A to I")
      val f = circularFaceAtoI
      import f._

      When("adding the edge B to F")
      val updatedEmbedding = faceAug.embedEdges(Vector(("B", "F")), face, embedding)

      Then("the resulting embedding should contain the faces [A, B, F, G, H, I] and [B, C, D, E, F]")
      val allFaces = faceComp.computeFaces(updatedEmbedding)
      allFaces should have size 3
      allFaces should contain (Face("A", "B", "F", "G", "H", "I"))
      allFaces should contain (Face("B", "C", "D", "E", "F"))

    }

    it ("should embed multiple edges to the same vertex when the vertex is below its neighbors") {

      Given("a face with vertices A to I")
      val f = circularFaceAtoI
      import f._

      When("adding edges E, F, G to vertex B")
      val updatedEmbedding = faceAug.embedEdges(Vector(("B", "F"), ("G", "B"), ("B", "E")), face, embedding)

      Then("the resulting embedding should contain the faces [B, C, D, E], [B, E, F] and [B, F, G]")
      val allFaces = faceComp.computeFaces(updatedEmbedding)
      allFaces should contain (Face("B", "C", "D", "E"))
      allFaces should contain (Face("B", "E", "F"))
      allFaces should contain (Face("B", "F", "G"))

    }

    it ("should embed multiple edges to the same vertex when the vertex is above its neighbors") {

      Given("a face with vertices A to I")
      val f = circularFaceAtoI
      import f._

      When("adding edges C, D, E to vertex G")
      val updatedEmbedding = faceAug.embedEdges(Vector(("G", "E"), ("G", "D"), ("G", "C")), face, embedding)

      Then("the resulting embedding should contain the faces [C, D, G], [D, E, G] and [E, F, G]")
      val allFaces = faceComp.computeFaces(updatedEmbedding)
      allFaces should contain (Face("C", "D", "G"))
      allFaces should contain (Face("D", "E", "G"))
      allFaces should contain (Face("E", "F", "G"))

    }

    it ("should embed edges around vertices that contains edges to both higher and lower neighbors") {

      Given("a face with vertices A to I")
      val f = circularFaceAtoI
      import f._

      When("adding edges B, C, G, H to vertex E")
      val updatedEmbedding = faceAug.embedEdges(Vector(("B", "E"), ("C", "E"), ("G", "E"), ("H", "E")), face, embedding)

      Then("the resulting embedding should contain the faces [B, C, E], [C, D, E], [E, F, G] and [E, G, H]")
      val allFaces = faceComp.computeFaces(updatedEmbedding)
      allFaces should contain (Face("B", "C", "E"))
      allFaces should contain (Face("C", "D", "E"))
      allFaces should contain (Face("E", "F", "G"))
      allFaces should contain (Face("E", "G", "H"))

    }

    it ("should embed multiple edges to the highest vertex") {

      Given("a face with vertices A to I")
      val f = circularFaceAtoI
      import f._

      When("adding edges E, F, G to vertex I")
      val updatedEmbedding = faceAug.embedEdges(Vector(("E", "I"), ("I", "F"), ("G", "I")), face, embedding)

      Then("the resulting embedding should contain the faces [E, F, I], [F, G, I] and [G, H, I]")
      val allFaces = faceComp.computeFaces(updatedEmbedding)
      allFaces should contain (Face("E", "F", "I"))
      allFaces should contain (Face("F", "G", "I"))
      allFaces should contain (Face("G", "H", "I"))

    }

    it ("should select the highest neighbor of v when embedding a neighbor with lower index than v, and no lower embedded edges are available") {

      Given("a face with vertex v = 6")
      val embedding = UndirectedEmbedding[Int]().embed(8, 7).embed(7, 6).embed(6, 1).embed(1, 4).embed(4, 2).embed(2, 8)
      val face = Face(8, 7, 6, 1, 4, 2)

      When("embedding edges from 6 to 2, 4 before embedding a lower neighbor 8")
      val edges = Vector((6, 2), (6, 4), (8, 6))
      val updatedEmbedding = faceAug.embedEdges(edges, face, embedding)

      Then("8 should be embedded after 2 in 6")
      val allFaces = new FaceComputation[Int]().computeFaces(updatedEmbedding)
      allFaces should contain (Face(6, 1, 4))
      allFaces should contain (Face(6, 4, 2))
      allFaces should contain (Face(6, 2, 8))
      allFaces should contain (Face(6, 8, 7))

    }

  }

}
