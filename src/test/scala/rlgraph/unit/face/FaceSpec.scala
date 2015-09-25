package rlgraph.unit.face

import net.cyndeline.rlgraph.face.Face
import rlgraph.SpecImports

class FaceSpec extends SpecImports {

  describe("Face") {

    it ("should return the vertex input list") {

      Given("a list of vertices 1, 2, 3")
      When("creating a face")
      val face = Face(1, 2, 3)

      Then("the vertex list in the face should be 1, 2, 3")
      face.vertices should be (Vector(1, 2, 3))

    }
    it ("should consider two face instances equal if they have the same vertex lists") {

      Given("two faces with the same vertex lists")
      val face1 = new Face(Vector(1, 2, 3))
      val face2 = new Face(Vector(1, 2, 3))

      When("comparing the faces")
      Then("they should be considered equal")
      face1 should equal (face2)

    }

    it ("should consider two face instances equal if they have the same vertex lists but in different order") {

      Given("two faces with the same vertex lists in different order")
      val face1 = new Face(Vector(1, 2, 3))
      val face2 = new Face(Vector(2, 3, 1))

      When("comparing the faces")
      Then("they should be considered equal")
      face1 should equal (face2)

    }

    it ("should consider two face instances with a repeating vertex equal if they have the same vertex lists but in different order") {

      Given("two faces with the same vertex lists in different order, and the repeating vertex 1")
      val face1 = new Face(Vector(1, 2, 1, 3))
      val face2 = new Face(Vector(2, 1, 3, 1))

      When("comparing the faces")
      Then("they should be considered equal")
      face1 should equal (face2)

    }

    it ("should return the same hash code for two faces with equal vertex lists in different order") {

      Given("two faces with the same vertex lists in different order")
      val face1 = new Face(Vector(1, 2, 3))
      val face2 = new Face(Vector(2, 3, 1))

      When("computing hash codes")
      Then("both faces should have the same hash code")
      face1.## should equal (face2.##)

    }

    it ("should return every edge in a circular face") {

      Given("a face 1, 2, 3, 4")
      val face = Face(1, 2, 3, 4)

      When("computing the edges")
      val edges = face.edges

      Then("the edge size should equal the size of the edge list")
      edges should have size face.edgeSize

      And("the edges should be 1~2, 2~3, 3~4, 4~1")
      edges should be (Vector((1, 2), (2, 3), (3, 4), (4, 1)))

    }

    it ("should return every edge in a face with repeating vertices") {

      Given("a face 1, 2, 3, 4, 5, 3, 6")
      val face = Face(1, 2, 3, 4, 5, 3, 6)

      When("computing the edges")
      val edges = face.edges

      Then("the edge size should equal the size of the edge list")
      edges should have size face.edgeSize

      And("the edges should be 1~2, 2~3, 3~4, 4~5, 5~3, 3~6, 6~1")
      edges should be (Vector((1, 2), (2, 3), (3, 4), (4, 5), (5, 3), (3, 6), (6, 1)))

    }

    it ("should map edges to their prior neighbor") {

      Given("the vertices 1, 2, 3, 4")
      val vs = Vector(1, 2, 3, 4)

      When("constructing a face")
      val face = new Face(vs)

      Then("the prior mapping 2 -> 1, 3 -> 2, 4 -> 3, 1 -> 4 should be created")
      face.vertexPriorTo(1) should be (4)
      face.vertexPriorTo(2) should be (1)
      face.vertexPriorTo(3) should be (2)
      face.vertexPriorTo(4) should be (3)

    }

    it ("should map edges to their next neighbor") {

      Given("the vertices 1, 2, 3, 4")
      val vs = Vector(1, 2, 3, 4)

      When("constructing a face")
      val face = new Face(vs)

      Then("the prior mapping 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1 should be created")
      face.vertexPresentAfter(1) should be (2)
      face.vertexPresentAfter(2) should be (3)
      face.vertexPresentAfter(3) should be (4)
      face.vertexPresentAfter(4) should be (1)

    }

  }

}
