package rlgraph.unit.triangulation.util

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.triangulation.util.ZigZag
import rlgraph.SpecImports

class ZigZagSpec extends SpecImports {

  describe("ZigZag") {

    it ("should split a face with 4 vertices") {

      Given("a face 1, 2, 3, 4")
      val face = Face(1, 2, 3, 4)

      When("zig-zagging the face from 1 to 4")
      val zz = ZigZag(face, 1, 4)

      Then("an edge should be added between 2 and 4")
      zz.additionalEdges should be (Vector((2, 4)))

    }

    it ("should zig-zag a face with an even number of vertices") {

      Given("a face with 10 vertices")
      val face = Face(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      When("zig-zagging the face from 1 to 10")
      val zz = ZigZag(face, 1, 10)

      Then("the edges 2-10, 2-9, 9-3, 3-8, 8-4, 4-7, 7-5 should be produced")
      zz.additionalEdges should be (Vector((2, 10), (2, 9), (3, 9), (3, 8), (4, 8), (4, 7), (5, 7)))

    }

    it ("should zig-zag a face with an odd number of vertices") {

      Given("a face with 7 vertices")
      val face = Face(1, 2, 3, 4, 5, 6, 7)

      When("zig-zagging the face from 1 to 7")
      val zz = ZigZag(face, 1, 7)

      Then("the edges 2-7, 2-6, 3-6, 3-5")
      zz.additionalEdges should be (Vector((2, 7), (2, 6), (3, 6), (3, 5)))

    }

    it ("should zig-zag a face with first/last not at the edges of the list") {

      Given("a face with the vertices 1 -> 4 in the order 3, 4, 1, 2")
      val face = Face(3, 4, 1, 2)

      When("zig-zagging the face from 1 to 4")
      val zz = ZigZag(face, 1, 4)

      Then("an edge should be added between 2 and 4")
      zz.additionalEdges should be (Vector((2, 4)))

    }

    it ("should throw an exception when specifying a non-existent start/stop value") {

      Given("a face with vertices")
      val face = Face(1, 2, 3, 4, 5, 6, 7)

      When("zig-zagging the face using start = 8, stop = 9")
      Then("an exception should be thrown")
      intercept[Error] {
        ZigZag(face, 8, 4)
      }
      intercept[Error] {
        ZigZag(face, 1, 9)
      }

    }

    it ("should throw an exception when zig-zagging a face with less than 4 vertices") {

      Given("a face with 3 vertices")
      val face = Face(1, 2, 3)

      When("zig-zagging the face")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        ZigZag(face, 1, 3)
      }

    }

    it ("should thrown an exception when zig-zagging a face where a vertex appears more than once") {

      Given("a face where the vertex 1 appears twice")
      val face = Face(1, 2, 3, 1, 4)

      When("zig-zagging the face")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        ZigZag(face, 1, 3)
      }

    }
  }
}
