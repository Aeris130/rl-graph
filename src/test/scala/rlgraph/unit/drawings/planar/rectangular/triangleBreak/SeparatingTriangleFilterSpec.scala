package rlgraph.unit.rectangular.triangleBreak

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.SeparatingTriangleFilter
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import rlgraph.SpecImports

class SeparatingTriangleFilterSpec extends SpecImports {

  describe("SeparatingTriangleFilter") {

    it ("should handle the trivial case") {

      Given("a triangle <1,2,3> and the faces [1,2,3] and [3,2,1]")
      val f1 = Face(1, 2, 3)
      val f2 = Face(1, 2, 3)
      val t1 = Triangle(2, 1, 3)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1), Vector(f1, f2))

      Then("no triangle should be returned")
      filter.triangles should be ('empty)

    }

    it ("should return a triangle not matching a face in the trivial case") {

      Given("the faces [1,2,3] and [3,2,1] and a triangle that matches neither")
      val f1 = Face(1, 2, 3)
      val f2 = Face(1, 2, 3)
      val t1 = Triangle(2, 1, 5)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1), Vector(f1, f2))

      Then("the triangle should be returned")
      filter.triangles should be (Vector(t1))

    }

    it ("should remove a triangle that matches a face") {

      Given("the faces 1::[1, 2, 3], 2::[4, 3, 5], 3::[6, 8, 7] and the triangle <5, 4, 3> matching face 2")
      val f1 = Face(1, 2, 3)
      val f2 = Face(4, 3, 5)
      val f3 = Face(6, 8, 7)
      val t1 = Triangle(5, 4, 3)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1), Vector(f1, f2, f3))

      Then("no triangle should be returned")
      filter.triangles should be ('empty)

    }

    it ("should keep a triangle that doesn't math any face") {

      Given("the faces 1::[1, 2, 3], 2::[4, 3, 5], 3::[6, 8, 7] and a triangle that doesn't match either of them")
      val f1 = Face(1, 2, 3)
      val f2 = Face(4, 3, 5)
      val f3 = Face(6, 8, 7)
      val t1 = Triangle(3, 9, 1)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1), Vector(f1, f2, f3))

      Then("the triangle should be returned")
      filter.triangles should be (Vector(t1))

    }

    it ("should return an empty list if no triangles are specified") {

      Given("some faces and an empty triangle list")
      val f1 = Face(1, 2, 3)
      val f2 = Face(4, 3, 5)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(), Vector(f1, f2))

      Then("no triangle should be returned")
      filter.triangles should be ('empty)

    }

    it ("should return every triangle if no face is specified") {

      Given("some triangles and an empty face list")
      val t1 = Triangle(3, 9, 1)
      val t2 = Triangle(4, 10, 88)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1, t2), Vector())

      Then("all triangles should be returned")
      filter.triangles.toSet should be (Set(t1, t2))

    }

    it ("should both remove matching triangles and keep non-matching ones") {

      Given("a number of faces and a triangle list containing both matching and non-matching triangles")
      val f1 = Face(99, 23, 11)
      val f2 = Face(4, 3, 5)
      val f3 = Face(1, 2, 3)
      val f4 = Face(9, 19, 18)
      val matching1 = Triangle(5, 4, 3)
      val nonMatching1 = Triangle(100, 1, 5)
      val matching2 = Triangle(18, 19, 9)
      val nonMatching2 = Triangle(1, 2, 333)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(matching1, nonMatching1, matching2, nonMatching2), Vector(f1, f2, f3, f4))

      Then("both non-matching triangles should be returned")
      filter.triangles.toSet should be (Set(nonMatching1, nonMatching2))

    }

    it ("should return every remaining triangles if there are triangles left, but no unmatched faces to compare with") {

      Given("a a single face that matches the first triangle in the list, followed by another triangle that sorts after the first lexicographically")
      val face = Face(1, 2, 3)
      val t1 = Triangle(1, 2, 3)
      val t2 = Triangle(4, 5, 6)

      When("filtering separating triangles")
      val filter = new SeparatingTriangleFilter(Vector(t1, t2), Vector(face))

      Then("the second triangle should be returned")
      filter.triangles should be (Vector(t2))

    }
  }

}
