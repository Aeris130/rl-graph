package rlgraph.unit.face

import net.cyndeline.rlgraph.face.{Face, FaceMembershipManager}
import rlgraph.SpecImports

class FaceMembershipManagerSpec extends SpecImports {

  describe("FaceMembershipManager") {

    it ("should map edges to their left faces") {

      Given("a face 1 -> 2 -> 3")
      val rightFace = new Face(Vector(1, 2, 3))
      val leftFace = new Face(Vector(3, 2, 1))
      val membershipManager = new FaceMembershipManager[Int](Vector(leftFace, rightFace))

      When("checking the face to the left of (1,2), (2,3) and (3,1)")
      val f12 = membershipManager.leftFace(1, 2)
      val f23 = membershipManager.leftFace(2, 3)
      val f31 = membershipManager.leftFace(3, 1)

      Then("the specified face should be to the left of all of them")
      f12 should be (leftFace)
      f23 should be (leftFace)
      f31 should be (leftFace)

    }

    it ("should map edges to their right faces") {

      Given("a faceA : 1 -> 2 -> 3 and a face B : 3 -> 2 -> 1")
      val A = new Face(Vector(1, 2, 3))
      val B = new Face(Vector(3, 2, 1))
      val membershipManager = new FaceMembershipManager[Int](Vector(A, B))

      When("checking the face to the right of (1,2)")
      val rightOf12 = membershipManager.rightFace(1, 2)

      Then("the resulting face should be the same as the one to the left of 2,1")
      rightOf12 should equal (membershipManager.leftFace(2, 1))

    }

    it ("should throw an exception when checking faces for non-existing edges") {

      Given("a membership manager")
      val membershipManager = new FaceMembershipManager[Int](Vector())

      When("checking for left and right faces of an edge that isn't mapped in the manager")
      Then("an exception should be thrown")
      intercept[NoSuchElementException] {
        membershipManager.leftFace(1, 2)
      }
      intercept[NoSuchElementException] {
        membershipManager.rightFace(1, 2)
      }
    }

  }
}
