package rlgraph.unit.cartogram.rectangular.segmentHeuristic

import net.cyndeline.rlgraph.cartogram.rectangular.common.{Constraint, _}
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help.MoveDir._
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.help.{SegmentEntry, ValidMovement}
import rlgraph.SpecImports

class ValidMovementSpec extends SpecImports {
  private val moveComp = new ValidMovement()

  describe("ValidMovement") {

    it ("should return both Up and Down when both directions are available") {

      Given("three segments spaced more than 1 coordinate away from each other, with the middle segment constrained by the other two")
      val s1 = Segment(0, 0, X_Axis)
      val s2 = Segment(1, 4, X_Axis)
      val s3 = Segment(2, 8, X_Axis)
      val lowerConstraint = Constraint(s2, s1, LessThan)
      val upperConstraint = Constraint(s2, s3, GreaterThan)
      val entry = SegmentEntry(s2, Vector(upperConstraint), Vector(lowerConstraint), Vector(), X_Axis)

      When("checking which direction segment 2 can move in")
      val dirs = moveComp.validMovementDirections(entry)

      Then("both directions should be available")
      dirs should contain (Up)
      dirs should contain (Down)

    }

    it ("should not return anything when both directions are blocked") {

      Given("two segments within 1 coordinate of a middle segment")
      val s1 = Segment(0, 0, X_Axis)
      val middle = Segment(1, 1, X_Axis)
      val s3 = Segment(2, 2, X_Axis)
      val lowerConstraint = Constraint(middle, s1, LessThan)
      val upperConstraint = Constraint(middle, s3, GreaterThan)
      val entry = SegmentEntry(middle, Vector(upperConstraint), Vector(lowerConstraint), Vector(), X_Axis)

      When("checking which direction segment 2 can move in")
      val dirs = moveComp.validMovementDirections(entry)

      Then("no results should be returned")
      dirs should be ('empty)

    }

    it ("should only return Up when a segment is blocking from below") {

      Given("a segment with coordinate 1, and another segment with coordinate 2 constrained by the first")
      val s1 = Segment(0, 1, X_Axis)
      val s2 = Segment(1, 2, X_Axis)
      val lowerConstraint = Constraint(s2, s1, LessThan)
      val entry = SegmentEntry(s2, Vector(), Vector(lowerConstraint), Vector(), X_Axis)

      When("checking which direction segment 2 can move in")
      val dirs = moveComp.validMovementDirections(entry)

      Then("only one result should be found")
      dirs should have size 1

      And("it should be Up")
      dirs.head should be (Up)

    }

  }
}
