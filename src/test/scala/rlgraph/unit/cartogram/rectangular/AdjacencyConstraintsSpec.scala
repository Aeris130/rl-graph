package rlgraph.unit.cartogram.rectangular

import net.cyndeline.rlgraph.cartogram.rectangular.common.{SegmentsOfRectangle, _}
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.Rectangle
import rlgraph.SpecImports

class AdjacencyConstraintsSpec extends SpecImports {

  /*
   * Note: Segment id's are stated using negative values to make fixtures more readable.
   */

  /** Segments bounding two rectangular areas stacked on top of each other. Shares the left and right boundary. */
  private def twoAdjacentAreasVerticalWithSharedBoundaries = new {
    val r1 = Rectangle(0, 0, 2, 2)
    val r2 = Rectangle(2, 0, 2, 3)

    val topYSegment = Segment(-0, 0, Y_Axis)
    val leftXSegment = Segment(-1, 0, X_Axis)
    val rightXSegment = Segment(-2, 2, X_Axis)
    val bottomYSegment = Segment(-3, 3, Y_Axis)
    val midYSegment = Segment(-4, 2, Y_Axis)

    val r1Entry = SegmentsOfRectangle(r1, leftXSegment, rightXSegment, topYSegment, midYSegment)
    val r2Entry = SegmentsOfRectangle(r1, leftXSegment, rightXSegment, midYSegment, bottomYSegment)
  }

  /** Two areas stacked on top of each other. The coordinates of the second are differs from the first by 1 on
    * the x axis.
    */
  private def twoVerticalAreasSeparateBoundaries = new {
    val r1 = Rectangle(0, 0, 2, 2)
    val r2 = Rectangle(1, 0, 3, 3)

    val topYSegmentR1 = Segment(-0, 0, Y_Axis)
    val leftXSegmentR1 = Segment(-1, 0, X_Axis)
    val rightXSegmentR1 = Segment(-2, 2, X_Axis)

    val midYSegmentShared = Segment(-3, 2, Y_Axis)

    val leftXSegmentR2 = Segment(-4, 1, X_Axis)
    val rightXSegmentR2 = Segment(-5, 3, X_Axis)
    val bottomYSegmentR2 = Segment(-6, 3, Y_Axis)

    val r1Entry = SegmentsOfRectangle(r1, leftXSegmentR1, rightXSegmentR1, topYSegmentR1, midYSegmentShared)
    val r2Entry = SegmentsOfRectangle(r1, leftXSegmentR2, rightXSegmentR2, midYSegmentShared, bottomYSegmentR2)
  }

  /** An area with one neighbor above it, and one below. */
  private def areaWithTwoNeighborsAboveBelow = new {
    val neighborAbove = Rectangle(0, 0, 2, 2)
    val middle = Rectangle(1, 0, 3, 3)
    val neighborBelow = Rectangle(2, 3, 4, 4)

    val aboveTop = Segment(-0, 0, Y_Axis)
    val aboveLeft = Segment(-1, 0, X_Axis)
    val aboveRight = Segment(-2, 2, X_Axis)

    val aboveMiddleSharedY = Segment(-3, 2, Y_Axis)

    val middleLeft = Segment(-4, 1, X_Axis)
    val middleRight = Segment(-5, 3, X_Axis)

    val middleBelowSharedY = Segment(-6, 3, Y_Axis)

    val belowLeft = Segment(-7, 2, X_Axis)
    val belowRight = Segment(-8, 4, X_Axis)
    val belowBottom = Segment(-9, 4, Y_Axis)

    val aboveEntry = SegmentsOfRectangle(neighborAbove, aboveLeft, aboveRight, aboveTop, aboveMiddleSharedY)
    val middleEntry = SegmentsOfRectangle(middle, middleLeft, middleRight, aboveMiddleSharedY, middleBelowSharedY)
    val belowEntry = SegmentsOfRectangle(neighborBelow, belowLeft, belowRight, middleBelowSharedY, belowBottom)
  }

  private def areaWithTwoNeighborsSameSide = new {
    val leftNeighbor = Rectangle(0, 0, 2, 2)
    val rightNeighbor = Rectangle(2, 0, 4, 2)
    val rectangle = Rectangle(1, 2, 3, 4)

    val neighborsTop = Segment(-0, 0, Y_Axis)
    val leftNeighborLeft = Segment(-1, 0, X_Axis)
    val neighborsMiddleX = Segment(-2, 2, X_Axis)
    val rightNeighborRight = Segment(-3, 4, X_Axis)
    val neighborsBottom = Segment(-4, 2, Y_Axis)

    val rectangleLeft = Segment(-5, 1, X_Axis)
    val rectangleRight = Segment(-6, 3, X_Axis)
    val rectangleBottom = Segment(-7, 4, Y_Axis)

    val leftNeighborEntry = SegmentsOfRectangle(leftNeighbor, leftNeighborLeft, neighborsMiddleX, neighborsTop, neighborsBottom)
    val rightNeighborEntry = SegmentsOfRectangle(rightNeighbor, neighborsMiddleX, rightNeighborRight, neighborsTop, neighborsBottom)
    val rectangleEntry = SegmentsOfRectangle(rectangle, rectangleLeft, rectangleRight, neighborsBottom, rectangleBottom)
  }

  describe("AdjacencyConstraints") {

    it ("should not compute adjacency constraints for a single area with another area on top when the two rectangles share the same segments") {

      Given("two rectangles that share the same vertical x segments")
      val f = twoAdjacentAreasVerticalWithSharedBoundaries
      import f._

      When("computing vertical adjacency constraints for the rectangles")
      val constraintsR1 = new AdjacencyConstraints(r1Entry, Vector(r2Entry), X_Axis)
      val constraintsR2 = new AdjacencyConstraints(r2Entry, Vector(r1Entry), X_Axis)

      Then("no constraints above or below the first rectangle should be found")
      constraintsR1.constraints should be ('empty)
      constraintsR1.constraints should be ('empty)

      And("no constraints above or below the second rectangle should be found")
      constraintsR2.constraints should be ('empty)
      constraintsR2.constraints should be ('empty)

    }

    it ("should compute constraints for an area with a single other area adjacent to it") {

      Given("two areas with each other as vertical neighbors")
      val f = twoVerticalAreasSeparateBoundaries
      import f._

      When("computing vertical adjacency constraints for the rectangles")
      val constraintsR1 = new AdjacencyConstraints(r1Entry, Vector(r2Entry), X_Axis)
      val constraintsR2 = new AdjacencyConstraints(r2Entry, Vector(r1Entry), X_Axis)

      Then("rectangle 1 should demand that the left segment of rectangle 2 is lower than its right segment")
      constraintsR1.constraints should contain (Constraint(rightXSegmentR1, leftXSegmentR2, LessThan))

      And("rectangle 1 should demand that the right segment of rectangle 2 is greater than its left segment")
      constraintsR1.constraints should contain (Constraint(leftXSegmentR1, rightXSegmentR2, GreaterThan))

      And("rectangle 2 should demand that the left segment of rectangle 1 is lower than its right segment")
      constraintsR2.constraints should contain (Constraint(rightXSegmentR2, leftXSegmentR1, LessThan))

      And("rectangle 2 should demand that the right segment of rectangle 1 is greater than its left segment")
      constraintsR2.constraints should contain (Constraint(leftXSegmentR2, rightXSegmentR1, GreaterThan))

    }

    it ("should compute constraints for an area with neighbors on both side of it") {

      Given("a middle rectangle with adjacent neighbors on both sides")
      val f = areaWithTwoNeighborsAboveBelow
      import f._

      When("computing vertical adjacency constraints for the middle rectangle")
      val constraintsR1 = new AdjacencyConstraints(middleEntry, Vector(aboveEntry, belowEntry), X_Axis)

      Then("the left segments of both neighbors should be less than the right segment of the middle rectangle")
      constraintsR1.constraints should contain (Constraint(middleEntry.right, aboveEntry.left, LessThan))
      constraintsR1.constraints should contain (Constraint(middleEntry.right, belowEntry.left, LessThan))

      And("the right segments of both neighbors should be greater than the left segment of the middle rectangle")
      constraintsR1.constraints should contain (Constraint(middleEntry.left, aboveEntry.right, GreaterThan))
      constraintsR1.constraints should contain (Constraint(middleEntry.left, belowEntry.right, GreaterThan))

    }

    it ("should compute constraints for an area with two neighbors on the same side") {

      Given("a rectangle with two neighbors above it")
      val f = areaWithTwoNeighborsSameSide
      import f._

      When("computing vertical adjacency constraints for the lower rectangle")
      val RectangleConstraints = new AdjacencyConstraints(rectangleEntry, Vector(leftNeighborEntry, rightNeighborEntry), X_Axis)

      Then("the right segment of the left neighbor should be greater than the rectangles left segment")
      RectangleConstraints.constraints should contain (Constraint(rectangleEntry.left, leftNeighborEntry.right, GreaterThan))

      And("the left segment of the right neighbor should be less than the rectangles right segment")
      RectangleConstraints.constraints should contain (Constraint(rectangleEntry.right, rightNeighborEntry.left, LessThan))

      And("no entry should exist for the middle neighbor segment")
      RectangleConstraints.constraints should have size 2

    }


  }
}
