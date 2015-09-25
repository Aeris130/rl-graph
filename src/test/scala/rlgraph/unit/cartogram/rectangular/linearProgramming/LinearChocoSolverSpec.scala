package rlgraph.unit.cartogram.rectangular.linearProgramming

import net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.linearSolver.LinearChocoSolver
import rlgraph.SpecImports

class LinearChocoSolverSpec extends SpecImports {
  private val dummyLowerValue = 0
  private val dummyHigherValue = 1
  private val dummyTargetArea = 1

  describe("LinearChocoSolver") {

    it ("should impose adjacency constraints") {

      Given("a solver with constraints that segments 0, 1 and 2 should have the order 0, 1, 2")
      val solver = new LinearChocoSolver(3, 1)
      solver.registerAdjacencyConstraint(0, 1)
      solver.registerAdjacencyConstraint(1, 2)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("each segment should have the same coordinate as its index")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (1)
      solution.segmentCoordinates(2) should be (2)

    }

    it ("should impose planarity constraints on rectangles") {

      Given("a solver with two rectangles using segments 0,1 and 1,2")
      val solver = new LinearChocoSolver(3, 1)
      solver.registerRectangle(0, 1, dummyLowerValue, dummyHigherValue, dummyTargetArea)
      solver.registerRectangle(1, 2, dummyLowerValue, dummyHigherValue, dummyTargetArea)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("each segment should have the same coordinate as its index")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (1)
      solution.segmentCoordinates(2) should be (2)

    }

    it ("should prioritize the maximum aspect ratio when a rectangle attempts to increase in size") {

      Given("a solver with maximum aspect ratio 1:1 and a rectangle with segments 0|1 on both axis")
      val solver = new LinearChocoSolver(2, 1)

      When("adding a rectangle with target size 2")
      solver.registerRectangle(0, 1, 0, 1, 2)
      val solution = solver.computeOptimalSolution

      Then("the coordinates should still be 0 and 1 on the current axis")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (1)

    }

    it ("should impose size constraints on a single rectangle") {

      Given("a solver that allows an aspect ratio divergence of 2:1 and a rectangle with segments 0|1 on both axis's and target area 2")
      val solver = new LinearChocoSolver(2, 2)
      solver.registerRectangle(0, 1, 0, 1, 2)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0, and segment 1 should have coordinate 2")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (2)

    }

    it ("should impose size constraints on a multiple rectangles") {

      Given("a solver that allows an aspect ratio divergence of 2:1 and two rectangles with segments 0|1|2 on the " +
        "current axis, and 0|1 on the opposite, with both rectangles having target area 2")
      val solver = new LinearChocoSolver(3, 2)
      val oppositeLower = 0
      val oppositeHigher = 1
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, 2)
      solver.registerRectangle(1, 2, oppositeLower, oppositeHigher, 2)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0")
      solution.segmentCoordinates(0) should be (0)

      And("segment 1 should have coordinate 2")
      solution.segmentCoordinates(1) should be (2)

      And("segment 2 should have coordinate 4")
      solution.segmentCoordinates(2) should be (4)

    }

    /**
     * ###################        ###################
     * # 1x1    #1x1     #        #        #        #
     * #        #        #        #        #        #
     * ################### --->   ###################
     * # 2x1(Too big)    #            #1x1(OK)#
     * #                 #            #       #
     * ###################            #########
     *
     *
     */
    it ("should adjust coordinates if needed to respect adjacency constraints") {

      Given("a solver with maximum aspect ratio 1:1 and three rectangles, with the bottom spanning the top 2 with a ratio 2:1")
      val solver = new LinearChocoSolver(5, 3)
      val top = 0
      val middle = 1
      val bottom = 2

      /* If every rectangle has size 1, there won't be any free coordinates left for the third rectangle to shrink into.
       * To allow this, the upper two has size 3.
       */
      solver.registerRectangle(0, 1, top, middle, 3)
      solver.registerRectangle(1, 2, top, middle, 3)

      /* Despite that the picture above shows that the three rectangles shares the left/right-most edges, this test
       * assigns the bottom rectangle a separate pair of segments 3 and 4, to allow them to move independently. */
      solver.registerRectangle(3, 4, middle, bottom, 1)
      solver.registerAdjacencyConstraint(3, 1)
      solver.registerAdjacencyConstraint(1, 4)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("the bottom rectangles sides should move as closely as possible to the middle segment")
      solution.segmentCoordinates(3) should be (2)
      solution.segmentCoordinates(4) should be (4)

    }

    it ("should scale a rooms segments to match a target size > 2 (2 being its minimum)") {

      Given("a solver with aspect ratio 4, and a rectangle with segments 0|1 on the current axis, and 0|1 on the other, with a target size 4")
      val solver = new LinearChocoSolver(2, 4)
      val oppositeLower = 0
      val oppositeHigher = 1
      val targetSize = 4
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, targetSize)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0, and segment 1 should have coordinate 4")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (4)

    }

    it ("should scale a rooms segment as far as possible, if the ratio prohibits it from reaching its target size") {

      Given("a solver that only allows aspect ratio 3, and a rectangle with segments 0|1 on the current axis, and 0|1 on the other, with a target size 4")
      val solver = new LinearChocoSolver(2, 3)
      val oppositeLower = 0
      val oppositeHigher = 1
      val targetSize = 4
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, targetSize)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0, and segment 1 should have coordinate 3")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (3)

    }

    it ("should scale a rooms segment if a real aspect ratio allows it") {

      Given("a solver with aspect ratio 1.5, and a rectangle with segments 0|1 on the current axis, and 0|2 on the other, with a target size 3 on the x axis")
      val solver = new LinearChocoSolver(2, 1.5)
      val oppositeLower = 0
      val oppositeHigher = 2
      val targetArea = 6 // x(3) * y(2)
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, targetArea)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0, and segment 1 should have coordinate 3")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (3)

    }

    /**
     * ##############       ################
     * #?x2  #?x2   #  -->  #2x4     #2x2  #
     * #     #      #       #        #     #
     * ##############       ################
     */
    it ("should scale one of two rooms if they request different target areas") {

      Given("a solver with two rectangles, once with target are 8, and another with target area 4, and opposite axis values 0|2")
      val solver = new LinearChocoSolver(3, 2)
      val oppositeLower = 0
      val oppositeHigher = 2
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, 8)
      solver.registerRectangle(1, 2, oppositeLower, oppositeHigher, 4)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("segment 0 should have coordinate 0")
      solution.segmentCoordinates(0) should be (0)

      And("the segment between the two rectangles should have coordinate 4")
      solution.segmentCoordinates(1) should be (4)

      And("the ending segment for rectangle 2 should have coordinate 6")
      solution.segmentCoordinates(2) should be (6)

    }

    it ("should scale a segment to the max when the maximum aspect ratio is higher than the target area") {

      Given("a solver with max aspect ratio 50 and segments 0|1 having target size 9 and with opposite side having size 1")
      val solver = new LinearChocoSolver(3, 50)
      val oppositeLower = 0
      val oppositeHigher = 1
      solver.registerRectangle(0, 1, oppositeLower, oppositeHigher, 9)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("the segments should be 0 and 9")
      solution.segmentCoordinates(0) should be (0)
      solution.segmentCoordinates(1) should be (9)

    }

    it ("should return a higher error the bigger difference there is between the final area of a rectangle, and the target area") {

      Given("three solvers with aspect ratio 1:1 and a rectangle with segments 0|1 on both axises, having rectangles with target area 1, 2 and 3")
      val segmentNr = 2
      val oppositeLower = 0
      val oppositeHigher = 1
      val solver1 = new LinearChocoSolver(segmentNr, 1)
      val solver2 = new LinearChocoSolver(segmentNr, 1)
      val solver3 = new LinearChocoSolver(segmentNr, 1)

      solver1.registerRectangle(0, 1, oppositeLower, oppositeHigher, 1)
      solver2.registerRectangle(0, 1, oppositeLower, oppositeHigher, 2)
      solver3.registerRectangle(0, 1, oppositeLower, oppositeHigher, 3)

      When("computing the optimal solution")
      val solution1 = solver1.computeOptimalSolution
      val solution2 = solver2.computeOptimalSolution
      val solution3 = solver3.computeOptimalSolution

      Then("solver 1 should have 0 error")
      solution1.sumOfErrors should be (0)

      And("solver 2 should have error = target size - actual size")
      solution2.sumOfErrors should be (1)

      And("solver 3 should have error = target size - actual size")
      solution3.sumOfErrors should be (2)

    }

    it ("should return -1 as coordinate for non-registered segment indices") {

      Given("a solver with size 3 and only indices 0 and 2 as registered segments")
      val solver = new LinearChocoSolver(3, 2)
      solver.registerRectangle(0, 2, 0, 1, 8)

      When("computing the optimal solution")
      val solution = solver.computeOptimalSolution

      Then("index 1 should be -1")
      solution.segmentCoordinates(1) should be (-1)

    }

  }

}
