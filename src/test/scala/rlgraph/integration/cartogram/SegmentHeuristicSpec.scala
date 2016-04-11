package rlgraph.integration.cartogram

import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.SegmentHeuristic
import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout
import rlgraph.SpecImports
import rlgraph.help.ConstraintRoom

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class SegmentHeuristicSpec extends SpecImports {
  private val maxAspectRatioMovement = new SegmentHeuristic[ConstraintRoom, UnDiEdge](999)

  private def singleRoomSize2Target16 = new {
    val room = ConstraintRoom(4, 4)
    val roomArea = (room, (0, 0), (1, 1))
    val graph = Graph[ConstraintRoom, UnDiEdge](room)
    val layout = new RectangularLayout(Vector(roomArea), graph)
  }

  describe("SegmentHeuristic") {

    it ("should expand a single rectangle") {

      Given("a layout with a rectangle of size 2 belonging to a room with target area 16")
      val f = singleRoomSize2Target16
      import f._

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("the area of the room should cover 4x4 coordinates from (0, 0) to (3, 3)")
      val r = optimizedLayout.rectangles(room)
      r.startX should be (0)
      r.startY should be (0)
      r.stopX should be (3)
      r.stopY should be (3)

    }

    it ("should shrink a rectangle down to its target size") {

      Given("a layout with a rectangle of size 5x5 belonging to a room with target are 9")
      val room = ConstraintRoom(3, 3)
      val roomArea = (room, (0, 0), (4, 4))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(roomArea), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("the area of the room should cover 3x3 coordinates")
      val r = optimizedLayout.rectangles(room)
      r.stopX - r.startX should be (2)
      r.stopY - r.startY should be (2)

    }

    it ("should not expand a rectangle if it is already at the maximum aspect ratio") {

      Given("a layout with a rectangle of size 2 belonging to a room with target size 99")
      val room = ConstraintRoom(33, 3)
      val roomArea = (room, (0, 0), (1, 1))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(roomArea), graph)

      When("optimizing segments using a heuristic with max aspect ratio 1")
      val heuristic = new SegmentHeuristic[ConstraintRoom, UnDiEdge](1)
      val optimizedLayout = heuristic.applyToLayout(layout)

      Then("the area of the room should still cover 2x2 coordinates")
      val r = optimizedLayout.rectangles(room)
      r.stopX - r.startX should be (1)
      r.stopY - r.startY should be (1)

    }

    it ("should expand one side more than the other if needed to hit the target size") {

      Given("a layout with a rectangle of size 2 belonging to a room with target size 6")
      val room = ConstraintRoom(2, 3)
      val roomArea = (room, (0, 0), (1, 1))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(roomArea), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("the area of the room should still cover 3x2 coordinates")
      val r = optimizedLayout.rectangles(room)
      r.stopX - r.startX should be (2)
      r.stopY - r.startY should be (1)

    }

    /**
     * Note: This test also checks that the layout has its coordinates adjusted inwards from 0. If not, then
     * the left rectangle wouldn't have enough space to expand to the right, and would become thinner than
     * the right rectangle.
     */
    it ("should expand multiple multiple rectangles to hit the target area") {

      Given("a layout with two rectangles of size 3 with a target size 16")
      val room1 = ConstraintRoom(4, 4)
      val room2 = ConstraintRoom(4, 4)
      val roomArea1 = (room1, (0, 0), (2, 2))
      val roomArea2 = (room2, (2, 0), (4, 2))
      val graph = Graph[ConstraintRoom, UnDiEdge](room1~room2)
      val layout = new RectangularLayout(Vector(roomArea1, roomArea2), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("room 1 should have the size 4x4")
      val r1 = optimizedLayout.rectangles(room1)
      r1.stopX - r1.startX should be (3)
      r1.stopY - r1.startY should be (3)

      And("room 2 should have the size 4x4")
      val r2 = optimizedLayout.rectangles(room2)
      r2.stopX - r2.startX should be (3)
      r2.stopY - r2.startY should be (3)

    }

    it ("should adjust coordinates towards 0") {

      Given("a layout with a rectangle above coordinate 0")
      val room = ConstraintRoom(2, 3)
      val roomArea = (room, (4, 4), (5, 5))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(roomArea), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("the start coordinates should be 0")
      val r = optimizedLayout.rectangles(room)
      optimizedLayout.minX should be (0)
      optimizedLayout.minY should be (0)
      r.startX should be (0)
      r.startY should be (0)

    }

    it ("should not optimize a rectangle marked as sea rectangle") {

      Given("a layout with a rectangle of size 2 belonging to a room with target area 16")
      val f = singleRoomSize2Target16
      import f._

      When("optimizing segments with the room marked as an exception")
      val optimizedLayout = maxAspectRatioMovement.applyToLayoutWithExceptions(layout, Set(room))

      Then("the rooms size should remain 2x2")
      val r = optimizedLayout.rectangles(room)
      r.stopX - r.startX should be (1)
      r.stopY - r.startY should be (1)

    }

    it ("should disregard the aspect ratio of sea rectangles") {

      Given("a layout with three rectangles, 2 horizontal neighbors and a rectangle below that spans them both, with the right neighbor having target size 8")
      val room1 = ConstraintRoom(2, 2) // Left neighbor, doesn't need expanding
      val room2 = ConstraintRoom(2, 4) // Right neighbor, would achieve the target size if expanded to the right
      val room3 = ConstraintRoom(2, 2) // Bottom neighbor, doesn't need expanding
      val roomArea1 = (room1, (0, 0), (1, 1))
      val roomArea2 = (room2, (1, 0), (2, 1))
      val roomArea3 = (room3, (0, 1), (2, 2))
      val graph = Graph[ConstraintRoom, UnDiEdge](room1~room2, room2~room3, room3~room1)
      val layout = new RectangularLayout(Vector(roomArea1, roomArea2, roomArea3), graph)

      When("optimizing segments using a heuristic with max aspect ratio 2") // Room 3 is already at 2
      val heuristic = new SegmentHeuristic[ConstraintRoom, UnDiEdge](2)
      val optimizedLayout = heuristic.applyToLayoutWithExceptions(layout, Set(room3))

      Then("room 1 should maintain its size")
      val r1 = optimizedLayout.rectangles(room1)
      r1.stopX - r1.startX should be (1)
      r1.stopY - r1.startY should be (1)

      And("room 2 should have width 4 and height 2")
      val r2 = optimizedLayout.rectangles(room2)
      r2.stopX - r2.startX should be (3)
      r2.stopY - r2.startY should be (1)

      And("room 3 should have width 5 and height 2, despite 5/2 > aspect ratio 2")
      val r3 = optimizedLayout.rectangles(room3)
      r3.stopX - r3.startX should be (4)
      r3.stopY - r3.startY should be (1)

    }

    it("should respect adjacency constraints") {

      Given("a layout with two upper and two lower rectangles, with the separating vertical segment of the upper " +
        "ones is to the right of the lower separating segment. The upper right and lower left have size 4 and target size 6, " +
        "and the upper left and lower right have size 6 and target size 4")
      val upperLeft = ConstraintRoom(2, 2)
      val upperRight = ConstraintRoom(2, 3)
      val lowerLeft = ConstraintRoom(2, 2)
      val lowerRight = ConstraintRoom(2, 3)
      val roomArea1 = (upperLeft, (0, 0), (2, 1))
      val roomArea2 = (upperRight, (2, 0), (3, 1))
      val roomArea3 = (lowerLeft, (0, 1), (1, 2))
      val roomArea4 = (lowerRight, (1, 1), (3, 2))

      // Important to get the graph to accurately require all areas to have 3 other neighbors
      val graph = Graph[ConstraintRoom, UnDiEdge](upperLeft~upperRight, upperRight~lowerRight, lowerRight~lowerLeft, lowerLeft~upperLeft, upperLeft~lowerRight)
      val layout = new RectangularLayout(Vector(roomArea1, roomArea2, roomArea3, roomArea4), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("neither separating segment should be able to switch the size of its neighboring rooms without running into constraints")
      val r1 = optimizedLayout.rectangles(upperLeft)
      r1.stopX - r1.startX should be (2)
      r1.stopY - r1.startY should be (1)

      val r2 = optimizedLayout.rectangles(upperRight)
      r2.stopX - r2.startX should be (1)
      r2.stopY - r2.startY should be (1)

      val r3 = optimizedLayout.rectangles(lowerLeft)
      r3.stopX - r3.startX should be (1)
      r3.stopY - r3.startY should be (1)

      val r4 = optimizedLayout.rectangles(lowerRight)
      r4.stopX - r4.startX should be (2)
      r4.stopY - r4.startY should be (1)

    }

    it ("should ignore gate rectangles") {

      Given("a layout with two rooms having size 2 and target size 16, with a gate between them")
      val room1 = ConstraintRoom(1, 16)
      val room2 = ConstraintRoom(1, 16)
      val roomArea1 = (room1, (0, 0), (1, 1))
      val gateArea = ((room1, room2), (1, 0), (2, 1))
      val roomArea2 = (room2, (2, 0), (3, 1))
      val graph = Graph[ConstraintRoom, UnDiEdge](room1~room2)
      val layout = new RectangularLayout(Vector(roomArea1, roomArea2), Vector(gateArea), graph)

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("room 1 should have the size 4x4")
      val r1 = optimizedLayout.rectangles(room1)
      r1.stopX - r1.startX should be (3)
      r1.stopY - r1.startY should be (3)

      And("room 2 should have the size 4x4")
      val r2 = optimizedLayout.rectangles(room1)
      r2.stopX - r2.startX should be (3)
      r2.stopY - r2.startY should be (3)

    }

    it ("should reduce large rectangles") {

      Given("a rectangle of size 100x100")
      val room1 = ConstraintRoom(3, 3)
      val roomArea1 = (room1, (0, 0), (100, 100))
      val layout = new RectangularLayout(Vector(roomArea1), Graph[ConstraintRoom, UnDiEdge](room1))

      When("optimizing segments")
      val optimizedLayout = maxAspectRatioMovement.applyToLayout(layout)

      Then("the area of the room should cover 3x3 coordinates")
      val r = optimizedLayout.rectangles(room1)
      r.stopX - r.startX should be (2)
      r.stopY - r.startY should be (2)

    }

    it ("should allow a segment movement that causes some rectangles to increase their aspect ratio beyond the maximum amount, if doing so " +
      "causes even more rectangles to reduce their invalid ratios") {

      Given("two rectangles A and D having perfect size and ratio, with two rectangles B, C between them with too high ratio and too high size")
      val a = ConstraintRoom(2, 2)
      val b = ConstraintRoom(2, 2)
      val c = ConstraintRoom(2, 2)
      val d = ConstraintRoom(3, 3)
      val aArea = (a, (0, 0), (2, 2)) // Area 2x2, target 2x2, AR 1
      val bArea = (b, (2, 0), (20, 1)) // Area 2x19, target 2x2
      val cArea = (c, (2, 1), (20, 2)) // Area 2x19, target 2x2
      val dArea = (d, (20, 0), (22, 2)) // Area 2x2, target 2x2, AR 1
      val layout = new RectangularLayout(Vector(aArea, bArea, cArea, dArea), Graph(a~b, a~c, b~c, c~d, b~d))

      When("optimizing segments")
      val heuristic = new SegmentHeuristic[ConstraintRoom, UnDiEdge](1)
      val optimizedLayout = heuristic.applyToLayout(layout)

      Then("area A and D should should have the same width")
      val rA = optimizedLayout.rectangles(a)
      val rD = optimizedLayout.rectangles(d)
      assert(rA.stopX - rA.startX == 2)
      assert(rD.stopX - rD.startX == 2)

      And("areas B and C should be less wide than initially")
      val rB = optimizedLayout.rectangles(b)
      val rC = optimizedLayout.rectangles(c)
      assert(rB.stopX - rB.startX < 18) // 20 - 2
      assert(rC.stopX - rC.startX < 18)

      And("no change should be made to the height of any rectangle")
      rA.startY should be (0)
      rA.stopY should be (2)
      rB.startY should be (0)
      rB.stopY should be (1)
      rC.startY should be (1)
      rC.stopY should be (2)
      rD.startY should be (0)
      rD.stopY should be (2)

    }

  }
}
