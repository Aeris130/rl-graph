package rlgraph.unit.cartogram.rectangular

import net.cyndeline.rlgraph.cartogram.rectangular.common.SegmentMap
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class SegmentMapSpec extends SpecImports {

  /**
   * Two vertices (1 and 2) having areas (0,0) to (2,2) and (2,0) to (3,2).
   */
  private def twoAdjacentAreas = new {
    val layout = new RectangularLayout(Vector((1, (0, 0), (2, 2)), (2, (2, 0), (3, 2))), Graph[Int, UnDiEdge](1, 2))
  }

  describe("SegmentMap") {

    it ("should compute four different segments for a single area") {

      Given("a rectangular layout with a vertex 1 represented by a single area from (0,0) to (2,3)")
      val layout = new RectangularLayout(Vector((1, (0, 0), (2, 3))), Graph[Int, UnDiEdge](1))

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)

      Then("the area of vertex 1 should be mapped to four segments")
      val a = layout.rectangles(1)
      val segmentsOf1 = segmentMap.segmentOfArea(a) // Error if not available

      And("the four segments should be distinct")
      Set(segmentsOf1.left, segmentsOf1.right, segmentsOf1.top, segmentsOf1.bottom) should have size 4

      And("the left segment should be 0")
      segmentsOf1.left.value should be (0)

      And("the right segment should be 2")
      segmentsOf1.right.value should be (2)

      And("the top segment should be 0")
      segmentsOf1.top.value should be (0)

      And("the bottom segment should be 3")
      segmentsOf1.bottom.value should be (3)

    }

    it ("should compute a common segment for two areas sharing an edge") {

      Given("a rectangular layout with the areas (0,0) to (2,2) and (2,0) to (3,2) sharing the edge (2,0) -> (2,2)")
      val f = twoAdjacentAreas
      import f._

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)

      Then("the right segment of area 1 should be the same object instance as the left segment of area 2")
      val segmentsOf1 = segmentMap.segmentOfArea(layout.rectangles(1))
      val segmentsOf2 = segmentMap.segmentOfArea(layout.rectangles(2))
      assert(segmentsOf1.right eq segmentsOf2.left, "Segments " + segmentsOf1.right + " and " + segmentsOf2.left + " did not share object instances")

    }

    it ("should compute a common segment for two areas sharing a boundary on the same axis") {

      Given("a rectangular layout with the areas (0,0) to (2,2) and (2,0) to (3,2) both using the segments (0,0)->(3,0) and (0,2)-> (3,2) as top and bottom")
      val f = twoAdjacentAreas
      import f._

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)
      val segmentsOf1 = segmentMap.segmentOfArea(layout.rectangles(1))
      val segmentsOf2 = segmentMap.segmentOfArea(layout.rectangles(2))

      Then("both areas should use the same segment instance as top")
      assert(segmentsOf1.top eq segmentsOf2.top, "Segments " + segmentsOf1.top + " and " + segmentsOf2.top + " did not share top segment instances")

      And("both areas should use the same segment instance as bottom")
      assert(segmentsOf1.bottom eq segmentsOf2.bottom, "Segments " + segmentsOf1.bottom + " and " + segmentsOf2.bottom + " did not share bottom segment instances")

    }

    it ("should assign a maximal segment to a rectangle that only uses a subset of it") {

      Given("two rectangles that share a common bottom interval, and a rectangle below them that shares a subset of it")
      val r1 = (1, (0, 0), (2, 2))
      val r2 = (2, (2, 0), (4, 2))
      val bottom = (3, (1, 2), (3, 4))
      val layout = new RectangularLayout(Vector(r1, r2, bottom), Graph[Int, UnDiEdge](1, 2, 3))

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)
      val segmentsOf1 = segmentMap.segmentOfArea(layout.rectangles(1))
      val segmentsOf2 = segmentMap.segmentOfArea(layout.rectangles(2))
      val segmentsOfBottom = segmentMap.segmentOfArea(layout.rectangles(3))

      Then("the top segment of the bottom area should be the same as the bottom of the two areas on top")
      assert(segmentsOfBottom.top eq segmentsOf1.bottom)
      assert(segmentsOfBottom.top eq segmentsOf2.bottom)

    }

    it ("should store the original rectangle data in the segment container") {

      Given("a rectangular layout with two areas")
      val f = twoAdjacentAreas
      import f._

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)
      val segmentsOf1 = segmentMap.segmentOfArea(layout.rectangles(1))
      val segmentsOf2 = segmentMap.segmentOfArea(layout.rectangles(2))

      Then("the segment entries should contain the original layout area of the vertex")
      segmentsOf1.rectangle should be (layout.rectangles(1))
      segmentsOf2.rectangle should be (layout.rectangles(2))

    }

    it ("should sort segments into vertical and horizontal") {

      Given("a rectangular layout with a vertex 1 represented by a single area from (0,0) to (2,3)")
      val layout = new RectangularLayout(Vector((1, (0, 0), (2, 3))), Graph[Int, UnDiEdge](1))

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)
      val segmentsOf1 = segmentMap.segmentOfArea(layout.rectangles(1))

      Then("the vertical segment list should contain the left and right segments of the area")
      segmentMap.verticalSegments should have size 2
      segmentMap.verticalSegments.toSet should be (Set(segmentsOf1.left, segmentsOf1.right))

      And("the horizontal segment list should contain the top and bottom segments of the area")
      segmentMap.horizontalSegments should have size 2
      segmentMap.horizontalSegments.toSet should be (Set(segmentsOf1.top, segmentsOf1.bottom))

    }

    it ("should use separate variable index counts for vertical and horizontal segments") {

      Given("a single area with two vertical and two horizontal segments")
      val layout = new RectangularLayout(Vector((1, (0, 0), (2, 3))), Graph[Int, UnDiEdge](1))

      When("mapping areas to maximal segments")
      val segmentMap = new SegmentMap(layout)

      Then("the vertical segment list should contain variable indices 0 and 1")
      segmentMap.verticalSegments.map(_.variableIndex) should be (Vector(0, 1))

      And("the horizontal segment list should contain variable indices 0 and 1")
      segmentMap.horizontalSegments.map(_.variableIndex) should be (Vector(0, 1))

    }

  }
}
