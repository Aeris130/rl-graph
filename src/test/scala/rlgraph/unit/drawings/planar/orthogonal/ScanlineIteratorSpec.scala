package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.improvementHeuristic.CornerType._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.improvementHeuristic.{ScanlineIterator, SegmentCorner}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.VertexWrapper
import rlgraph.SpecImports

class ScanlineIteratorSpec extends SpecImports {

  def sixCorners = new {
    val v1 = new VertexWrapper(1, "v1")
    val segment1 = new Segment(Horizontal, v1)
    val corner1_top = new SegmentCorner(segment1, 0, 3, Top)
    val corner1_bottom = new SegmentCorner(segment1, 0, 1, Bottom)

    val v2 = new VertexWrapper(2, "v2")
    val segment2 = new Segment(Horizontal, v2)
    val corner2_top = new SegmentCorner(segment2, 0, 5, Top)
    val corner2_bottom = new SegmentCorner(segment2, 0, 2, Bottom)

    val v3 = new VertexWrapper(3, "v3")
    val segment3 = new Segment(Horizontal, v3)
    val corner3_top = new SegmentCorner(segment3, 0, 4, Top)
    val corner3_bottom = new SegmentCorner(segment3, 0, 0, Bottom)

    val v4 = new VertexWrapper(4, "v4")
    val segment4 = new Segment(Horizontal, v4)
    val corner4_top = new SegmentCorner(segment4, 0, 4, Top)
    val corner4_bottom = new SegmentCorner(segment4, 0, 0, Bottom)
  }

  describe("ScanlineIterator") {

    it ("should throw an exception when supplied a segment corner missing its opposite") {

      Given("a single segment corner")
      val v1 = new VertexWrapper(1, "v1")
      val segment = new Segment(Horizontal, v1)
      val corner = new SegmentCorner(segment, 0, 1, Top)

      When("constructing a scanline iterator")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new ScanlineIterator(corner)
      }

    }

    it ("should throw an exception when supplied two segment corners that doesn't have opposite types") {

      Given("two segment corner woth the type Top")
      val v1 = new VertexWrapper(1, "v1")
      val segment1 = new Segment(Horizontal, v1)
      val corner1 = new SegmentCorner(segment1, 0, 1, Top)

      val v2 = new VertexWrapper(2, "v2")
      val segment2 = new Segment(Horizontal, v2)
      val corner2 = new SegmentCorner(segment2, 0, 1, Top)

      When("constructing a scanline iterator")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new ScanlineIterator(corner1, corner2)
      }

    }

    it ("should return corners in order of their secondary coordinate") {

      Given("given two segment corners 5->2 (v2) and 3->1 (v1)")
      val f = sixCorners
      import f._

      When("constructing a scanline iterator")
      val it = new ScanlineIterator(corner1_top, corner1_bottom, corner2_top, corner2_bottom)

      Then("the corners should be returned in the order 5, 3, 2, 1")
      it.next() should be ((5, Set(corner2_top)))
      it.next() should be ((3, Set(corner1_top)))
      it.next() should be ((2, Set(corner2_bottom)))
      it.next() should be ((1, Set(corner1_bottom)))

    }

    it ("should group corners with the same secondary coordinates into the same set") {

      Given("two segments having the same top and bottom coordinates (4->0) in their corners (v3, v4)")
      val f = sixCorners
      import f._

      When("constructing a scanline iterator")
      val it = new ScanlineIterator(corner3_top, corner3_bottom, corner4_top, corner4_bottom)

      Then("the first entry should contain both segments tops")
      it.next() should be ((4, Set(corner3_top, corner4_top)))

      And("the second entry should contain both bottoms")
      it.next() should be ((0, Set(corner3_bottom, corner4_bottom)))

    }

    it ("should report hasNext as true as long as there's an element left") {

      Given("an iterator with two corners")
      val f = sixCorners
      import f._
      val it = new ScanlineIterator(corner1_top, corner1_bottom)

      When("calling hasNext before remiving any elements")
      val result = it.hasNext

      Then("the result should be true")
      result should be (true)

      And("the result should still be true after removing one element")
      it.next()
      it.hasNext should be (true)

      And("the result should be false after removing the last element")
      it.next()
      it.hasNext should be (false)

    }
  }
}
