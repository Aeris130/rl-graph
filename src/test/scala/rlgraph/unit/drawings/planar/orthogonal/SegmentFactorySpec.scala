package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.SegmentFactory
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{Dart, VertexWrapper}
import rlgraph.SpecImports

class SegmentFactorySpec extends SpecImports {
  val segmentFactory = new SegmentFactory()

  /**
   * Example taken from fig. 4.3 in A Combinatorial Approach to Orthogonal Placement Problems by Gunnar Werner Klau.
   */
  def exampleFace = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val v4 = new VertexWrapper(4, "v4")
    val v5 = new VertexWrapper(5, "v5")
    val v6 = new VertexWrapper(6, "v6")
    val v7 = new VertexWrapper(7, "v7")

    // Extra vertex not found in the example. Needed in order to create a vertical single-vertex segment.
    val v8 = new VertexWrapper(8, "v8")

    // External face
    val v6_v7 = new DefaultDart[String](v6, v7, 1, 0, Option(Up))
      val v7_v8 = new DefaultDart[String](v7, v8, 4, 0, Option(Right))
      val v8_v7 = new DefaultDart[String](v8, v7, 2, 0, Option(Left))
    val v7_v4 = new DefaultDart[String](v7, v4, 1, 0, Option(Left))
    val v4_v5 = new DefaultDart[String](v4, v5, 3, 0, Option(Up))
    val v5_v3 = new DefaultDart[String](v5, v3, 3, 0, Option(Left))
    val v3_v2 = new DefaultDart[String](v3, v2, 2, 0, Option(Down))
    val v2_v1 = new DefaultDart[String](v2, v1, 4, 0, Option(Down))
    val v1_v2 = new DefaultDart[String](v1, v2, 1, 0, Option(Up))
    val v2_v4 = new DefaultDart[String](v2, v4, 2, 0, Option(Right))
    val v4_v7 = new DefaultDart[String](v4, v7, 1, 0, Option(Right))
    val v7_v6 = new DefaultDart[String](v7, v6, 4, 0, Option(Down))

    // Internal face
    val v2_v3 = new DefaultDart[String](v2, v3, 1, 0, Option(Up))
    val v3_v5 = new DefaultDart[String](v3, v5, 1, 0, Option(Right))
    val v5_v4 = new DefaultDart[String](v5, v4, 1, 0, Option(Down))
    val v4_v2 = new DefaultDart[String](v4, v2, 1, 0, Option(Left))

    val dartSet = Set[Dart[String]](v6_v7, v7_v8, v8_v7, v7_v4, v4_v5, v5_v3, v3_v2, v2_v1, v1_v2, v2_v4, v4_v7, v7_v6, v2_v3, v3_v5, v5_v4, v4_v2)
  }

  describe("SegmentFactory") {

    it ("should add a segment for every connected directed component and every vertex not belonging to a component in some direction, for half the directed darts") {

      Given("two faces with 5 components and 3 vertices that doesn't belong to a component in one direction")
      val f = exampleFace
      import f._

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(dartSet)

      Then("there should be 8 segments")
      segments should have size 8

    }

    /*
     *
     *  Horizontal tests
     *
     */

    it ("should add a segment for every horizontal (Right) dart") {

      Given("faces with darts")
      val f = exampleFace
      import f._

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(dartSet)

      Then("there should be a segment v3 -> v5")
      val v3_v5_segment = findSegment(segments, Vector(v3, v5))

      assert(v3_v5_segment != null, Vector(v3, v5) + " was not found.")
      v3_v5_segment.orientation should be (Horizontal)

      And("there should be a segment v2 -> v4 -> v7")
      val v2_v4_v7_segment = findSegment(segments, Vector(v2, v4, v7, v8))

      assert(v2_v4_v7_segment != null, Vector(v2, v4, v7, v8) + " was not found.")
      v2_v4_v7_segment.orientation should be (Horizontal)

    }

    it ("should add vertices not belonging to a horizontal (Right) dart into their own segments") {

      Given("a face with two vertices (v1, v6) only belonging to a vertical dart")
      val f = exampleFace
      import f._

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(dartSet)

      Then("there should be a segment containing the single vertex v1")
      val v1_segment = findSegment(segments, Vector(v1))

      assert(v1_segment != null, Vector(v1) + " was not found.")
      v1_segment.orientation should be (Horizontal)

      And("there should be a segment containing the single vertex v6")
      val v6_segment = findSegment(segments, Vector(v6))

      assert(v6_segment != null, Vector(v6) + " was not found.")
      v6_segment.orientation should be (Horizontal)

    }

    it ("should add vertices not belonging to a horizontal segment into their own segment despite there not being any darts in that direction") {

      Given("two darts pointing in vertical directions")
      val v1 = new VertexWrapper(1, "v1")
      val v2 = new VertexWrapper(2, "v2")
      val d1 = new DefaultDart[String](v1, v2, 4, 0, Option(Up))
      val d2 = new DefaultDart[String](v2, v1, 4, 0, Option(Down))
      val darts = Set[Dart[String]](d1, d2)

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(darts)

      Then("there should be a horizontal segment containing v1")
      val v1_seg = findSegment(segments, Vector(v1))
      assert(v1_seg != null, Vector(v1) + " was not found.")

      And("there should be a horizontal segment containing v2")
      val v2_seg = findSegment(segments, Vector(v1))
      assert(v2_seg != null, Vector(v2) + " was not found.")

    }

    /*
     *
     *  Vertical tests
     *
     */

    it ("should add a segment for every vertical (upwards) dart") {

      Given("faces with darts")
      val f = exampleFace
      import f._

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(dartSet)

      Then("there should be a segment v1 -> v2 -> 3")
      val aSeg = Vector(v1, v2, v3)
      val v1_v2_v3_segment = findSegment(segments, aSeg)

      assert(v1_v2_v3_segment != null, aSeg + " was not found.")
      v1_v2_v3_segment.orientation should be (Vertical)

      And("there should be a segment v4 -> v5")
      val bSeg = Vector(v4, v5)
      val v4_v5_segment = findSegment(segments, bSeg)

      assert(v4_v5_segment != null, bSeg + " was not found.")
      v4_v5_segment.orientation should be (Vertical)

      And("there should be a segment v6 -> v7")
      val cSeg = Vector(v6, v7)
      val v6_v7_segment = findSegment(segments, cSeg)

      assert(v6_v7_segment != null, cSeg + " was not found.")
      v6_v7_segment.orientation should be (Vertical)

    }

    it ("should add vertices not belonging to a vertical (Upwards) dart into their own segments") {

      Given("a face with vertex v8 not belonging to a vertical dart")
      val f = exampleFace
      import f._

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(dartSet)

      Then("there should be a segment containing the single vertex v8")
      val aSeg = Vector(v8)
      val v8_segment = findSegment(segments, aSeg)

      assert(v8_segment != null, aSeg + " was not found.")
      v8_segment.orientation should be (Vertical)

    }

    it ("should add vertices not belonging to a vertical segment into their own segment despite there not being any darts in that direction") {

      Given("two darts pointing in horizontal directions")
      val v1 = new VertexWrapper(1, "v1")
      val v2 = new VertexWrapper(2, "v2")
      val d1 = new DefaultDart[String](v1, v2, 4, 0, Option(Left))
      val d2 = new DefaultDart[String](v2, v1, 4, 0, Option(Right))
      val darts = Set[Dart[String]](d1, d2)

      When("computing segments")
      val segments: Set[Segment[String]] = segmentFactory.computeSegments(darts)

      Then("there should be a vertical segment containing v1")
      val v1_seg = findSegment(segments, Vector(v1))
      assert(v1_seg != null, Vector(v1) + " was not found.")

      And("there should be a vertical segment containing v2")
      val v2_seg = findSegment(segments, Vector(v1))
      assert(v2_seg != null, Vector(v2) + " was not found.")

    }


  }

  private def findSegment(segments: Set[Segment[String]], vertices: Vector[VertexWrapper[String]]): Segment[String] = {
    for (segment <- segments)
      if (segment.vertices == vertices)
        return segment

    null
  }
}
