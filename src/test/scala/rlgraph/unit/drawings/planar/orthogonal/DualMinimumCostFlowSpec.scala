package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.DualMinimumCostFlow.DualMinimumCostFlow
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.VertexWrapper
import rlgraph.SpecImports

import scalax.collection.edge.Implicits._
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

class DualMinimumCostFlowSpec extends SpecImports {
  private val dualMinCostFlow = new DualMinimumCostFlow()

  def singleSegmentPath = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val s1 = new Segment(Horizontal, v1)
    val s2 = new Segment(Horizontal, v2)
    val s3 = new Segment(Horizontal, v3)

    val e1 = (s1~%+#>s2)(1, "Label e1")
    val e2 = (s2~%+#>s3)(1, "Label e2")

    val shapeGraph = Graph.from(Nil, List(e1, e2))
  }

  def sourceToTwoSinks = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val s1 = new Segment(Horizontal, v1)
    val s2 = new Segment(Horizontal, v2)
    val s3 = new Segment(Horizontal, v3)

    val e1 = (s1~%+#>s2)(1, "Label e1")
    val e2 = (s1~%+#>s3)(1, "Label e2")

    val shapeGraph = Graph.from(Nil, List(e1, e2))
  }

  def twoSourcetoTwoSinks = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val v4 = new VertexWrapper(4, "v4")
    val v5 = new VertexWrapper(5, "v5")
    val s1 = new Segment(Horizontal, v1)
    val s2 = new Segment(Horizontal, v2)
    val s3 = new Segment(Horizontal, v3)
    val s4 = new Segment(Horizontal, v4)
    val s5 = new Segment(Horizontal, v5)

    val e1 = (s1~%+#>s3)(1, "Label e1")
    val e2 = (s2~%+#>s3)(1, "Label e2")
    val e3 = (s3~%+#>s4)(1, "Label e3")
    val e4 = (s3~%+#>s5)(1, "Label e4")

    val shapeGraph = Graph.from(Nil, List(e1, e2, e3, e4))
  }

  describe("DualMinimumCostFlow") {

    /* Since it's possible for source and sink to get swapped during transformation (depending on how the edges
     * are drawn), only their relation to each other can be tested
     */
    it ("should add successive coordinates to a single path of segments") {

      Given("a shape graph with 3 segments having relation s1 -> s2 -> s3")
      val f = singleSegmentPath
      import f._

      When("assigning segment coordinates")
      val segmentsWithCoordinates: Set[Segment[String]] = dualMinCostFlow.assignSegmentCoordinates(shapeGraph)
      val s1WithCoordinate = segmentsWithCoordinates.find(s => s == s1).get
      val s2WithCoordinate = segmentsWithCoordinates.find(s => s == s2).get
      val s3WithCoordinate = segmentsWithCoordinates.find(s => s == s3).get

      val s1c = s1WithCoordinate.coordinate
      val s2c = s2WithCoordinate.coordinate
      val s3c = s3WithCoordinate.coordinate

      /* The order of coordinates should be 1 < 2 < 3 or 1 > 2 > 3. Checking 2's relation to 1 or 3 can be used to
       * determine which.
       */
      val s1LowerThanS2 = s2c > s1c

      Then("the coordinate of s2 should be between s1 and s3")
      s2c should be (1)

      And("the coordinate of s1 should conform to the relation")
      if (s1LowerThanS2)
        s1c should be (0)
      else
        s1c should be (2)

      And("the coordinate of s3 should conform to the relation")
      if (s1LowerThanS2)
        s3c should be (2)
      else
        s3c should be (0)

    }

    it ("should assign the same coordinates to segments at the same distance from its parent") {

      Given("a segment s1 with two outgoing edges to separate segments (s2 and s3)")
      val f = sourceToTwoSinks
      import f._

      When("assigning segment coordinates")
      val segmentsWithCoordinates: Set[Segment[String]] = dualMinCostFlow.assignSegmentCoordinates(shapeGraph)
      val s1WithCoordinate = segmentsWithCoordinates.find(s => s == s1).get
      val s2WithCoordinate = segmentsWithCoordinates.find(s => s == s2).get
      val s3WithCoordinate = segmentsWithCoordinates.find(s => s == s3).get

      Then("segment 2 and 3 should have the same coordinates")
      s2WithCoordinate.coordinate should equal (s3WithCoordinate.coordinate)

      And("segment 1 should have a different coordinate")
      s1WithCoordinate.coordinate should not equal s2WithCoordinate.coordinate

    }

    /*
     * This tests checks that adding super-sources or sinks doesn't push the coordinates for regular
     * segments ahead by 1.
     */
    it ("should assign some segment the coordinate 0 when the shape-graph has multiple sources/sinks") {

      Given("two segments s1 and s2 pointing at segment s3, and segment s3 pointing at segments s4 and s5")
      val f = twoSourcetoTwoSinks
      import f._

      When("assigning segment coordinates")
      val segmentsWithCoordinates: Set[Segment[String]] = dualMinCostFlow.assignSegmentCoordinates(shapeGraph)

      Then("two of the segments should have coordinates 0")
      val segmentsWithCoordinate0: Set[Segment[String]] = segmentsWithCoordinates.filter(_.coordinate == 0)
      segmentsWithCoordinate0.size should be (2)

      And("the segments should be either s1/s2 or s4/s5")
      val a = Set(s1, s2)
      val b = Set(s4, s5)
      assert(segmentsWithCoordinate0 == a || segmentsWithCoordinate0 == b, "Segments " + segmentsWithCoordinate0 + " did not match " + a + " or " + b)

    }

    /*
     * +/- since there's no way to tell if the segment s3 ends up being the sink having coordinate 0 with all other
     * coordinates above it, or vice verse.
     */
    it ("should add the minimum distance to a parent of a segment, if that parent isn't affected by other segments") {

      Given("a shape graph with segments pointing as: s1 -> s2 -> s3, and a segment s4 pointing at s3 as well")
      val f = singleSegmentPath
      import f._
      val v4 = new VertexWrapper(4, "v4")
      val s4 = new Segment(Horizontal, v4)
      val e3 = (s4~%+#>s3)(1, "Label e3")

      val shapeGraphWithExtraSegment = Graph.from(Nil, List(e1, e2, e3))

      When("assigning segment coordinates")
      val segmentsWithCoordinates: Set[Segment[String]] = dualMinCostFlow.assignSegmentCoordinates(shapeGraphWithExtraSegment)

      Then("the coordinate of s4 should be the same as s2 since they appear on the same level (1 distance short of s3)")
      val s2WithCoordinate = segmentsWithCoordinates.find(s => s == s2).get
      val s4WithCoordinate = segmentsWithCoordinates.find(s => s == s4).get

      s2WithCoordinate.coordinate should equal (s4WithCoordinate.coordinate)

    }

    it ("should compute distance between segments connected by multiple edges") {

      Given("two segments s1 and s2 connected by multiple edges")
      val v1 = new VertexWrapper(1, "v1")
      val v2 = new VertexWrapper(2, "v2")
      val s1 = new Segment(Horizontal, v1)
      val s2 = new Segment(Horizontal, v2)

      val e1 = (s1~%+#>s2)(1, "Label e1")
      val e2 = (s1~%+#>s2)(1, "Label e2")
      val e3 = (s1~%+#>s2)(1, "Label e3")

      val shapeGraph = Graph.from(Nil, List(e1, e2, e3))

      When("assigning segment coordinates")
      val segmentsWithCoordinates: Set[Segment[String]] = dualMinCostFlow.assignSegmentCoordinates(shapeGraph)

      Then("the difference in coordinates between the two segments should be 1")
      val s1WithCoordinate = segmentsWithCoordinates.find(s => s == s1).get
      val s2WithCoordinate = segmentsWithCoordinates.find(s => s == s2).get

      Math.abs(s1WithCoordinate.coordinate - s2WithCoordinate.coordinate) should be (1)

    }

    it ("should throw an exception when attempting to compute flow for an empty graph") {

      Given("an empty graph")
      val f = singleSegmentPath
      import f._
      val emptyGraph: Graph[Segment[String], WLkDiEdge] = shapeGraph.empty

      When("assigning segment coordinates")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        dualMinCostFlow.assignSegmentCoordinates(emptyGraph)
      }

    }
  }

}
