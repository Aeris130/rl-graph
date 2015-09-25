package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic.VisibilityGraphFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.VertexWrapper
import rlgraph.SpecImports

class VisibilityGraphFactorySpec extends SpecImports {
  private val visibilityFactory = new VisibilityGraphFactory[String]()

  def vertices = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val v4 = new VertexWrapper(4, "v4")
    val v5 = new VertexWrapper(5, "v5")
    val v6 = new VertexWrapper(6, "v6")
    val v7 = new VertexWrapper(7, "v7")
    val v8 = new VertexWrapper(8, "v8")
    val v9 = new VertexWrapper(9, "v9")
    val v10 = new VertexWrapper(10, "v10")

  }

  def complexExample = new {
    val f = vertices
    import f._

    // Upward
    val s1 = new Segment(Vertical, v7, v1).newCoordinate(0)
    val s2 = new Segment(Vertical, v5, v3).newCoordinate(1)
    val s3 = new Segment(Vertical, v8, v6).newCoordinate(2)
    val s4 = new Segment(Vertical, v9, v4).newCoordinate(3)
    val s5 = new Segment(Vertical, v10, v2).newCoordinate(4)

    // Rightward
    val s6 = new Segment(Horizontal, v1, v2).newCoordinate(3)
    val s7 = new Segment(Horizontal, v3, v4).newCoordinate(2)
    val s8 = new Segment(Horizontal, v5, v6).newCoordinate(1)
    val s9 = new Segment(Horizontal, v7, v8).newCoordinate(0)
    val s10 = new Segment(Horizontal, v9, v10).newCoordinate(0)

    val segments = Set(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)
  }

  describe("VisibilityGraphFactory") {

    it ("should add a segment -> right-neighbor edge between two segments that overlap") {

      Given("a segment s1 going from 5 to 3, and a segment s2 from 4 to 1 to the right of it")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s3 = new Segment(Horizontal, v1).newCoordinate(5)
      val s4 = new Segment(Horizontal, v2).newCoordinate(3)
      val s5 = new Segment(Horizontal, v3).newCoordinate(4)
      val s6 = new Segment(Horizontal, v4).newCoordinate(1)

      val segments = Set(s1, s2, s3, s4, s5, s6)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("only one edge should be added")
      verticalShapeGraph.edges.size should be (1)

      And("there should be an edge from s1 to s2")
      val edges = verticalShapeGraph.get(s1).outgoing.toVector
      edges(0).to should be (s2)
    }

    it ("should add a left-neighbor -> segment edge between two segments that overlap") {

      Given("a segment s2 going from 4 to 1, and a segment s1 from 5 to 3 to the right of it")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(2)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(1)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s3 = new Segment(Horizontal, v1).newCoordinate(5)
      val s4 = new Segment(Horizontal, v2).newCoordinate(3)
      val s5 = new Segment(Horizontal, v3).newCoordinate(4)
      val s6 = new Segment(Horizontal, v4).newCoordinate(1)

      val segments = Set(s1, s2, s3, s4, s5, s6)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("only one edge should be added")
      verticalShapeGraph.edges.size should be (1)

      And("there should be an edge from s2 to s1")
      val edges = verticalShapeGraph.get(s2).outgoing.toVector
      edges(0).to should be (s1)
    }

    it ("should add an edge between two segments ending and beginning on the same coordinate") {

      Given("a segment s1 that goes from 4 to 2, and a segment s2 that goes from 2 to 0")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s3 = new Segment(Horizontal, v1).newCoordinate(4)
      val s4 = new Segment(Horizontal, v2).newCoordinate(2)
      val s5 = new Segment(Horizontal, v3).newCoordinate(2)
      val s6 = new Segment(Horizontal, v4).newCoordinate(0)

      val segments = Set(s1, s2, s3, s4, s5, s6)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("there should be an edge from s1 to s2")
      val edges = verticalShapeGraph.get(s1).outgoing.toVector
      edges(0).to should be (s2)

    }

    it ("should not add edges between segments with no overlapping coordinates") {

      Given("a segment s1 that goes from 6 to 3, and a segment s2 that goes from 2 to 1")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s3 = new Segment(Horizontal, v1).newCoordinate(6)
      val s4 = new Segment(Horizontal, v2).newCoordinate(3)
      val s5 = new Segment(Horizontal, v3).newCoordinate(2)
      val s6 = new Segment(Horizontal, v4).newCoordinate(1)

      val segments = Set(s1, s2, s3, s4, s5, s6)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("no edges should be added")
      assert(verticalShapeGraph.edges.size == 0, "Some edges were added despite no overlapping coordinates: " + verticalShapeGraph.edges)

    }

    it ("should add two edges left->s->right if segment s has neighbors left and right at the same coordinate when its top coordinate is scanned") {

      Given("three segments s1, s2, s3 (in that order) starting at the same coordinate")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)
      val s3 = new Segment(Vertical, v6, v5).newCoordinate(3)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s4 = new Segment(Horizontal, v1).newCoordinate(10)
      val s5 = new Segment(Horizontal, v2).newCoordinate(0)
      val s6 = new Segment(Horizontal, v3).newCoordinate(10)
      val s7 = new Segment(Horizontal, v4).newCoordinate(0)
      val s8 = new Segment(Horizontal, v5).newCoordinate(10)
      val s9 = new Segment(Horizontal, v6).newCoordinate(0)

      val segments = Set(s1, s2, s3, s4, s5, s6, s7, s8, s9)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("two edges should be added")
      verticalShapeGraph.edges.size should be (2)

      And("there should be an edge from s1 to s2") // One for s1's top, and 1 for s2's
      val edges1 = verticalShapeGraph.get(s1).outgoing.toVector
      edges1(0).to should be (s2)

      And("there should be an edge from s2 to s3") // One for s2's top, and 1 for s3's
      val edges2 = verticalShapeGraph.get(s2).outgoing.toVector
      edges2(0).to should be (s3)

    }

    it ("should add an edge between the left/right neighbors of a segment s if the two can still see each other when s ends") {

      Given("a segment s2 with left neighbor s1 and right neighbor s3, both having start coordinates inside s2's interval, and continuing after s2 has ended")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)
      val s3 = new Segment(Vertical, v6, v5).newCoordinate(3)

      // Opposite segments used to pinpoint top and bottom coordinates
      // s1
      val s4 = new Segment(Horizontal, v1).newCoordinate(4)
      val s5 = new Segment(Horizontal, v2).newCoordinate(0)

      //s2
      val s6 = new Segment(Horizontal, v3).newCoordinate(6)
      val s7 = new Segment(Horizontal, v4).newCoordinate(2)

      //s3
      val s8 = new Segment(Horizontal, v5).newCoordinate(4)
      val s9 = new Segment(Horizontal, v6).newCoordinate(0)

      val segments = Set(s1, s2, s3, s4, s5, s6, s7, s8, s9)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("there should be an edge from s1 to s3 despite them not seeing each other when their beginnings are scanned")
      val edges1 = verticalShapeGraph.get(s1).outgoing.toVector
      val edgesToS3 = edges1.filter(e => e.to == s3)
      edgesToS3.size should be (1)

    }

    it ("should add an edge between the left- and right neighbors of a consecutive list of segments ending at the same coordinate") {

      Given("a left segment s1, a right segment s4, and two intermediate segments s2, s3 ending at the same coordinate (before s1 and s4)")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v4, v3).newCoordinate(2)
      val s3 = new Segment(Vertical, v6, v5).newCoordinate(3)
      val s4 = new Segment(Vertical, v8, v7).newCoordinate(4)

      // Opposite segments used to pinpoint top and bottom coordinates
      // s1
      val s5 = new Segment(Horizontal, v1).newCoordinate(10)
      val s6 = new Segment(Horizontal, v2).newCoordinate(0)

      //s2
      val s7 = new Segment(Horizontal, v3).newCoordinate(10)
      val s8 = new Segment(Horizontal, v4).newCoordinate(5) // Ending on the same coordinate as s3

      //s3
      val s9 = new Segment(Horizontal, v5).newCoordinate(10)
      val s10 = new Segment(Horizontal, v6).newCoordinate(5)

      //s4
      val s11 = new Segment(Horizontal, v7).newCoordinate(10)
      val s12 = new Segment(Horizontal, v8).newCoordinate(0)

      val segments = Set(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("there should be an edge between s1 and s4 despite them not seeing each other when either of them begins")
      val edges1 = verticalShapeGraph.get(s1).outgoing.toVector
      val edgesToS3 = edges1.filter(e => e.to == s4)
      edgesToS3.size should be (1)

    }

    it ("should add edges based on segments beginning and ending on the same vertex and coordinate") {

      Given("a segment s1, and a segment s2 containing a single vertex with a coordinate inside s1's interval")
      val f = vertices
      import f._

      // Segments used to build shape graph
      val s1 = new Segment(Vertical, v2, v1).newCoordinate(1)
      val s2 = new Segment(Vertical, v3).newCoordinate(2)

      // Opposite segments used to pinpoint top and bottom coordinates
      val s3 = new Segment(Horizontal, v1).newCoordinate(5)
      val s4 = new Segment(Horizontal, v2).newCoordinate(3)
      val s5 = new Segment(Horizontal, v3).newCoordinate(4)

      val segments = Set(s1, s2, s3, s4, s5)

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("there should be an edge between s1 and s2")
      val edges1 = verticalShapeGraph.get(s1).outgoing.toVector
      edges1(0).to should be (s2)

    }

    /*
     *  Two examples from figure 4.17 in A Combinatorial Approach to orthogonal placement problems, p. 66
     *
     *  Both examples involves solving for the set of segments being assigned edges in the example. Example 1 is
     *  the vertical segments, example 2 the horizontal.
     */

    it ("should solve horizontal edges between vertical segments for a complex example") {

      Given("a set of horizontal and vertical segments")
      val f = complexExample
      import f._

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val verticalShapeGraph = graphs._2

      Then("7 edges should be added")
      assert(verticalShapeGraph.edges.size == 7, "The edges in " + verticalShapeGraph + " was not 7")

      And("there should be an edge 1->2")
      val edges1to2 = verticalShapeGraph.get(s1).outgoing.find(edge => edge.to == s2)
      assert(edges1to2.isDefined, "No edge from segment 1 -> 2 found")

      And("there should be an edge 1->3")
      val edges1to3 = verticalShapeGraph.get(s1).outgoing.find(edge => edge.to == s3)
      assert(edges1to3.isDefined, "No edge from segment 1 -> 3 found")

      And("there should be an edge 1->5")
      val edges1to5 = verticalShapeGraph.get(s1).outgoing.find(edge => edge.to == s5)
      assert(edges1to5.isDefined, "No edge from segment 1 -> 5 found")

      And("there should be an edge 2->3")
      val edges2to3 = verticalShapeGraph.get(s2).outgoing.find(edge => edge.to == s3)
      assert(edges2to3.isDefined, "No edge from segment 2 -> 3 found")

      And("there should be an edge 2->4")
      val edges2to4 = verticalShapeGraph.get(s2).outgoing.find(edge => edge.to == s4)
      assert(edges2to4.isDefined, "No edge from segment 2 -> 4 found")

      And("there should be an edge 3->4")
      val edges3to4 = verticalShapeGraph.get(s3).outgoing.find(edge => edge.to == s4)
      assert(edges3to4.isDefined, "No edge from segment 3 -> 4 found")

      And("there should be an edge 4->5")
      val edges4to5 = verticalShapeGraph.get(s4).outgoing.find(edge => edge.to == s5)
      assert(edges4to5.isDefined, "No edge from segment 4 -> 5 found")

    }

    it ("should solve vertical edges between vertical segments for a complex example") {

      Given("a set of horizontal and vertical segments")
      val f = complexExample
      import f._

      When("computing shape graphs")
      val graphs = visibilityFactory.buildVisibilityGraphs(segments)
      val horizontalShapeGraph = graphs._1

      Then("6 edges should be added")
      assert(horizontalShapeGraph.edges.size == 6, horizontalShapeGraph.edges.size + "edge found instead of 6 in " + horizontalShapeGraph)

      And("there should be an edge 9->6")
      val edges9to6 = horizontalShapeGraph.get(s9).outgoing.find(edge => edge.to == s6)
      assert(edges9to6.isDefined, "No edge from segment 9 -> 6 found")

      And("there should be an edge 10->6")
      val edges10to6 = horizontalShapeGraph.get(s10).outgoing.find(edge => edge.to == s6)
      assert(edges10to6.isDefined, "No edge from segment 10 -> 6 found")

      And("there should be an edge 9->8")
      val edges9to8 = horizontalShapeGraph.get(s9).outgoing.find(edge => edge.to == s8)
      assert(edges9to8.isDefined, "No edge from segment 9 -> 8 found")

      And("there should be an edge 10->7")
      val edges10to7 = horizontalShapeGraph.get(s10).outgoing.find(edge => edge.to == s7)
      assert(edges10to7.isDefined, "No edge from segment 10 -> 7 found")

      And("there should be an edge 8->7")
      val edges8to7 = horizontalShapeGraph.get(s8).outgoing.find(edge => edge.to == s7)
      assert(edges8to7.isDefined, "No edge from segment 8 -> 7 found")

      And("there should be an edge 7->6")
      val edges7to6 = horizontalShapeGraph.get(s7).outgoing.find(edge => edge.to == s6)
      assert(edges7to6.isDefined, "No edge from segment 7 -> 6 found")

    }
  }

}
