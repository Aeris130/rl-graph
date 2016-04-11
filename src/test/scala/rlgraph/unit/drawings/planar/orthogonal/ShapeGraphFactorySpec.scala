package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.ShapeGraphFactory
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{Dart, VertexWrapper}
import rlgraph.SpecImports

import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

class ShapeGraphFactorySpec extends SpecImports {
  val shapeFactory = new ShapeGraphFactory()

  def exampleSegments = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val v4 = new VertexWrapper(4, "v4")
    val v5 = new VertexWrapper(5, "v5")
    val v6 = new VertexWrapper(6, "v6")
    val v7 = new VertexWrapper(7, "v7")

    // Horizontal segments
    val S3 = new Segment(Vector(v3, v5), Horizontal)
    val S2 = new Segment(Vector(v2, v4, v7), Horizontal)
    val S1 = new Segment(Vector(v1), Horizontal)
    val S4 = new Segment(Vector(v6), Horizontal)

    // Vertical darts
    val v1_v2 = new DefaultDart[String](v1, v2, 1, 0, Option(Up))
    val v2_v3 = new DefaultDart[String](v2, v3, 1, 0, Option(Up))
    val v4_v5 = new DefaultDart[String](v4, v5, 3, 0, Option(Up))
    val v6_v7 = new DefaultDart[String](v6, v7, 1, 0, Option(Up))

    // Vertical segments
    val S5 = new Segment(Vector(v1, v2, v3), Vertical)
    val S6 = new Segment(Vector(v4, v5), Vertical)
    val S7 = new Segment(Vector(v6, v7), Vertical)

    // Horizontal darts
    val v3_v5 = new DefaultDart[String](v3, v5, 1, 0, Option(Right))
    val v2_v4 = new DefaultDart[String](v2, v4, 2, 0, Option(Right))
    val v4_v7 = new DefaultDart[String](v4, v7, 1, 0, Option(Right))

    val dartSet = Set[Dart[String]](v1_v2, v2_v3, v4_v5, v6_v7, v3_v5, v2_v4, v4_v7)
    val segments = Set(S1, S2, S3, S4, S5, S6, S7)
  }

  describe("ShapeGraphFactory") {

    it ("should insert one edge for each dart") {

      Given("7 darts")
      val f = exampleSegments
      import f._

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(dartSet, segments)
      val horizontal: Graph[Segment[String], WLkDiEdge] = shapeGraphs._1
      val vertical: Graph[Segment[String], WLkDiEdge] = shapeGraphs._2

      Then("the combined amount of edges should be 7")
      (horizontal.edges.size + vertical.edges.size) should be (7)

    }

    it("it should insert arcs between horizontal segments connected by a dart, using one edge for each dart") {

      Given("4 segments (1, 2, 3, 4) with darts going from 1->2, 2->3 (two of them), 4->2")
      val f = exampleSegments
      import f._

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(dartSet, segments)
      val horizontal: Graph[Segment[String], WLkDiEdge] = shapeGraphs._1

      Then("segment 2 should have 2 outgoing edges to segment 3")
      val outgoingFrom2 = horizontal.get(S2).outgoing.toVector
      outgoingFrom2.size should be (2)

      outgoingFrom2(0).to should be (S3)
      outgoingFrom2(1).to should be (S3)

      And("segment 1 should have 1 outgoing edges to segment 2")
      val outgoingFrom1 = horizontal.get(S1).outgoing.toVector
      outgoingFrom1.size should be (1)

      outgoingFrom1(0).to should be (S2)

      And("segment 4 should have 1 outgoing edges to segment 2")
      val outgoingFrom4 = horizontal.get(S4).outgoing.toVector
      outgoingFrom4.size should be (1)

      outgoingFrom4(0).to should be (S2)

    }

    it ("it should insert arcs between vertical segments connected by a dart, using one edge for each dart") {

      Given("3 segments (5, 6, 7) with darts going from 5->6 (two of them) and 6->7")
      val f = exampleSegments
      import f._

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(dartSet, segments)
      val vertical: Graph[Segment[String], WLkDiEdge] = shapeGraphs._2

      Then("segment 5 should have two edges to segment 6")
      val outgoingFrom5 = vertical.get(S5).outgoing.toVector
      outgoingFrom5.size should be (2)

      outgoingFrom5(0).to should be (S6)
      outgoingFrom5(1).to should be (S6)

      And("segment 6 should have one outgoing edge to segment 7")
      val outgoingFrom6 = vertical.get(S6).outgoing.toVector
      outgoingFrom6.size should be (1)

      outgoingFrom6(0).to should be (S7)

    }

    it ("should ignore darts pointing in the wrong direction") {

      Given("a dart-set with a left-pointing dart from segment 6 to 5 and a right-pointing dart to segment 7")
      val f = exampleSegments
      import f._
      val leftDart = new DefaultDart[String](v6, v5, 1, 0, Option(Left))

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(dartSet + leftDart, segments)
      val vertical: Graph[Segment[String], WLkDiEdge] = shapeGraphs._2

      Then("segment 6 should only contain 1 edge, and it should be to segment 7 (not 5)")
      val outgoingFrom6 = vertical.get(S6).outgoing.toVector
      outgoingFrom6.size should be (1)

      outgoingFrom6(0).to should be (S7)

    }

    it ("should set the weight of all edges to 1") {

      Given("4 vertical and 3 horizontal darts")
      val f = exampleSegments
      import f._

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(dartSet, segments)
      val horizontal: Graph[Segment[String], WLkDiEdge] = shapeGraphs._1
      val vertical: Graph[Segment[String], WLkDiEdge] = shapeGraphs._2

      Then("all 4 edges in the horizontal graph should have weight 1")
      val verticalEdges = horizontal.edges.toVector
      verticalEdges(0).weight should be (1)
      verticalEdges(1).weight should be (1)
      verticalEdges(2).weight should be (1)
      verticalEdges(3).weight should be (1)

      Then("all 3 edges in the vertical graph should have weight 1")
      val horizontalEdges = vertical.edges.toVector
      horizontalEdges(0).weight should be (1)
      horizontalEdges(1).weight should be (1)
      horizontalEdges(2).weight should be (1)

    }

    it ("should create a graph from segments that only orient in a single direction") {

      Given("a horizontal dart")
      val f = exampleSegments
      import f._
      val dart: Dart[String] = v3_v5
      val segment: Segment[String] = S3

      When("computing shape graphs")
      val shapeGraphs = shapeFactory.makeShapeGraphs(Set[Dart[String]](dart), Set[Segment[String]](segment))
      val horizontal: Graph[Segment[String], WLkDiEdge] = shapeGraphs._1
      val vertical: Graph[Segment[String], WLkDiEdge] = shapeGraphs._2

      Then("the horizontal graph should be empty")
      assert(horizontal.isEmpty, "The horizontal graph " + horizontal + " was not empty despite not being based on any darts")

    }

  }
}
