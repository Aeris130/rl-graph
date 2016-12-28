package rlgraph.unit.drawings.planar.annealing

import net.cyndeline.rlcommon.math.geom._
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.common.VertexAdjustment
import rlgraph.SpecImports
import rlgraph.help.ProduceDrawing

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class VertexAdjustmentSpec extends SpecImports {
  private val dummyBorder = 999

  /* A 3x3 rectangle within a border from (0,0) to (5,5). */
  private def multiPointInsideBorder = new {
    val v = (1, Point(2, 3))
    val graph = Graph[Int, UnDiEdge](v._1)
    val coordinates = Map(v)
    val dimensions = Map(v._1 -> Dimensions(3, 3))
    val border = Rectangle(Point(0, 0), Point(5, 5))
    val drawing = ProduceDrawing(coordinates, graph)
    val adjust = VertexAdjustment(drawing, graph, 6, dimensions)
  }

  /* Two vertices at (2,3) and (7,7) with user-specified dimensions. Connected to a dummy at (5,1). */
  private def dualVerticesWithDummy(v1Dim: Dimensions, v2Dim: Dimensions) = new {
    val v1 = (1, Point(2, 3))
    val v2 = (2, Point(7, 7))
    val dummy = (3, Point(5, 1))
    val graph = Graph[Int, UnDiEdge](v1._1~dummy._1, v2._1~dummy._1)
    val coordinates = Map(v1, v2, dummy)
    val dimensions = Map(v1._1 -> v1Dim, v2._1 -> v2Dim)
    val drawing = ProduceDrawing(coordinates, graph)
    val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)
  }

  /* Two vertices at (2,3) and (7,7) with user-specified dimensions. */
  private def dualVertices(v1Dim: Dimensions, v2Dim: Dimensions) = new {
    val v1 = (1, Point(2, 3))
    val v2 = (2, Point(7, 7))
    val graph = Graph[Int, UnDiEdge](v1._1~v2._1)
    val coordinates = Map(v1, v2)
    val dimensions = Map(v1._1 -> v1Dim, v2._1 -> v2Dim)
    val drawing = ProduceDrawing(coordinates, graph)
    val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)
  }

  /* Three connected vertices with no borders. */
  private def tripleSingle = new {
    val v = (1, Point(2, 7))
    val n = (2, Point(2, 2))
    val m = (3, Point(6, 2))
    val graph = Graph(v._1~n._1, n._1~m._1)
    val coordinates = Map(v, n, m)
    val drawing = ProduceDrawing(coordinates, graph)
    val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())
  }

  // A single vertex
  private def single = new {
    val v = (1, Point(2, 3))
    val graph = Graph[Int, UnDiEdge](v._1)
    val coordinates = Map(v)
    val drawing = ProduceDrawing(coordinates, graph)
    val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())
  }

  describe("VertexAdjustment") {

    it ("should move a single vertex") {

      Given("a single vertex")
      val f = single
      import f._

      When("moving the vertex to a new position")
      val np = Point(67, 23)
      val newPos = adjust.moveVertex(v._1, np)

      Then("the vertex should have its coordinate updated")
      newPos.get.coordinate(v._1) should be (np)

    }

    it ("should not move a vertex onto its own coordinate") {

      Given("a single vertex")
      val f = single
      import f._

      When("moving the vertex onto its own coordinate")
      val newPos = adjust.moveVertex(v._1, v._2)

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should move a vertex connected to a single neighbor") {

      Given("two neighboring vertices")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(5, 4))
      val graph = Graph(v1._1~v2._1)
      val coordinates = Map(v1, v2)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v1 to a new position")
      val np = Point(67, 23)
      val newPos = adjust.moveVertex(v1._1, np)

      Then("the vertex should have its coordinate updated")
      newPos.get.coordinate(v1._1) should be (np)

      And("the other vertex should remain as is")
      newPos.get.coordinate(v2._1) should be (v2._2)

    }

    it ("should move a vertex connected to multiple neighbors") {

      Given("a vertex v and three neighbors")
      val v = (1, Point(2, 3))
      val n1 = (2, Point(5, 4))
      val n2 = (3, Point(3, 9))
      val n3 = (4, Point(9, 10))
      val graph = Graph(v._1~n1._1, v._1~n2._1, v._1~n3._1)
      val coordinates = Map(v, n1, n2, n3)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v to a new position")
      val np = Point(4, 7)
      val newPos = adjust.moveVertex(v._1, np)

      Then("the vertex should have its coordinate updated")
      newPos.get.coordinate(v._1) should be (np)

      And("the other vertices should remain as is")
      newPos.get.coordinate(n1._1) should be (n1._2)
      newPos.get.coordinate(n2._1) should be (n2._2)

    }

    it ("should move a vertex when non-related vertices are present") {

      Given("a pair of vertices v1,v2 and another pair n1,n2")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(8, 3))
      val n1 = (3, Point(3, 5))
      val n2 = (4, Point(11, 9))
      val graph = Graph(v1._1~v2._1, n1._1~n2._1)
      val coordinates = Map(v1, v2, n1, n2)
      val dimensions = Map(n1._1 -> Dimensions(3, 3), n2._1 -> Dimensions(3, 3))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("moving v2 to a new position")
      val np = Point(10, 5)
      val newPos = adjust.moveVertex(v2._1, np)

      Then("the vertex should have its coordinate updated")
      newPos.get.coordinate(v2._1) should be (np)

    }

    it ("should reject a change that overlaps two single-point neighbors") {

      Given("two single-point vertices")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(5, 4))
      val graph = Graph[Int, UnDiEdge](v1._1~v2._1)
      val coordinates = Map(v1, v2)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v1 onto v2")
      val newPos = adjust.moveVertex(v1._1, v2._2)

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps two single-point vertices") {

      Given("two single-point vertices")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(5, 4))
      val neighbor1 = (3, Point(3, 5))
      val neighbor2 = (4, Point(4, 5))
      val graph = Graph[Int, UnDiEdge](v1._1~neighbor1._1, v2._1~neighbor2._1, neighbor1._1~neighbor2._1)
      val coordinates = Map(v1, v2, neighbor1, neighbor2)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v1 onto v2")
      val newPos = adjust.moveVertex(v1._1, v2._2)

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps two multi-point vertices") {

      Given("two multi-point vertices")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(7, 7))
      val graph = Graph[Int, UnDiEdge](v1._1~v2._1)
      val coordinates = Map(v1, v2)
      val dimensions = Map(v1._1 -> Dimensions(3, 3), v2._1 -> Dimensions(3, 3))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("moving v2 onto v1")
      val newPos = adjust.moveVertex(v2._1, Point(3, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that moves a vertex into the rectangle of its only other neighbor") {

      Given("a vertex v1 with a 5x5 rectangle, and a neighbor v2")
      val f = dualVertices(Dimensions(5, 5), Dimensions(1, 1))
      import f._

      When("moving v2 into the rectangle of v1")
      val newPos = adjust.moveVertex(v2._1, Point(3, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that moves a vertex onto the edge of the rectangle of its only other neighbor") {

      Given("a vertex v1 with a 5x5 rectangle, and a neighbor v2")
      val f = dualVertices(Dimensions(3, 3), Dimensions(1, 1))
      import f._

      When("moving v2 into the rectangles edge of v1")
      val newPos = adjust.moveVertex(v2._1, Point(3, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that makes two multi-point vertices share an edge") {

      Given("two multi-point vertices")
      val f = dualVerticesWithDummy(Dimensions(3, 3), Dimensions(3, 3))
      import f._

      When("moving them next to each other")
      val newPos = adjust.moveVertex(v2._1, Point(4, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that moves a single-point vertex to share an edge with a multi-point vertex") {

      Given("a multi-point and a single-point vertex")
      val f = dualVerticesWithDummy(Dimensions(3, 3), Dimensions(1, 1))
      import f._

      When("moving the single-point onto the border of the multi-point")
      val newPos = adjust.moveVertex(v2._1, Point(3, 3))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that moves a multi-point vertex to share an edge with a single-point vertex") {

      Given("a single-point vertex v1 and a multi-point vertex v2")
      val f = dualVerticesWithDummy(Dimensions(1, 1), Dimensions(3, 3))
      import f._

      When("moving v2 so that its border intersects v1")
      val newPos = adjust.moveVertex(v2._1, Point(2, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps a single-point vertex with an edge connected to a neighbor") {

      Given("a vertex v with a neighbor n, having a neighbor m")
      val f = tripleSingle
      import f._

      When("moving v between n and m")
      val newPos = adjust.moveVertex(v._1, Point(4, 2))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps a neighbors edge with the edge between v and another neighbor") {

      Given("a vertex v with a neighbor n, having a neighbor m")
      val f = tripleSingle
      import f._

      When("moving v beyond n and m such that the edge v-n overlaps n-m")
      val newPos = adjust.moveVertex(v._1, Point(7, 2))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that makes 3+ neighbors collinear with v") {

      Given("five collinear vertices, and a vertex v connecting to all of them")
      val n1 = (1, Point(1, 5))
      val n2 = (2, Point(2, 5))
      val n3 = (3, Point(4, 5))
      val n4 = (4, Point(5, 5))
      val n5 = (5, Point(6, 5))
      val v = (6, Point(2, 2))
      val graph = Graph[Int, UnDiEdge](v._1~n1._1, v._1~n2._1, v._1~n3._1, v._1~n4._1, v._1~n5._1)
      val coordinates = Map(v, n1, n2, n3, n4, n5)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v in between 2 and 3")
      val newPos = adjust.moveVertex(v._1, Point(3, 5))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps a single-point vertex with an edge not connected to a neighbor") {

      Given("a vertex v with a neighbor n, and a disjoint pair of vertices m1 and m2")
      val v = (1, Point(2, 7))
      val n = (2, Point(2, 2))
      val m1 = (3, Point(4, 2))
      val m2 = (4, Point(4, 7))
      val graph = Graph(v._1~n._1, m1._1~m2._1)
      val coordinates = Map(v, n, m1, m2)
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, Map())

      When("moving v onto the edge between m1 and m2")
      val newPos = adjust.moveVertex(v._1, Point(4, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that overlaps a multi-point vertex with an edge connected to a neighbor") {

      Given("a vertex v with a neighbor n, having a neighbor m")
      val v = (1, Point(2, 6))
      val n = (2, Point(2, 2))
      val m = (3, Point(8, 2))
      val graph = Graph(v._1~n._1, n._1~m._1)
      val coordinates = Map(v, n, m)
      val dimensions = Map(v._1 -> Dimensions(3, 3), n._1 -> Dimensions(3, 5), m._1 -> Dimensions(3, 5))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("moving v so that its rectangle overlaps the edge between n and m")
      val newPos = adjust.moveVertex(v._1, Point(5, 3))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that intersects a multi-point vertex with an edge connected to a neighbor") {

      Given("a vertex v with a neighbor n, having a neighbor m")
      val v = (1, Point(2, 6))
      val n = (2, Point(2, 2))
      val m = (3, Point(8, 2))
      val graph = Graph(v._1~n._1, n._1~m._1)
      val coordinates = Map(v, n, m)
      val dimensions = Map(v._1 -> Dimensions(3, 3), n._1 -> Dimensions(3, 5), m._1 -> Dimensions(3, 5))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("moving v so that its rectangle intersects the edge between n and m")
      val newPos = adjust.moveVertex(v._1, Point(5, 2))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that intersects two multi-point vertices with each other") {

      Given("a vertex v with a neighbor n, having a neighbor m")
      val v = (1, Point(2, 6))
      val n = (2, Point(2, 2))
      val m = (3, Point(8, 2))
      val graph = Graph(v._1~n._1, n._1~m._1)
      val coordinates = Map(v, n, m)
      val dimensions = Map(v._1 -> Dimensions(3, 3), n._1 -> Dimensions(3, 5), m._1 -> Dimensions(3, 5))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("moving n so that it intersects v")
      val newPos = adjust.moveVertex(n._1, Point(2, 4))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should handle multiple adjustments") {

      Given("two vertices")
      val v1 = (1, Point(2, 3))
      val v2 = (2, Point(7, 7))
      val graph = Graph[Int, UnDiEdge](v1._1~v2._1)
      val coordinates = Map(v1, v2)
      val dimensions = Map(v1._1 -> Dimensions(3, 3), v2._1 -> Dimensions(3, 3))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, dummyBorder, dimensions)

      When("switching their places")
      val newPos = adjust.moveVertex(1, Point(99, 99)).get
        .moveVertex(2, v1._2).get
        .moveVertex(1, v2._2).get

      Then("the vertices should switch places")
      newPos.coordinate(v1._1) should be (v2._2)
      newPos.coordinate(v2._1) should be (v1._2)

    }

    /*
     *
     * Border tests.
     *
     */

    it ("should allow a single-point vertex to move onto the border") {

      Given("a border (0,0) to (5,5) and a vertex inside it")
      val v = (1, Point(2, 3))
      val graph = Graph[Int, UnDiEdge](v._1)
      val coordinates = Map(v)
      val border = Rectangle(Point(0, 0), Point(5, 5))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, border.width.toInt, Map())

      When("moving the vertex to the border")
      val lower = adjust.moveVertex(v._1, border.start)
      val upper = adjust.moveVertex(v._1, border.stop)
      val middle = adjust.moveVertex(v._1, Point(0, 2))

      Then("the vertex should have its coordinate updated accordingly")
      lower.get.coordinate(v._1) should be (border.start)
      upper.get.coordinate(v._1) should be (border.stop)
      middle.get.coordinate(v._1) should be (Point(0, 2))

    }

    it ("should reject a single-point vertex moving outside the border") {

      Given("a border (0,0) to (5,5) and a vertex inside it")
      val v = (1, Point(2, 3))
      val graph = Graph[Int, UnDiEdge](v._1)
      val coordinates = Map(v)
      val border = Rectangle(Point(0, 0), Point(5, 5))
      val drawing = ProduceDrawing(coordinates, graph)
      val adjust = VertexAdjustment(drawing, graph, border.width.toInt, Map())

      When("moving the vertex outside the border")
      val lower = adjust.moveVertex(v._1, border.start - 1)
      val upper = adjust.moveVertex(v._1, border.stop + 1)

      Then("no movement should be allowed")
      lower should be (None)
      upper should be (None)

    }

    it ("should allow a multi-point vertex to share an edge with the border") {

      Given("a border (0,0) to (5,5) and a multi-point vertex inside it")
      val f = multiPointInsideBorder
      import f._

      When("moving the vertex to the border")
      val lower = adjust.moveVertex(v._1, Point(4, 4))
      val upper = adjust.moveVertex(v._1, Point(4, 3))
      val middle = adjust.moveVertex(v._1, Point(4, 1))

      Then("the vertex should have its coordinate updated accordingly")
      lower.get.coordinate(v._1) should be (Point(4, 4))
      upper.get.coordinate(v._1) should be (Point(4, 3))
      middle.get.coordinate(v._1) should be (Point(4, 1))

    }

    it ("should reject a change that makes a multi-point vertex intersect inside the border") {

      Given("a border (0,0) to (5,5) and a multi-point vertex inside it")
      val f = multiPointInsideBorder
      import f._

      When("moving the vertex inside the border")
      val newPos = adjust.moveVertex(v._1, Point(5, 5))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should reject a change that places a multi-point vertex outside the border") {

      Given("a border (0,0) to (5,5) and a multi-point vertex inside it")
      val f = multiPointInsideBorder
      import f._

      When("moving the vertex outside the border")
      val newPos = adjust.moveVertex(v._1, Point(2, -7))

      Then("no adjustment should be made")
      newPos should be (None)

    }

    it ("should throw an exception when attempting to build a factory with initial rectangles that lies outside the border") {

      Given("a drawing with a single vertex at (5,6)")
      val drawing = new StraightLineDrawing(Vector(1), Vector(), Map(1 -> Point(5, 6)), 5, 6)

      When("building a factory with a border from (0,0) to (4,4)")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        VertexAdjustment(drawing, Graph[Int, UnDiEdge](1), 5)
      }

    }

  }

}
