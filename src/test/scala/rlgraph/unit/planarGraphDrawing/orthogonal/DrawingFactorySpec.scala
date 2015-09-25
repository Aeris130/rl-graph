package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.drawing.DrawingFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.{DefaultDart, SimpleDart}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper, VertexWrapperFactory}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DrawingFactorySpec extends SpecImports {
  private val drawFactory = new DrawingFactory()

  def simpleDarts = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper[String](2)
    val C = new VertexWrapper[String](3)
    val D = new VertexWrapper(4, "D")

    val originalDart = new DefaultDart[String](A, D, 1, 0) // Next and Bends doesn't matter

    val AtoB = new SimpleDart(A, B, 1, originalDart)
    val BtoC = new SimpleDart(B, C, 1, originalDart)
    val CtoD = new SimpleDart(C, D, 1, originalDart)

    val hSeg1 = new Segment(Horizontal, A, B).newCoordinate(4)
    val hSeg2 = new Segment(Horizontal, C, D).newCoordinate(2)

    val vSeg1 = new Segment(Vertical, A).newCoordinate(0)
    val vSeg2 = new Segment(Vertical, B, C).newCoordinate(3)
    val vSeg3 = new Segment(Vertical, D).newCoordinate(5)

    val segmentSet = Set(hSeg1, hSeg2, vSeg1, vSeg2, vSeg3)
    val dartSet: Set[Dart[String]] = Set(AtoB, BtoC, CtoD)

    val graphEdge = "A"~"D"
    val graph = Graph(graphEdge)
  }

  def singleDefaultDart = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val AtoB = new DefaultDart[String](A, B, 1, 0).newDirection(Right)

    val s1 = new Segment(Horizontal, A, B).newCoordinate(0)
    val s2 = new Segment(Vertical, A).newCoordinate(0)
    val s3 = new Segment(Vertical, B).newCoordinate(1)

    val segmentSet = Set(s1, s2, s3)
    val dartSet = Set(AtoB)

    val graphEdge = "A"~"B"
    val graph = Graph(graphEdge)
  }

  /**
   * An edge A to B represented by a default dart, and an edge B to D split in the middle using C.
   *
   * Dummy values on degrees, bends and coordinates.
   */
  def multipleDarts = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper[String](3) // Dummy
    val D = new VertexWrapper(4, "D")

    val AtoB = new DefaultDart[String](A, B, 1, 0)

    val BtoD = new DefaultDart[String](B, D, 1, 0)
    val v2_v3 = new SimpleDart(B, C, 1, BtoD)
    val v3_v4 = new SimpleDart(C, D, 1, BtoD)

    val hSeg1 = new Segment(Horizontal, A, B).newCoordinate(0)
    val hSeg2 = new Segment(Horizontal, C, D).newCoordinate(0)

    val vSeg1 = new Segment(Vertical, A).newCoordinate(0)
    val vSeg2 = new Segment(Vertical, B, C).newCoordinate(0)
    val vSeg3 = new Segment(Vertical, D).newCoordinate(0)

    val segmentSet = Set(hSeg1, hSeg2, vSeg1, vSeg2, vSeg3)
    val dartSet = Set(AtoB, v2_v3, v3_v4)

    val edge1 = "A"~"B"
    val edge2 = "B"~"D"
    val graph = Graph(edge1, edge2)
  }

  describe("DrawingFactory") {

    it ("should parse a single default dart into a line with no bends between its vertex coordinates") {

      Given("a single default dart with coordinates (1, 2) and (3, 2)")
      val vertexFactory = new VertexWrapperFactory[String]()
      val dart = new DefaultDart("A", "B", 1, 0, vertexFactory)
      val hSeg = new Segment(Horizontal, dart.from, dart.to).newCoordinate(2)
      val vSeg1 = new Segment(Vertical, dart.from).newCoordinate(1)
      val vSeg2 = new Segment(Vertical, dart.to).newCoordinate(3)

      When("parsing the orthogonal drawing")
      val graph = Graph("A"~"B")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, Set(hSeg, vSeg1, vSeg2), Set(dart))

      Then("there should be a line from A to B")
      val line = drawing.edges.find(edge => edge.start == "A" && edge.stop == "B")
      assert(line.isDefined, "No line from A to B found.")

      And("only the vertex coordinates should be on it")
      line.get.startPos should be ((1, 2))
      line.get.stopPos should be ((3, 2))

    }

    it ("should parse multiple simple darts with the same default original into a line with bends") {

      Given("three simple darts with coordinates (0, 4)->(3, 4), (3, 4)->(3, 2) and (3, 2)->(5, 2)")
      val f = simpleDarts
      import f._

      When("parsing the orthogonal drawing")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, Set(hSeg1, hSeg2, vSeg1, vSeg2, vSeg3), Set(AtoB, BtoC, CtoD))

      Then("there should be a line from A to D")
      drawing.edges.size should be (1)
      val edge = drawing.edges.find(edge => edge.start == "A" && edge.stop == "D")
      assert(edge.isDefined, "No line from A to B found.")

      And("the line coordinates should be (0, 4)->(3, 4)->(3, 2)->(5, 2)")
      edge.get.startPos should be ((0, 4))
      edge.get.stopPos should be ((5, 2))

      edge.get.bends should be (Vector((3, 4), (3, 2)))

    }

    it ("should throw an exception if attempting to parse a simple line ending in an undefined vertex") {

      Given("three simple darts ending in a dummy vertex")
      val A = new VertexWrapper(1, "A")
      val B = new VertexWrapper(2, "B")
      val C = new VertexWrapper(3, "C")
      val D = new VertexWrapper[String](4) // Dummy!

      val originalDart = new DefaultDart[String](A, C, 1, 0) // D isn't used as end since defaults can't use dummy values

      val v1_v2 = new SimpleDart(A, B, 1, originalDart)
      val v2_v3 = new SimpleDart(B, C, 1, originalDart)
      val v3_v4 = new SimpleDart(C, D, 1, originalDart)

      val hSeg1 = new Segment(Horizontal, A, B).newCoordinate(4)
      val hSeg2 = new Segment(Horizontal, C, D).newCoordinate(2)

      val vSeg1 = new Segment(Vertical, A).newCoordinate(0)
      val vSeg2 = new Segment(Vertical, B, C).newCoordinate(3)
      val vSeg3 = new Segment(Vertical, D).newCoordinate(5)

      val graph = Graph("A"~"C")

      When("parsing the orthogonal drawing")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        drawFactory.draw[String, UnDiEdge](graph, Set(hSeg1, hSeg2, vSeg1, vSeg2, vSeg3), Set(v1_v2, v2_v3, v3_v4))
      }

    }

    /* Since drawings are only made from darts pointing up/right, sometimes simple darts from both opposites
     * of a default darts is needed to reconstruct the chain.
     */
    it ("should reconstruct simplified edges when darts don't follow a perfect chain") {

      Given("an edge A->D where two simple darts remain from the dart A->D, and one from D->A")
      val f = simpleDarts
      import f._
      val oppositeOriginalDart = new DefaultDart[String](D, A, 1, 0)
      val CtoB = new SimpleDart(C, B, 1, oppositeOriginalDart)

      When("parsing the orthogonal drawing without BtoC and instead uses CtoB")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, Set(hSeg1, hSeg2, vSeg1, vSeg2, vSeg3), Set(AtoB, CtoB, CtoD))

      Then("there should be a line from A to D")
      drawing.edges.size should be (1)
      val edge = drawing.edges.find(edge => edge.start == "A" && edge.stop == "D")
      assert(edge.isDefined, "No line from A to B found.")

      And("the line coordinates should be (0, 4)->(3, 4)->(3, 2)->(5, 2)")
      edge.get.startPos should be ((0, 4))
      edge.get.stopPos should be ((5, 2))

      edge.get.bends should be (Vector((3, 4), (3, 2)))

    }

    it ("should increase coordinate values to make all coordinates positive") {

      Given("three vertices with negative coordinates")
      val A = new VertexWrapper(1, "A")
      val B = new VertexWrapper(2, "B")
      val C = new VertexWrapper(3, "C")
      val AtoB = new DefaultDart[String](A, B, 1, 0).newDirection(Right)
      val AtoC = new DefaultDart[String](A, C, 1, 0).newDirection(Up)
      val s1 = new Segment(Horizontal, A, B).newCoordinate(-1)
      val s2 = new Segment(Horizontal, C).newCoordinate(-2)
      val s3 = new Segment(Vertical, A, C).newCoordinate(-3)
      val s4 = new Segment(Vertical, B).newCoordinate(-4)

      When("parsing the orthogonal drawing")
      val graph = Graph("A"~"B", "A"~"C")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, Set(s1, s2, s3, s4), Set(AtoB, AtoC))

      Then("A -> B should be (1, _) -> (0, _)")
      val horizontalEdge = drawing.edges.find(e => e.start == A.value.get && e.stop == B.value.get).get
      horizontalEdge.startPos._1 should be (1)
      horizontalEdge.stopPos._1 should be (0)

      And("A -> C should be (_, 1) -> (_, 0)")
      val verticalEdge = drawing.edges.find(e => e.start == A.value.get && e.stop == C.value.get).get
      verticalEdge.startPos._2 should be (1)
      verticalEdge.stopPos._2 should be (0)

      And("the vertex coordinates should be updated with the same values as the edges")
      val A_x = drawing.vertices.find(v => v._1 == "A").get._2
      val A_y = drawing.vertices.find(v => v._1 == "A").get._3
      A_x should be (1)
      A_y should be (1)

      val B_x = drawing.vertices.find(v => v._1 == "B").get._2
      val B_y = drawing.vertices.find(v => v._1 == "B").get._3
      B_x should be (0)
      B_y should be (1)

      val C_x = drawing.vertices.find(v => v._1 == "C").get._2
      val C_y = drawing.vertices.find(v => v._1 == "C").get._3
      C_x should be (1)
      C_y should be (0)

    }

    it ("should not increase coordinate values that are already positive when a negative coordinate is found on only one axis") {

      Given("a vertex A with a negative coordinate -1 on the x axis and a positive coordinate 4 on the y axis")
      val A = new VertexWrapper(1, "A")
      val B = new VertexWrapper(2, "B")
      val AtoB = new DefaultDart[String](A, B, 1, 0).newDirection(Right)
      val s1 = new Segment(Horizontal, A, B).newCoordinate(4)
      val s2 = new Segment(Vertical, A).newCoordinate(-1)
      val s3 = new Segment(Vertical, B).newCoordinate(2)

      When("parsing the orthogonal drawing")
      val graph = Graph("A"~"B")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, Set(s1, s2, s3), Set(AtoB))

      Then("the edge A~B should be adjusted by 1 on the x axis")
      val AtoBEdge = drawing.edges.find(e => e.start == A.value.get && e.stop == B.value.get).get
      AtoBEdge.startPos._1 should be (-1 + 1)
      AtoBEdge.stopPos._1 should be (2 + 1)

      And("the edge A~B should not have its y axis adjusted")
      AtoBEdge.startPos._2 should be (4)
      AtoBEdge.stopPos._2 should be (4)

      And("the vertex A should have its x coordinate adjusted by 1 and its y coordinate adjusted by 0")
      val A_x = drawing.vertices.find(v => v._1 == "A").get._2
      val A_y = drawing.vertices.find(v => v._1 == "A").get._3
      A_x should be (0)
      A_y should be (4)

      And("the vertex B should have its x coordinate adjusted by 1 and its y coordinate adjusted by 0")
      val B_x = drawing.vertices.find(v => v._1 == "B").get._2
      val B_y = drawing.vertices.find(v => v._1 == "B").get._3
      B_x should be (3)
      B_y should be (4)

    }

    it ("should map each vertex to its coordinate") {

      Given("a dart from A to B")
      val f = singleDefaultDart
      import f._

      When("parsing the orthogonal drawing")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, segmentSet, dartSet)

      Then("vertex A should be mapped to coordinates (0, 0")
      drawing.vertices should contain (("A", 0, 0))

      And("vertex B should be mapped to coordinates (1, 0")
      drawing.vertices should contain (("B", 1, 0))

    }

    it ("should add an edge represented by a default dart to the drawn edge") {

      Given("an edge A ~ B represented by a dart A ~ B")
      val f = singleDefaultDart
      import f._

      When("parsing the orthogonal drawing")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, segmentSet, dartSet)

      Then("the drawn edge containing A and B should be based on the undirected edge A~B")
      drawing.edges.size should be (1)
      drawing.edges.head.originalEdge should be (graphEdge)

    }

    it ("should add an edge represented by an opposite default dart to the drawn edge") {

      Given("an edge B ~ A represented by a dart A ~ B")
      val f = singleDefaultDart
      import f._
      val edge = "B"~"A"

      When("parsing the orthogonal drawing")
      val graph = Graph(edge)
      val drawing = drawFactory.draw[String, UnDiEdge](graph, segmentSet, dartSet)

      Then("the drawn edge containing A and B should be based on the undirected edge B~A")
      drawing.edges.size should be (1)
      drawing.edges.head.originalEdge should be (edge)

    }

    it ("should add an edge represented by a simple dart to the drawn edge") {

      Given("an edge A ~ D represented by 3 simple darts")
      val f = simpleDarts
      import f._

      When("parsing the orthogonal drawing")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, segmentSet, dartSet)

      Then("the drawn edge from A to D should have the undirected edge A~D as its original edge")
      drawing.edges.size should be (1)
      drawing.edges.head.originalEdge should be (graphEdge)

    }

    it ("should add an edge represented by an opposite simple dart to the drawn edge") {

      Given("an edge D ~ A represented by 3 simple darts from A to D")
      val f = simpleDarts
      import f._

      When("parsing the orthogonal drawing")
      val edge = "D"~"A"
      val graphWithOppositeEdge = Graph(edge)
      val drawing = drawFactory.draw[String, UnDiEdge](graphWithOppositeEdge, segmentSet, dartSet)

      Then("the drawn edge from A to D should have the undirected edge A~D as its original edge")
      drawing.edges.size should be (1)
      drawing.edges.head.originalEdge should be (edge)

    }

    it ("should add the edge represented by one or more darts to the drawn edge when drawing multiple edges") {

      Given("an edge A~B represented by a default dart and an edge B~D represented by two simple darts (B~C, C~D)")
      val f = multipleDarts
      import f._

      When("parsing the orthogonal drawing")
      val drawing = drawFactory.draw[String, UnDiEdge](graph, segmentSet, dartSet)

      Then("the drawn edge from A to B should have the undirected edge A ~ B as its original edge")
      drawing.edges.find(e => e.start == "A" && e.stop == "B").get.originalEdge should be (edge1)

      And("the drawn edge from B to D should have the undirected edge B ~ D as its original edge")
      drawing.edges.find(e => e.start == "B" && e.stop == "D").get.originalEdge should be (edge2)

    }

  }
}
