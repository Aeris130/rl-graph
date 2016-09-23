package rlgraph.integration.drawings.planar.annealing

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{PreProcessingAlgorithm, Settings}
import rlgraph.SpecImports

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class PreProcessingAlgorithmSpec extends SpecImports {
  private def algorithm = new PreProcessingAlgorithm()
  private def random = new Random(666)

  describe("PreProcessingAlgorithmSpec") {

    it ("should process an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.default)

      Then("an empty drawing should be returned")
      drawing.vertices should be ('empty)
      drawing.edges should be ('empty)
      drawing.coordinates should be ('empty)

    }

    it ("should process a graph with a single vertex") {

      // Also tests that coordinates are adjusted to 0

      Given("an graph with a vertex")
      val v = 0
      val graph = Graph[Int, UnDiEdge](v)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.default)

      Then("the drawing should contain the vertex")
      drawing.vertices should be (Vector(v))

      And("no edges should be present")
      drawing.edges should be ('empty)

      And("the vertex should have coordinate (0,0)")
      drawing.coordinates should be (Map(v -> Point(0, 0)))

    }

    it ("should process a graph with a single vertex having no neighbors") {

      Given("a graph with vertex 2 having no neighbor")
      val graph = Graph[Int, UnDiEdge](0~1, 2)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.default)

      Then("a valid drawing should be produced")
      validate(drawing)

      And("the drawing should contain the edge 0-1")
      drawing.edges should have size 1
      assert(drawing.edges.contains((0, 1)) || drawing.edges.contains((1, 0)))

    }

    it ("should process a graph with multiple vertices but no edges") {

      Given("a graph with multiple vertices")
      val graph = Graph[Int, UnDiEdge](0, 1, 2, 3, 4, 5)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.default)

      Then("a valid drawing should be produced")
      validate(drawing)

      And("the drawing should not contain edges")
      drawing.edges should be ('empty)

    }

    it ("should process a graph with vertex rectangles") {

      Given("a graph with vertices and rectangles")
      val graph = Graph[Int, UnDiEdge](0~1, 1~2, 1~3, 3~4, 5~6, 7, 8)
      val rectangles = Map[Int, Dimensions](
        0 -> Dimensions(2, 3),
        1 -> Dimensions(4, 4),
        4 -> Dimensions(6, 5),
        7 -> Dimensions(2, 2)
      )

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.defaultWithRectangles(rectangles))

      Then("a valid drawing should be produced")
      validate(drawing, rectangles)

    }

    it ("should be deterministic") {

      Given("a graph with vertices")
      val graph = Graph[Int, UnDiEdge](0~1, 1~2, 1~3, 3~4, 5~6, 7, 8)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, Settings.default)

      Then("subsequent processes should yield the same drawing")
      for (i <- 0 to 100) {
        val newDrawing = algorithm.computeDrawing(graph, random, Settings.default)
        validate(newDrawing)
        newDrawing should equal (drawing)
      }

    }

  }

  private def validate(drawing: StraightLineDrawing[Int], rectangles: Map[Int, Dimensions] = Map()): Unit = {
    assert(drawing.coordinates.values.size == drawing.vertices.size, "Overlapping coordinates found")

    def rectangle(v: Int) = {
      val rDimensions = rectangles.getOrElse(v, Dimensions(1, 1))
      Rectangle.centerAround(drawing.coordinates(v), rDimensions.width, rDimensions.height)
    }

    def rOverlap(v: Int) = {
      val vr = rectangle(v)
      assert(!drawing.vertices.exists(other => other != v && rectangle(other).intersection(vr).isDefined), "Overlapping rectangles found")
    }

    for (v <- drawing.vertices) {
      rOverlap(v)
    }
  }

}
