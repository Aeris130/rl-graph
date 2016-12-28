package rlgraph.integration.drawings.planar.annealing

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.costFunctions.{BorderLinesScore, DistributionScore, EdgeLengthScore}
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.{DefaultState, PreProcessingAlgorithm, Settings}
import rlgraph.SpecImports

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class PreProcessingAlgorithmSpec extends SpecImports {
  private def algorithm = new PreProcessingAlgorithm()
  private def random = new Random(666)

  private val defaultState = new DefaultState()
  private val costFunctions = Vector(new EdgeLengthScore(1, 0), new BorderLinesScore(1), new DistributionScore(1, 20))
  private val defaultSettings = Settings.default(defaultState, costFunctions)

  describe("PreProcessingAlgorithmSpec") {

    it ("should process an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

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
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

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
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

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
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

      Then("a valid drawing should be produced")
      validate(drawing)

      And("the drawing should not contain edges")
      drawing.edges should be ('empty)

    }

    it ("should be deterministic") {

      Given("a graph with vertices")
      val graph = Graph[Int, UnDiEdge](0~1, 1~2, 1~3, 3~4, 5~6, 7, 8)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

      Then("subsequent processes should yield the same drawing")
      for (i <- 0 to 100) {
        val newDrawing = algorithm.computeDrawing(graph, random, defaultSettings)
        validate(newDrawing)
        newDrawing should equal (drawing)
      }

    }

    it ("should run with any vertex set") {

      Given("a graph with vertices 3, 5, 22")
      val graph = Graph[Int, UnDiEdge](3~5, 22)

      When("processing the graph")
      val drawing = algorithm.computeDrawing(graph, random, defaultSettings)

      Then("the drawing should assign the original vertices its coordinates")
      drawing.coordinates.keySet should contain(3)
      drawing.coordinates.keySet should contain(5)
      drawing.coordinates.keySet should contain(22)

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
