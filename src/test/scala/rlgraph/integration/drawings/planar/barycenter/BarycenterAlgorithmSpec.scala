package rlgraph.integration.drawings.planar.barycenter

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlgraph.drawings.planar.barycenter.BarycenterAlgorithm
import rlgraph.SpecImports
import rlgraph.help.StraightLineIntersections

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class BarycenterAlgorithmSpec extends SpecImports {
  private val algorithm = new BarycenterAlgorithm()

  describe("BarycenterAlgorithm") {

    it ("should compute a drawing from an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

      And("the drawing should be empty")
      assert(drawing.isEmpty)

    }

    it ("should compute a drawing from a graph with a single vertex") {

      Given("a graph with a vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

      And("no edges should be present")
      drawing.edges should be ('empty)

    }

    it ("should compute a drawing from a graph with two vertices") {

      Given("a graph with two vertices")
      val graph = Graph[Int, UnDiEdge](1~2)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

      And("a single edge between 1 and 2 should be present")
      drawing.edges should be (Vector((1, 2)))

      And("the coordinate should be (0,0) and (1,0)")
      drawing.coordinates.values.toSet should be (Set(Point(0, 0), Point(1, 0)))

    }

    it ("should compute a drawing from a graph with three vertices and edges") {

      Given("a triangle")
      val graph = Graph(1~2, 2~3, 3~1)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

    }

    it ("should compute a drawing from a graph with an inner edge") {

      Given("a triangular graph with an inner vertex")
      val graph = Graph(0~1, 1~2, 2~0, 3~0, 3~1, 3~2)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

    }

    it ("should compute a drawing from a non-triconnected graph") {

      Given("a graph where vertex 4 can be disconnected by removing 2-3 or 1-3")
      val graph = Graph(0~1, 1~2, 2~0, 1~3, 2~3, 3~4)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

    }

    it ("should compute a drawing from a graph with no edges") {

      Given("a graph with no edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      When("computing its drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be valid")
      StraightLineIntersections.validateDrawing(drawing, graph)

    }

  }

}
