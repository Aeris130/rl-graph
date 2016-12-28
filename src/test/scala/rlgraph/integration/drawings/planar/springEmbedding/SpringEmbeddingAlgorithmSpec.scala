package rlgraph.integration.drawings.planar.springEmbedding

import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.springEmbedding.SpringEmbeddingAlgorithm
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class SpringEmbeddingAlgorithmSpec extends SpecImports {
  private val algorithm = new SpringEmbeddingAlgorithm(2000)

  describe("SpringEmbeddingAlgorithm") {

    it ("should compute a drawing for an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("computing the drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("an empty drawing should be produced")
      drawing.isEmpty should be (true)

    }

    it ("should compute a drawing for a graph with no edges") {

      Given("a graph without edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3)

      When("computing the drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("a valid drawing should be produced")
      drawingIsValid(drawing)

    }

    it ("should compute a drawing for a graph with multiple faces") {

      Given("a graph where every vertex cannot lie on the same face")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~5, 5~1, 1~6, 6~4)

      When("computing the drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("a valid drawing should be produced")
      drawingIsValid(drawing)

    }

  }

  private def drawingIsValid(d: StraightLineDrawing[Int]): Unit = {
    assert(d.coordinates.values.toSet.size == d.coordinates.size, "Overlapping coordinates found, drawing is not planar.")
  }

}
