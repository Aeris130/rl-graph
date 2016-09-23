package rlgraph.integration.drawings.planar.grid

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.canonicalOrder.planarBiconnected.Contour
import net.cyndeline.rlgraph.drawings.planar.grid.PlanarGridAlgorithm
import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.util.GraphCommons
import rlgraph.SpecImports
import rlgraph.help.GridLayoutData

import scalax.collection.GraphPredef._
import scala.language.postfixOps
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class PlanarGridAlgorithmSpec extends SpecImports {
  private val algorithm = new PlanarGridAlgorithm()

  /**
    * A maximally planar graph with triangles 1, 2, 3 (outer) and 4, 5, 6 (inner) with 7 in the middle.
    */
  private def maximalGraph = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(2, 3)
      .embedEdge(Vertex(1) withDefaultPositionInVertex 6 withInsertPosition 3)
      .embedEdge(Vertex(1) withDefaultPositionInVertex 4 withInsertPosition 6)
      .embedEdge(Vertex(4) withInsertPosition 1 inVertex 2 withDefaultInsertPosition)
      .embedEdge(Vertex(5) withInsertPosition 4 inVertex 2 withDefaultInsertPosition)
      .embedEdge(Vertex(5) withInsertPosition 2 inVertex 3 withDefaultInsertPosition)
      .embedEdge(Vertex(6) withInsertPosition 5 inVertex 3 withDefaultInsertPosition)
      .embedEdge(Vertex(4) withInsertPosition 2 inVertex 5 withInsertPosition 1)
      .embedEdge(Vertex(7) withInsertPosition 1 inVertex 4 withDefaultInsertPosition)
      .embedEdge(Vertex(6) withInsertPosition 4 inVertex 5 withInsertPosition 3)
      .embedEdge(Vertex(7) withInsertPosition 1 inVertex 4 withInsertPosition 5)
      .embedEdge(Vertex(6) withInsertPosition 1 inVertex 4 withInsertPosition 5)
      .embedEdge(Vertex(6) withInsertPosition 4 inVertex 7 withInsertPosition 5)
      .embedEdge(Vertex(7) withInsertPosition 4 inVertex 5 withInsertPosition 6)
  }

  describe("PlanarGridAlgorithm") {

    it ("should draw an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("computing a drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("the drawing should be empty")
      drawing.isEmpty should be (true)

    }

    it ("should draw a single vertex") {

      Given("a graph with a single vertex 1")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing a drawing")
      val drawing = algorithm.computeDrawing(graph)

      Then("vertex 1 should have coordinate (0,0)")
      drawing.vertices should be (Vector(1))
      drawing.coordinates(1) should be (Point(0, 0))

    }

    it ("should draw 1~2") {

      Given("the edge 1~2")
      val e = UndirectedEmbedding[Int]().embed(1, 2)

      When("computing a drawing using edge 1-2 as start")
      val drawing = algorithm.computeDrawing(e, (1, 2))

      Then("vertex 1, 2 should have coordinate (0, 0), (1, 0)")
      drawing.coordinates(1) should be (Point(0, 0))
      drawing.coordinates(2) should be (Point(1, 0))

    }

    it ("should draw a triangle") {

      Given("a triangular graph")
      val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(2, 3)

      When("computing a drawing using edge 1-2 as start")
      val drawing = algorithm.computeDrawing(e, (1, 2))

      Then("vertex 1, 2, 3 should have coordinate (0, 0), (2, 0), (1, 1)")
      drawing.coordinates(1) should be (Point(0, 0))
      drawing.coordinates(2) should be (Point(2, 0))
      drawing.coordinates(3) should be (Point(1, 1))

    }

    it ("should assign width and height to the drawing") {

      Given("a triangular graph")
      val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(2, 3)

      When("computing a drawing using edge 1-2 as start")
      val drawing = algorithm.computeDrawing(e, (1, 2))

      Then("the drawings width should be the highest x value + 1")
      drawing.width should be (drawing.coordinates.values.maxBy(_.x).x + 1)

      And("the drawings height should be the highest y value + 1")
      drawing.height should be (drawing.coordinates.values.maxBy(_.y).y + 1)

    }

    it ("should draw a face of vertices") {

      Given("a face 1, 3, 4, 5, 2")
      val f = GridLayoutData.singleRow
      import f._

      When("computing a drawing using edge 1-2 as start")
      val drawing = algorithm.computeDrawing(e, (1, 2))

      Then("vertex 1 should have coordinate (0, 0)")
      drawing.coordinates(1) should be (Point(0, 0))

      And("vertex 2 should have coordinate (2n - 4, 0")
      val v2c = drawing.coordinates(2)
      v2c should be (Point(2 * drawing.vertices.length - 4, 0))

      And("vertex 3 should have y coordinate 1")
      val v3c = drawing.coordinates(3)
      v3c.y should be (1)

      And("vertex 4 should have y coordinate 2")
      val v4c = drawing.coordinates(4)
      v4c.y should be (2)

      And("vertex 5 should have y coordinate 3")
      val v5c = drawing.coordinates(5)
      v5c.y should be (3)

      And("vertex 4 should have its x coordinate between 1 and 3 or 3 and 2")
      assert(v4c.x > 0 && v4c.x < v2c.x)
      assert(v4c.x < v3c.x || v4c.x > v3c.x)

      And("vertex 5 should have its x coordinate between 4 and 2")
      assert(v5c.x > v4c.x && v5c.x < v2c.x)

    }

    it ("should store vertices in a canonical order") {

      /* This test fails if the contour class throws errors due to invalid vertex order. Hinges on the fact that
       * the algorithm uses the same contour implementation.
       */

      Given("a biconnected planar graph")
      val f = maximalGraph
      import f._

      When("computing a drawing using edge 1-2 as start")
      val drawing = algorithm.computeDrawing(e, (1, 2))

      Then("the vertices of the drawing should constitute a valid canonical order")
      var c = new Contour[Int](1, 2, e)
      for (v <- drawing.vertices.drop(2)) // 1 and 2 is already on C
        c = c.addVertex(v).newContour

    }

    it ("should compute a drawing from a graph") {

      Given("a biconnected graph")
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 5~2, 2~6, 6~7, 7~4)

      When("drawing the graph")
      val drawing = algorithm.computeDrawing(graph)

      Then("no overlapping coordinates should exist")
      assert(drawing.vertices.map(drawing.coordinates).toSet.size == drawing.vertices.size, "Overlapping coordinates found.")

      And("every neighbor of a vertex should be drawn above or below it, and to the left or right of it")
      val v1 = drawing.vertices.find(v => drawing.coordinates(v) == Point(0, 0)).get
      val v2 = drawing.coordinates.maxBy(kv => kv._2.x)._1
      for (v <- drawing.vertices; neighbor <- GraphCommons.outerNeighbors(v, graph)) {
        val vc = drawing.coordinates(v)
        val nc = drawing.coordinates(neighbor)

        if (Set(v, neighbor) != Set(v1, v2)) {
          assert(vc.y < nc.y || vc.y > nc.y, "Neighbors " + vc + " and " + nc + " were drawn at the same y coordinate.")
          assert(vc.x < nc.x || vc.x > nc.x, "Neighbors " + vc + " and " + nc + " were drawn at the same x coordinate.")
        }
      }

    }

    it ("should include the original edges of a graph") {

      Given("a graph with edges 1-2, 2-3 and 3-4")
      val graph = Graph(1~2, 2~3, 3~4)

      When("drawing the graph")
      val drawing = algorithm.computeDrawing(graph)

      Then("only the original edges should be included in the final drawing")
      drawing.edges should have size 3
      drawing.edges.map(UnorderedPair(_)).toSet should equal (Set(UnorderedPair(1, 2), UnorderedPair(2, 3), UnorderedPair(3, 4)))

    }

    it ("should not include any edges if the graph is completely disconnected") {

      Given("a non-biconnected graph without edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3, 4)

      When("drawing the graph")
      val drawing = algorithm.computeDrawing(graph)

      Then("no edges should be included in the final drawing")
      drawing.edges should be ('empty)

    }

  }

}
