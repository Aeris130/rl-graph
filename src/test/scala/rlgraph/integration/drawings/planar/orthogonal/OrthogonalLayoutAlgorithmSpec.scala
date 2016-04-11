package rlgraph.integration.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.OrthogonalLayoutAlgorithm
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class OrthogonalLayoutAlgorithmSpec extends SpecImports {
  private val orthoDrawAlgorithm = new OrthogonalLayoutAlgorithm[Int, UnDiEdge]()

  describe("OrthogonalLayoutAlgorithm") {

    it ("should throw an exception when attempting to draw a graph having a vertex with more than 4 edges connected to it") {

      Given("a graph where a vertex has 5 edges connected to it")
      val g = Graph(1~2, 1~3, 1~4, 1~5, 1~6)

      When("computing a drawing for the graph")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        orthoDrawAlgorithm.orthogonalize(g)
      }

    }

    it ("should throw an exception for attempting to draw a non-connected graph") {

      Given("a non-connected graph")
      val g = Graph[Int, UnDiEdge](1~2, 3~4, 5)

      When("computing a drawing for the graph")
      Then("an exception should be thrown")
      val thrown = intercept[IllegalArgumentException] {
        orthoDrawAlgorithm.orthogonalize(g)
      }

      thrown.getMessage should be ("The supplied graph was not connected.")

    }

    it ("should produce a drawing of a single vertex, giving it coordinate 0") {

      Given("a graph with a single vertex")
      val g = Graph[Int, UnDiEdge](99)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      Then("the drawing should contain no edges")
      drawing.edges should be ('empty)

      And("a single vertex with coordinate (0, 0)")
      drawing.vertices should have size (1)
      drawing.vertices should contain ((99, 0, 0))

    }

    /* This is an interesting test-graph, since the single edge means it'll only have one face, and
     * only have shape-graphs in a single orientation.
     */
    it ("should produce drawings from a graph directed in a single direction") {

      Given("a graph")
      val g = Graph(1~2)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)
    }

    /* Could either be on the same x/y axis or not */
    it ("should produce drawings from a graph directed in two directions") {

      Given("a graph")
      val g = Graph(0~1, 0~2)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should produce drawings with a vertex having 3 edges") {

      Given("a graph")
      val g = Graph(0~1, 0~2, 0~3)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should produce drawings with a dummy vertex") {

      Given("a graph with a bend")
      val g = Graph(0~1, 1~2, 2~0)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should produce drawings with multiple bends") {

      Given("a graph with a bend")
      val g = Graph(0~1, 1~2, 2~0, 1~3, 0~3)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should produce drawings with cut points") {

      Given("a graph with a bend")
      val g = Graph(0~1, 1~2, 2~0, 0~4, 4~5, 5~0)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should handle the litmus test!") {

      /* Here's the graph that originally caused my external library to bug out, making me write my own. :) */

      Given("a graph that makes the kieler library choke")
      val g = Graph(0~1, 0~5, 1~4, 1~3, 1~2, 3~4, 4~2, 4~6, 2~5, 2~7, 7~6, 6~5, 7~8, 8~9, 9~7)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should produce drawings deterministically") {

      Given("a graph")
      val g = Graph(0~1, 0~5, 1~4, 1~3, 1~2, 3~4, 4~2, 4~6, 2~5, 2~7, 7~6, 6~5, 7~8, 8~9, 9~7)

      When("computing a drawing for the graph 200 times")
      Then("all drawings should be equal")
      var currentDrawing = orthoDrawAlgorithm.orthogonalize(g)
      var i = 50
      while (i > 0) {
        val nextDrawing = orthoDrawAlgorithm.orthogonalize(g)
        nextDrawing should equal (currentDrawing)
        currentDrawing = nextDrawing
        i -= 1
      }

    }

    it ("should produce a drawing using directed edges") {

      Given("a graph with directed edges")
      val g = Graph[Int, UnDiEdge](0~>1, 0~>5, 1~>4, 3~>1, 1~>2, 4~>3, 4~>2, 6~>4, 2~>5, 2~>7, 7~>6, 6~>5, 7~>8, 8~>9, 9~>7)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)

    }

    it ("should throw an exception if the user inputs graphs with vertices sharing more than one edge") {

      Given("a graph with edges 1->2 and 2->1")
      val g = Graph[Int, UnDiEdge](1~>2, 2~>1)

      When("computing a drawing for the graph")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        orthoDrawAlgorithm.orthogonalize(g)
      }

    }

    /*
     *
     * Various graphs that have triggered errors in the past. Test-by-shotgun-approach.
     *
     */

    it ("trigger_graph_1") {
      val g = Graph[Int, UnDiEdge](1~4, 1~8, 2~1, 2~4, 3~0, 4~0, 5~3, 7~0, 7~6, 8~0, 8~4, 8~7)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)
    }

    it ("trigger_graph_2") {
      val g = Graph[Int, UnDiEdge](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1~4, 2~1, 4~0, 4~5, 5~6, 6~7, 7~0, 8~0, 8~3, 8~13, 10~9, 11~0, 11~3, 11~10, 12~3, 12~11, 13~3, 13~12)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)
    }

    /* Causes an artificial dart to be split in OrthogonalDrawingComputation. */
    it ("trigger_graph_3") {
      val g = Graph[Int, UnDiEdge](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 1~4, 2~1, 4~0, 4~5, 5~6, 6~7, 7~30, 8~10, 9~8, 10~11, 11~12, 12~13, 13~14, 14~15, 15~0, 15~20, 17~16, 18~17, 19~0, 19~18, 20~19, 21~9, 22~21, 23~22, 24~23, 25~24, 26~25, 27~3, 27~26, 28~3, 28~27, 29~3, 29~28, 30~0, 30~3, 30~29)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)
    }

    /* A graph that has its improvement heuristics applied more than once, in such a way that coordinates
     * overlap unless improvement in one direction takes the improvements made in the other direction
     * on the same pass unto account.
     */
    it ("trigger_graph_4") {
      val g = Graph[Int, UnDiEdge](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 1~4, 2~1, 4~0, 4~5, 5~6, 6~7, 7~8, 8~9, 9~10, 10~11, 11~12, 12~13, 13~14, 14~15, 15~0, 15~16, 16~17, 17~3, 17~40, 18~20, 19~0, 19~18, 20~21, 21~22, 22~23, 23~24, 24~25, 25~30, 27~26, 28~27, 29~28, 30~0, 30~29, 31~19, 32~31, 33~32, 34~33, 35~34, 36~35, 37~36, 38~3, 38~37, 39~3, 39~38, 40~3, 40~39)

      When("computing a drawing for the graph")
      val drawing = orthoDrawAlgorithm.orthogonalize(g)

      //println(drawing)
    }

  }
}
