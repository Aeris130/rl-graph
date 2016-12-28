package rlgraph.unit.triangulation.naive

import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.triangulation.naive.FaceTriangulationAlgorithm
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FaceTriangulationAlgorithmSpec extends SpecImports {
  private val algorithm = new FaceTriangulationAlgorithm()

  describe("FaceTriangulationAlgorithm") {

    it ("should not add edges to an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("no edges should be added")
      extraEdges should be ('empty)

    }

    it ("should not add edges to a graph with a single vertex") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("no edges should be added")
      extraEdges should be ('empty)

    }

    it ("should add edges to a graph with no edges") {

      Given("a graph with multiple vertex and no edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3, 4, 5)

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("every face should be triangular")
      triangular(graph, extraEdges)

    }

    it ("should add edges to a non-triangular graph") {

      Given("a non-triangular graph with edges")
      val graph = Graph(1~2, 2~3, 3~4, 3~5, 6~7)

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("every face should be triangular")
      triangular(graph, extraEdges)

    }

    it ("should add edges to a face with four vertices") {

      Given("a graph with a rectangular face")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 4~2)

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("every face should be triangular")
      triangular(graph, extraEdges)

    }

    it ("should not add edges to a triangular graph") {

      Given("a triangular graph")
      val graph = Graph(1~2, 2~3, 3~1, 1~4, 1~6, 2~5, 2~6, 3~4, 3~5, 4~5, 4~6, 5~6)

      When("triangulating the graph")
      val extraEdges = algorithm.triangulate(graph)

      Then("no edges should be added")
      extraEdges should be ('empty)

    }

  }

  private def triangular(graph: Graph[Int, UnDiEdge], extraEdges: Vector[(Int, Int)]): Unit = {
    val tri = graph ++ extraEdges.map(e => e._1~e._2)
    val faces = new FaceComputation[Int]().computeFacesFromGraph(tri)
    for (f <- faces)
      assert(f.vertexSize == 3, s"Face $f was not triangular.")
  }

}
