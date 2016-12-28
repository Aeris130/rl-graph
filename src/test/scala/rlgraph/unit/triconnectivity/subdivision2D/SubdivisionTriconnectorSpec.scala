package rlgraph.unit.triconnectivity.subdivision2D

import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.triconnectivity.subdivision2D.SubdivisionTriconnector
import net.cyndeline.rlgraph.util.GraphCommons
import rlgraph.SpecImports

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class SubdivisionTriconnectorSpec extends SpecImports {
  private val algorithm = new SubdivisionTriconnector()

  private def vertexFactory(start: Int) = new {
    var latest = start
    val allExtras = ArrayBuffer[Int]()
    def factory() = {
      latest += 1
      allExtras += latest - 1
      allExtras.last
    }
  }

  describe("SubdivisionTriconnector") {

    it ("should do nothing with an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("triconnecting the graph")
      val f = vertexFactory(0)
      val triconnection = algorithm.triconnectGraph(graph, f.factory)

      Then("no changes should be made")
      triconnection.extraVertices should be ('empty)
      triconnection.extraEdges should be ('empty)
      triconnection.graph should be ('empty)

    }

    it ("should triconnect a graph without edges") {

      Given("a graph with multiple vertices and no edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3, 4, 5)

      When("triconnecting the graph")
      val f = vertexFactory(6)
      val triconnection = algorithm.triconnectGraph(graph, f.factory)

      Then("every face in the graph should have size 3")
      val allFaces = faces(triconnection.graph)
      val nonTriangular = allFaces.find(_.vertexSize != 3)
      assert(nonTriangular.isEmpty, "A face in the triconnected graph was not triangular: " + nonTriangular)

      And("every edge in the graph should be in the extras")
      val newEdges = triconnection.graph.edges.map(e => (e._1, e._2))
      newEdges should equal (triconnection.extraEdges.toSet)

      And("every new vertex should be among the extras")
      val newVertices = GraphCommons.outerVertices(triconnection.graph).filterNot(n => graph.contains(n))
      triconnection.extraVertices.toSet should equal (newVertices.toSet)

    }

    it ("should avoid adding edges to a user-specified face") {

      Given("a biconnected graph with the face 1-2-3-4-5")
      val face = Face(1, 2, 3, 4, 5)
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 5~1)

      When("triconnecting the graph")
      val f = vertexFactory(6)
      val triconnection = algorithm.triconnectGraphWithOuterFace(graph, f.factory, (fs:  Vector[Face[Int]]) => face)

      Then("the face 1-2-3-4-5 should remain in the graph")
      val allFaces = faces(triconnection.graph)
      assert(allFaces.contains(face) || allFaces.contains(Face(face.vertices.reverse:_*)))

    }

    it ("should not add vertices and edges to a triangular face") {

      Given("a triangular graph")
      val graph = Graph(1~2, 2~3, 3~1)

      When("triconnecting the graph")
      val f = vertexFactory(4)
      val triconnection = algorithm.triconnectGraph(graph, f.factory)

      Then("no changes should be made")
      triconnection.extraVertices should be ('empty)
      triconnection.extraEdges should be ('empty)
      triconnection.graph should equal (graph)

    }

  }

  private def faces(g: Graph[Int, UnDiEdge]) = new FaceComputation[Int]().computeFacesFromGraph(g)

}
