package rlgraph.unit.rectangular

import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.embedding.help.CommonVertexFinder
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class CommonVertexFinderSpec extends SpecImports {
  private val vertexFinder = new CommonVertexFinder()

  describe("CommonVertexFinder") {

    it ("should find all vertices as common in a triangle") {

      Given("a triangle")
      val e1 = 1~2
      val e2 = 2~3
      val e3 = 3~1
      val graph = Graph(e1, e2, e3)

      When("computing all common vertices")
      val cv1 = vertexFinder.findCommonVertices(e1, graph)
      val cv2 = vertexFinder.findCommonVertices(e2, graph)
      val cv3 = vertexFinder.findCommonVertices(e3, graph)

      Then("vertex 3 should be common to 1~2")
      cv1 should equal (Vector(3))

      And("vertex 1 should be common to 2~3")
      cv2 should equal (Vector(1))

      And("vertex 2 should be common to 3~1")
      cv3 should equal (Vector(2))

      And("the graph should contain 3 common vertices")
      vertexFinder.numberOfCommonVertices(graph) should be (3)

    }

    it ("should find both common vertices for an edge connecting two triangles") {

      Given("two triangles connected by an edge")
      val connectingEdge = 1~2
      val graph = Graph(connectingEdge, 2~3, 3~1, 1~4, 4~2)

      When("computing all common vertices for the connecting edge")
      val connectingVertices = vertexFinder.findCommonVertices(connectingEdge, graph)

      Then("vertices 3 and 4 should be common")
      connectingVertices.size should be (2)
      connectingVertices.toSet should be (Set(3, 4))

    }

    it ("should ignore vertices not connected to both ends of an edge") {

      Given("an edge 1~2 connected to another edge in one end")
      val edge = 1~2
      val graph = Graph(edge, 2~3)

      When("computing all common vertices for the edge")
      val connectingVertices = vertexFinder.findCommonVertices(edge, graph)

      Then("no common vertices should be found")
      connectingVertices should be (Vector())
      vertexFinder.numberOfCommonVertices(graph) should be (0)

    }

  }
}
