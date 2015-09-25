package rlgraph.unit.subgraph.isomorphism

import net.cyndeline.rlgraph.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter
import rlgraph.SpecImports

import scala.collection.JavaConversions._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class ScalaGraphToJGraphTConverterSpec extends SpecImports {
  private val converter = new ScalaGraphToJGraphTConverter()

  class VertexInstance() {}

  describe("ScalaGraphToJGraphTConverter") {

    it ("should add all vertices of the original to the new graph") {

      Given("a graph with 3 vertices")
      val graph: Graph[Int, UnDiEdge] = Graph(1~2, 2~3)

      When("converting the graph")
      val jGraphT = converter.convert(graph)

      Then("the vertex set should contain 1, 2 and 3")
      val vertices = jGraphT.vertexSet.toSet
      vertices should have size (3)
      vertices should contain (1)
      vertices should contain (2)
      vertices should contain (3)

    }

    it ("should add all edges of the original to the new graph") {

      Given("a graph with edges 1~2, 2~3")
      val graph: Graph[Int, UnDiEdge] = Graph(1~2, 2~3)

      When("converting the graph")
      val jGraphT = converter.convert(graph)

      Then("the edges 1~2, 2~3 should be present in the new graph")
      jGraphT.edgeSet().toSet should have size 2
      jGraphT.getEdge(1, 2) should not be null
      jGraphT.getEdge(2, 3) should not be null

    }

    it ("should maintain vertex equality between vertices converted to multiple graphs") {

      Given("a graph with vertices 1, 2, 3")
      val v1 = new VertexInstance()
      val v2 = new VertexInstance()
      val v3 = new VertexInstance()
      val g1 = Graph[VertexInstance, UnDiEdge](v1~v2, v2~v3)

      When("converting the graph twice")
      val InstanceConverter = new ScalaGraphToJGraphTConverter()
      val jGraphT1 = InstanceConverter.convert(g1)
      val jGraphT2 = InstanceConverter.convert(g1)

      Then("the vertex sets of both converted graphs should be equal")
      jGraphT1.vertexSet().toSet should equal (jGraphT2.vertexSet().toSet)

    }

  }
}
