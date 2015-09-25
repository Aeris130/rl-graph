package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex, FlowVertexReplacer}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FlowVertexReplacerSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)

  def graph = new {
    val v1 = new FlowVertex(1, 2) // Source
    val v2 = new FlowVertex(2, 0)
    val v3 = new FlowVertex(3, -2) // Sink
    val e1 = v1 ~> v2 ## (12, 0, 2, 0, 0) // Capacity 2
    val e2 = v2 ~> v3 ## (23, 0, 2, 0, 0) // Capacity 2
    val g = Graph.from(List(v1, v2, v3), List(e1, e2))
  }

  describe("VertexReplacer") {

    it ("shouldn't add any extra vertices") {

      Given("a graph")
      val f = graph
      import f._

      When("replacing one of the vertices with a copy")
      val copy = new FlowVertex(4, 0)
      val newGraph = FlowVertexReplacer.replace(g, v1, copy)

      Then("the amount of vertices should be the same")
      newGraph.nodes should have size (3)

      And("the other two vertices should remain")
      newGraph.nodes should contain (newGraph.get(v2))
      newGraph.nodes should contain (newGraph.get(v3))

      And("the old node should have been replaced")
      newGraph.contains(copy) should be (true)
      newGraph.contains(v1) should be (false)

    }

    it ("should add all outgoing edges to the new copy") {

      Given("a graph with vertex v1 having an outgoing edge e1")
      val f = graph
      import f._

      When("replacing one of the vertices with a copy")
      val copy = new FlowVertex(4, 0)
      val newGraph = FlowVertexReplacer.replace(g, v1, copy)

      Then("the new graph should contain an edge e1 going to the copy")
      newGraph.edges.filter(e => e.from == newGraph.get(copy)) should have size (1)

    }

    it ("should add all incoming edges to the new copy") {

      Given("a graph with vertex v2 having an incoming edge e1")
      val f = graph
      import f._

      When("replacing one of the vertices with a copy")
      val copy = new FlowVertex(4, 0)
      val newGraph = FlowVertexReplacer.replace(g, v2, copy)

      Then("the new graph should contain an edge e1 going to the copy")
      newGraph.edges.filter(e => e.to == newGraph.get(copy)) should have size (1)

    }
  }
}
