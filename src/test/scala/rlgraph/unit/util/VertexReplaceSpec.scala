package rlgraph.unit.util

import net.cyndeline.rlgraph.util.{EdgeCopyFactory, UndirectedEdgeFactory, VertexReplacer}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class VertexReplaceSpec extends SpecImports {
  val undirectedReplacer = new VertexReplacer[Int, UnDiEdge](new UndirectedEdgeFactory[Int]())

  describe("VertexReplace") {

    it ("should replace a vertex in an undirected graph") {

      Given("a graph with vertex 2 having two edges connecting it to vertices 1 and 3")
      val g = Graph(1~2, 2~3)

      When("replacing vertex 2 with vertex 4")
      val result = undirectedReplacer.replace(2, 4, g)

      Then("vertex should be replaced with vertex 4")
      result should equal (Graph(1~4, 4~3))

    }

    it ("should replace a vertex in a directed graph") {
      val replacer = new VertexReplacer[Int, DiEdge](new DirectedEdgeFactory())

      Given("a graph with vertex 2 having two directed edges connecting it to vertices 1 and 3")
      val g = Graph(1~>2, 3~>2)

      When("replacing vertex 2 with vertex 4")
      val result = replacer.replace(2, 4, g)

      Then("vertex should be replaced with vertex 4")
      result should equal (Graph(1~>4, 3~>4))

    }

    it ("should preserve unrelated vertices") {

      Given("a graph with a vertex 5 not connected to any edge being replaced")
      val g = Graph[Int, UnDiEdge](1~2, 2~3, 5)

      When("replacing vertex 2 with vertex 4")
      val result = undirectedReplacer.replace(2, 4, g)

      Then("the resulting graph should contain vertex 5")
      assert(result.contains(5), "The expected vertex 5 was not preserved when replacing edges in the graph.")

    }

    it ("should preserve unrelated edges") {

      Given("a graph with vertices 5 and 6 connected by an edge, not connected to the vertex being replaced")
      val g = Graph[Int, UnDiEdge](1~2, 2~3, 5~6, 1~5)

      When("replacing vertex 2 with vertex 4")
      val result = undirectedReplacer.replace(2, 4, g)

      Then("the resulting graph should contain the edge 5~6")
      assert(result.contains(5~6), "The expected edge 5~6 was not preserved when replacing edges in the graph.")

    }

    it ("should maintain the same node ordering with subsequent replaces") {

      Given("a graph with nodes in a specific order")
      val g = Graph[Int, UnDiEdge](1, 2, 3, 4, 5, 6)
      val nodes = g.nodes.toVector

      When("performing the same replacement multiple times")
      val replace1 = undirectedReplacer.replace(1, 7, g).nodes.toVector
      val replace2 = undirectedReplacer.replace(1, 7, g).nodes.toVector
      val replace3 = undirectedReplacer.replace(1, 7, g).nodes.toVector
      val replace4 = undirectedReplacer.replace(1, 7, g).nodes.toVector

      replace1 should equal(replace2)
      replace2 should equal (replace3)
      replace3 should equal (replace4)

    }
  }

  private class DirectedEdgeFactory extends EdgeCopyFactory[Int, DiEdge] {
    def copyEdge(edge: DiEdge[Int], a: Int, b: Int): DiEdge[Int] = a~>b
  }

}
