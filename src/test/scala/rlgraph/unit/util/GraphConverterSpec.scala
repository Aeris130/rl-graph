package rlgraph.unit.util

import net.cyndeline.rlgraph.util.GraphConverter
import rlgraph.SpecImports

import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class GraphConverterSpec extends SpecImports {

  describe("GraphConverter") {

    it ("should only call node factory methods once per vertex") {

      Given("a graph with two edges both having the vertex 1")
      val graph = Graph(1~2, 1~3)

      When("converting the graph using a function that increments an integer every time a vertex is converted")
      var i = 0
      def convert(v: Int): String = { i += 1; v.toString }
      GraphConverter.toUndirectedWithNodes[Int, String, UnDiEdge](convert).convert(graph)

      Then("i should be 3")
      i should be (3)

    }

    it ("should convert an undirected graph") {

      Given("a graph with the edges 1->2, 2->3")
      val graph = Graph(1~>2, 2~>3)

      When("converting the graph using a function that reverses the edges")
      val converted = GraphConverter.convertEdges[Int, DiEdge, DiEdge]((e: DiEdge[Int], v1: Int, v2: Int) => v2~>v1).convert(graph)

      Then("every edge in the result should be reversed")
      converted should be (Graph(2~>1, 3~>2))

    }

  }

}
