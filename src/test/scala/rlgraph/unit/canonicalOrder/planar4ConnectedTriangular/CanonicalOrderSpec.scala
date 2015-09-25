package rlgraph.unit.canonicalOrder.planar4ConnectedTriangular

import net.cyndeline.rlgraph.canonicalOrder.planar4ConnectedTriangular.CanonicalOrder
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class CanonicalOrderSpec extends SpecImports {
  private val embedder = new BoyerMyrwoldEmbedder[Int]()

  describe("CanonicalOrder") {

    it ("should throw an error if every vertex in the graph doesn't have degree 4") {

      Given("a triangular graph where vertex 3 has degree 3")
      val graph = Graph(1~2, 2~3, 3~1, 1~4, 4~3, 4~5, 1~5)
      val embedding = embedder.embed(graph).get

      When("computing the canonical order")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new CanonicalOrder(1, 2, 4, 5, embedding)
      }

    }

    it ("should compute an order for a 4-connected graph with inner vertices") {

      Given("a graph with inner vertices 3, 4, 5, 6, 7 and outer vertices 1, 2, 8, 9")
      val outer = Graph(1~2, 2~9, 9~8, 8~1, 1~9)
      val between = Graph(1~3, 1~4, 2~4, 2~7, 8~3, 8~6, 9~6, 9~7)
      val inner = Graph(3~4, 3~5, 3~6, 4~5, 4~7, 5~6, 5~7, 6~7)
      val graph = outer ++ between ++ inner
      val embedding = embedder.embed(graph).get

      When("computing the order using 1 and 2 as start edge, 9 as vn and 8 as v(n-1)")
      val order = new CanonicalOrder(1, 2, 8, 9, embedding)

      Then("vertex 1 and 2 should have order 1 and 2")
      order.vertexOrder(1) should be (1)
      order.vertexOrder(2) should be (2)

      And("vertex 8 and 9 should have order 8 and 9")
      order.vertexOrder(8) should be (8)
      order.vertexOrder(9) should be (9)

      And("vertex 6 should have order 7")
      order.vertexOrder(6) should be (7) // The only vertex adjacent to 8 and 9

      And("vertex 4 should have order 3")
      order.vertexOrder(4) should be (3) // The only vertex adjacent to 1 and 2

      And("every vertex 3 - 7 should have at least two higher and two lower neighbors")
      hasHigherNeighbors(3, graph, order)
      hasHigherNeighbors(4, graph, order)
      hasHigherNeighbors(5, graph, order)
      hasHigherNeighbors(6, graph, order)
      hasHigherNeighbors(7, graph, order)

    }

  }

  private def hasHigherNeighbors(v: Int, g: Graph[Int, UnDiEdge], order: CanonicalOrder[Int]): Unit = {
    val high = g.find(v).get.neighbors.count(n => order.vertexOrder(n) > order.vertexOrder(v))
    val low =  g.find(v).get.neighbors.count(n => order.vertexOrder(n) < order.vertexOrder(v))
    val max = order.ordering.map(_._2).max

    if (order.vertexOrder(v) < max - 1)
      assert(high >= 2, "Vertex " + v + " did not have two higher neighbors.")

    if (order.vertexOrder(v) > 2)
      assert(low >= 2, "Vertex " + v + " did not have two lower neighbors.")
  }
}
