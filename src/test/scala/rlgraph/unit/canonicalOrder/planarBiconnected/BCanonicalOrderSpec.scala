package rlgraph.unit.canonicalOrder.planarBiconnected

import net.cyndeline.rlgraph.canonicalOrder.planarBiconnected.BCanonicalOrder
import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.planar.boyerMyrwold.BoyerMyrwoldEmbedder
import rlgraph.SpecImports

import scala.language.postfixOps
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BCanonicalOrderSpec extends SpecImports {
  private val embedder = new BoyerMyrwoldEmbedder[Int]()
  private def computeOrder(v1: Int, v2: Int, e: Embedding[Int]): Vector[Int] = {
    val bco = new BCanonicalOrder[Int]((e: Embedding[Int]) => (v1, v2))
    bco.order(e)
  }

  describe("BCanonicalOrder") {

    it ("should compute ordering for a single edge") {

      Given("an embedding with the edge 1-2")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("computing the canonical order using start edge 1-2")
      val order = computeOrder(1, 2, embedding)

      Then("the vertex order should be 1, 2")
      order should be (Vector(1, 2))

    }

    it ("should compute ordering for a triangle") {

      Given("a triangular embedding")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 1)

      When("computing the canonical order using start edge 1-2")
      val order = computeOrder(1, 2, embedding)

      Then("the vertex order should be 1, 2, 3")
      order should be (Vector(1, 2, 3))

    }

    it ("should compute ordering for a non-maximal graph") {

      Given("an edge 1~2 with two paths leading to a vertex 5")
      val embedding = embedder.embed(Graph(1~2, 1~3, 3~5, 2~4, 4~5)).get

      When("computing the canonical order using start edge 1-2")
      val order = computeOrder(1, 2, embedding)

      Then("the first vertices should be 1 and 2")
      order(0) should be (1)
      order(1) should be (2)

      And("the last three vertices should be 3, 4, 5 in any order")
      Set(order(2), order(3), order(4)) should be (Set(3, 4, 5))

    }

    it ("should compute ordering for a graph where edge connections dictate ordering beyond the initial edge") {

      Given("a maximal planar graph")
      val embedding = embedder.embed(Graph(1~2, 1~3, 1~6, 2~3, 2~4, 2~6, 3~6, 3~5, 3~4, 4~5, 4~6, 5~6)).get

      When("computing the canonical order using start edge 1-2")
      val order = computeOrder(1, 2, embedding)

      Then("the first vertices should be 1 and 2")
      order(0) should be (1)
      order(1) should be (2)

      And("the next vertex should be 3")
      order(2) should be (3)

      And("the next three vertices should be 4, 5 and 6 in any order")
      Set(order(3), order(4), order(5)) should be (Set(4, 5, 6))

    }

    it ("should adhere strictly to neighbor order where one exists") {

      Given("a graph where vertex 3 is the neighbor of the starting edge S, vertex 4 is neighbor of S and 3, and vertex 5 is neighbor of S and 4")
      val embedding = embedder.embed(Graph(1~2, 1~3, 2~3, 1~4, 2~4, 3~4, 1~5, 2~5, 4~5)).get

      When("computing the canonical order using start edge 1-2")
      val order = computeOrder(1, 2, embedding)

      Then("the first vertices should be 1 and 2")
      order(0) should be (1)
      order(1) should be (2)

      And("the next vertex should be 3")
      order(2) should be (3)

      And("the next vertex should be 4")
      order(3) should be (4)

      And("the next vertex should be 5")
      order(4) should be (5)

    }

    it ("should compute the correct ordering for an embedding where a vertex has the same face to the left and right of its neighbors") {

      Given("an embedding with vertex 0 having the same face to the left and right")
      val embedding = UndirectedEmbedding[Int]().embed(0, 4).embed(4, 2).embed(2, 1).embed(1, 3).embed(3, 0)
        .embedEdge(Vertex(3) withInsertPosition 0 inVertex 4 withInsertPosition 1)
        .embedEdge(Vertex(1) withInsertPosition 3 inVertex 4 withInsertPosition 2)

      When("computing the canonical order using start edge 4-3")
      val order = computeOrder(4, 3, embedding)

      Then("the order should be 4, 3, 0, 2, 1")
      order should be (Seq(4, 3, 0, 2, 1))

    }

  }

}
