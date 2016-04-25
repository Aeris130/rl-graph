package rlgraph.unit.planar

import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, AdjacencyList}
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DemoucronEmbeddingSpec extends SpecImports {
  private val embedder = new DemoucronEmbedding[Int, UnDiEdge]()

  describe("DemoucronEmbedding") {

    it ("should throw an exception if the graph isn't connected") {

      Given("a non-connected graph")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](2~3, 1)

      When("attempting to embed it")
      Then("an exception whould be thrown")
      intercept[IllegalArgumentException] {
        embedder.embed(graph)
      }
    }

    it ("should embed a single bridge") {

      Given("a graph with 2 vertices and a single edge between them")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2)

      When("embedding it")
      val embedding = embedder.embed(graph).get

      Then("both vertices should be represented in the embedding")
      assert(embedding.embeddingFor(1).isDefined, "Vertex 1 were not embedded")
      assert(embedding.embeddingFor(2).isDefined, "Vertex 2 were not embedded")

      And("both vertices should contain a single edge to the other vertex")
      val emb1 = embedding.embeddingFor(1)
      val emb2 = embedding.embeddingFor(2)

      emb1.size should be (1)
      emb2.size should be (1)

      assert(emb1.containsEntryFor(2), "The edge to vertex 2 was not embedded in vertex 1")
      assert(emb2.containsEntryFor(1), "The edge to vertex 1 was not embedded in vertex 2")

    }

    it ("should embed two connected bridges") {

      Given("a graph with 3 vertices connected by 2 bridges")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2, 2~3)

      When("embedding it")
      val embedding = embedder.embed(graph).get

      Then("all 3 vertices should be present in the embedding")
      assert(embedding.embeddingFor(1).isDefined, "Vertex 1 were not embedded")
      assert(embedding.embeddingFor(2).isDefined, "Vertex 2 were not embedded")
      assert(embedding.embeddingFor(3).isDefined, "Vertex 3 were not embedded")

      And("vertex 1 should have a single neighbor to vertex 2")
      val emb1 = embedding.embeddingFor(1)

      emb1.size should be (1)
      assert(emb1.containsEntryFor(2), "The edge to vertex 2 was not embedded in vertex 1")

      And("vertex 2 should contain both 1 and 3 as neighbors")
      val emb2 = embedding.embeddingFor(2)

      emb2.size should be (2)
      assert(emb2.containsEntryFor(1), "The edge to vertex 1 was not embedded in vertex 2")
      assert(emb2.containsEntryFor(3), "The edge to vertex 3 was not embedded in vertex 2")

      And("vertex 3 should have a single neighbor to vertex 2")
      val emb3 = embedding.embeddingFor(3)

      emb3.size should be (1)
      assert(emb3.containsEntryFor(2), "The edge to vertex 2 was not embedded in vertex 3")
    }

    it ("should embed a single biconnected component") {

      Given("a maximal biconnected graph")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2, 2~3, 3~1)

      When("embedding it")
      val embedding = embedder.embed(graph).get

      Then("all 3 vertices should be present in the embedding")
      assert(embedding.embeddingFor(1).isDefined, "Vertex 1 were not embedded")
      assert(embedding.embeddingFor(2).isDefined, "Vertex 2 were not embedded")
      assert(embedding.embeddingFor(3).isDefined, "Vertex 3 were not embedded")

      And("vertex 1 should have edges to 2 and 3 embedded")
      val emb1 = embedding.embeddingFor(1)
      assert(emb1.containsEntryFor(2), "vertex 1 did not contain an edge to vertex 2")
      assert(emb1.containsEntryFor(3), "vertex 1 did not contain an edge to vertex 3")

      And("the edges of vertex 1 should be linked to each other")
      val totalSet = Set(2, 3)
      totalSet should contain (emb1.head.adjacentVertex)
      (totalSet - emb1.head.adjacentVertex) should contain (emb1.head.next.adjacentVertex)
      emb1.head.next.previous should equal (emb1.head)

    }

    it ("should report k33 as non-planar") {

      Given("a non-planar k33 graph")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~4, 1~5, 1~6, 2~4, 2~5, 2~6, 3~4, 3~5, 3~6)

      When("embedding it")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

      And("isPlanar returns false")
      assert(!embedder.isPlanar(graph), "The non-planar k33 graph " + graph + " was embedded despite being non-planar.")
    }

    /**
     * This test uses a graph with additional edges beyond k55, otherwise the graph would simply get caught by
     * Euler's theorem.
     */
    it ("should report a graph containing k55 as non-planar") {

      Given("a non-planar k55 graph")
      var graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2, 1~3, 1~4, 1~5, 2~3, 2~4, 2~5, 3~4, 3~5, 4~5)
      graph += (1~7, 7~8, 8~9, 9~10, 10~11, 11~12)

      When("embedding it")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

      And("isPlanar returns false")
      assert(!embedder.isPlanar(graph), "The non-planar k55 graph " + graph + " was embedded despite being non-planar.")

    }

    /**
     * Making use of the unique embedding of a graph that is 4-connected and where
     * every face is a triangle. The flipped embedding is always available though,
     * so every test checks for either one embedding or its inverse.
     */
    it ("should embed a 4-connected planar graph") {

      /* If true, the graph forms a triangle with 1 on top, 2 bottom left, 3 bottom right. Then another triangle in the middle
       * with 4 upper left, 5 upper right, 6 bottom. False and the triangle is mirrored.
       */
      var dir = false

      Given("a 4-connected planar graph where every face is a triangle")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2, 1~4, 1~5, 1~3, 2~4, 2~6, 2~3, 3~6, 3~5, 4~5, 4~6, 5~6)

      When("embedding it")
      val embedding = embedder.embed(graph).get

        // Check which direction the embedding goes at. This direction will be assumed for every other adj. list.
        val emb1 = embedding.embeddingFor(1)
        val entryFor2In1 = emb1.entryFor(2)
        val entryFor3In1 = emb1.entryFor(3)
        val entryFor4In1 = emb1.entryFor(4)
        if (entryFor2In1.next == entryFor4In1) dir = false
        else if (entryFor2In1.next == entryFor3In1) dir = true
        else throw new Error("Neither edge 4 nor 3 was found, instead: " + entryFor2In1.next)
        // End direction check

      Then("the outer vertex 1 should have 2, 4, and 5 and 3 as neighbors")
      val expectedOrderFor1 = edgeOrder(Vector(2, 4, 5, 3), dir)
      assert(orderMatches(emb1, expectedOrderFor1), "The edge order " + expectedOrderFor1 + " did not match embedded edges " + emb1)

      And("the inner vertex 4 should have 2, 6, 5, 1 as neighbors")
      val emb4 = embedding.embeddingFor(4)
      val expectedOrderFor4 = edgeOrder(Vector(2, 6, 5, 1), dir)
      assert(orderMatches(emb4, expectedOrderFor4), "The edge order " + expectedOrderFor4 + " did not match embedded edges " + emb4)

    }

    it ("should embed two biconnected components") {

      Given("a butterfly graph with components 1~2~3 and 3~4~5")
      val graph: Graph[Int, UnDiEdge] = Graph[Int, UnDiEdge](1~2, 2~3, 3~1, 3~4, 4~5, 5~3)

      When("embedding it")
      val embedding = embedder.embed(graph).get

      Then("the vertex 3 should contain edges 1 and 2 consecutively, as well as edges 4 and 5")
      val emb3 = embedding.embeddingFor(3)
      val edge1 = emb3.entryFor(1)
      val edge2 = emb3.entryFor(2)
      val edge4 = emb3.entryFor(4)
      val edge5 = emb3.entryFor(5)

      val oneAndTwoConsec = (edge1.next == edge2 && edge2.previous == edge1) || (edge2.next == edge1 && edge1.previous == edge2)
      val fourAndFiveConsec = (edge4.next == edge5 && edge5.previous == edge4) || (edge5.next == edge4 && edge4.previous == edge5)
      assert(oneAndTwoConsec, "Edges 1 and 2 were not consecutive in embedding " + emb3)
      assert(fourAndFiveConsec, "Edges 4 and 5 were not consecutive in embedding " + emb3)

    }

    it ("should embed graphs deterministically") {

      Given("a graph")
      val graph = Graph(1~2, 2~3, 3~1, 1~4, 4~5, 5~3, 2~6, 2~7)

      When("embedding the graph multiple times")
      val embeddings = for (i <- 1 to 50) yield embedder.embed(graph).get
      val numberOfEmbeddings = embeddings.length

      Then("every embedding should equal every other embedding")
      assert(!embeddings.exists(e => embeddings.count(_ == e) != numberOfEmbeddings), "Every embedding was not deterministic.")

    }
  }

  /**
   * Checks if a list of edges have been embedded in a certain order.
   */
  private def orderMatches(adj: AdjacencyList[Int], edgeOrder : Vector[Int]): Boolean = {
    var currentEntry: AdjacencyEntry[Int] = adj.entryFor(edgeOrder(0))
    val start = currentEntry
    var remainingEdges = edgeOrder

    while (remainingEdges.nonEmpty) {
      if (currentEntry.adjacentVertex == remainingEdges(0)) {
        currentEntry = currentEntry.next
        remainingEdges = remainingEdges.drop(1)

      } else {
        return false
      }
    }

    true
  }

  /**
   * Reverses the direction of a list of expected to.
   */
  private def edgeOrder(edges: Vector[Int], direction: Boolean) = if (!direction) edges else edges.reverse

}
