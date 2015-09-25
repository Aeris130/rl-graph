package rlgraph.unit.planar

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, AdjacencyList, Embedding}
import net.cyndeline.rlgraph.planar.demoucron.operation.help.TreeEmbedMerger
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TreeEmbedMergerSpec extends SpecImports {
  val merger = new TreeEmbedMerger[Int, UnDiEdge]


  describe("TreeEmbedMerger") {

    it ("should join one embedding at the end of another embeddings adjacency list around a common vertex") {

      Given("two embeddings sharing the vertex 2 as a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2)
      val embedding2 = UndirectedEmbedding[Int]().embed(2, 3)
      val graph1 = Graph(1~2)
      val graph2 = Graph(2~3)

      When("merging the two embeddings")
      val result: Embedding[Int] = merger.merge(Map(embedding1 -> graph1, embedding2 -> graph2))

      Then("the vertex 2 should have outgoing edges to vertex 1 and 3 in the final embedding")
      val adj2: AdjacencyList[Int] = result.embeddingFor(2)
      adj2.size should be (2)

      /* Throws exception if not present. */
      adj2.entryFor(1)
      adj2.entryFor(3)

      And("vertex 1 and 3 should still only have edges to their original neighbor")
      val adj1: AdjacencyList[Int] = result.embeddingFor(1)
      adj1.size should be (1)
      adj1.entryFor(2)

      val adj3: AdjacencyList[Int] = result.embeddingFor(3)
      adj3.size should be (1)
      adj3.entryFor(2)
    }

    it ("should preserve the order of edges in an embedding when merging it with others") {

      Given("two embeddings with 2 edges each, sharing the vertex 3")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 3).embed(2, 3, Some(1), None) // 3: 1 then 2
      val embedding2 = UndirectedEmbedding[Int]().embed(3, 4).embed(3, 5, None, Some(4)) // 3: 4 then 5
      val graph1 = Graph(1~3, 2~3)
      val graph2 = Graph(3~4, 3~5)

      When("merging the two embeddings")
      val result: Embedding[Int] = merger.merge(Map(embedding1 -> graph1, embedding2 -> graph2))

      Then("vertex 3 should have the edges 1, 2 adjacent to each other in that order")
      val emb3 = result.embeddingFor(3)
      val edge1: AdjacencyEntry[Int] = emb3.entryFor(1)
      val edge2: AdjacencyEntry[Int] = emb3.entryFor(2)
      edge1.next should equal (edge2)
      edge2.previous should equal (edge1)

      And("vertex 3 should have the edges 4, 5 adjacent to each other in that order")
      val edge4: AdjacencyEntry[Int] = emb3.entryFor(4)
      val edge5: AdjacencyEntry[Int] = emb3.entryFor(5)
      edge4.next should equal (edge5)
      edge5.previous should equal (edge4)

    }

    it ("should return the same embedding when attempting to merge a single embedding") {

      Given("an embedding")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2)
      val graph1 = Graph(1~2)

      When("merging the two embeddings")
      val result: Embedding[Int] = merger.merge(Map(embedding1 -> graph1))

      Then("the result should be the same embedding as the one used for input")
      result should equal (embedding1)

    }

    it ("should throw an exception when attempting to merge an empty map") {

      Given("an empty map")
      val empty = Map[Embedding[Int], Graph[Int, UnDiEdge]]()

      When("merging the two embeddings")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        val result: Embedding[Int] = merger.merge(empty)
      }
    }

    /**
     * Mainly to ensure that merging works when there's an embedding that doesn't lie at the start or end of the
     * list of embeddings, which only happen when there's 3 or more.
     */
    it ("should preserve the order of edges added when 3 or more embeddings are merged") {

      Given("three embeddings sharing a single vertex (1)")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3, None, Some(2))
      val graph1 = Graph(1~2, 1~3)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 4).embed(1, 5, None, Some(4))
      val graph2 = Graph(1~4, 1~5)
      val embedding3 = UndirectedEmbedding[Int]().embed(1, 6).embed(1, 7, None, Some(6))
      val graph3 = Graph(1~6, 1~7)
      val mapping = Map(embedding1 -> graph1, embedding2 -> graph2, embedding3 -> graph3)

      When("merging the two embeddings")
      val result: Embedding[Int] = merger.merge(mapping)

      Then("the vertex 1 should have 6 edges embedded onto it")
      val emb1 = result.embeddingFor(1)
      emb1.size should be (6)

      And("edge 1~3 should come after 1~2")
      val edge2 = emb1.entryFor(2)
      val edge3 = emb1.entryFor(3)
      edge2.next should be (edge3)
      edge3.previous should be (edge2)

      And("edge 1~5 should come after 1~4")
      val edge4 = emb1.entryFor(4)
      val edge5 = emb1.entryFor(5)
      edge4.next should be (edge5)
      edge5.previous should be (edge4)

      And("edge 1~7 should come efter 1~6")
      val edge6 = emb1.entryFor(6)
      val edge7 = emb1.entryFor(7)
      edge6.next should be (edge7)
      edge7.previous should be (edge6)
    }

    it ("should merge embeddings forming a chain") {

      Given("three embeddings forming a chain of components (each embedding only being connected to one other)")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2)
      val graph1 = Graph(1~2)
      val embedding2 = UndirectedEmbedding[Int]().embed(2, 3)
      val graph2 = Graph(2~3)
      val embedding3 = UndirectedEmbedding[Int]().embed(3, 4)
      val graph3 = Graph(3~4)
      val mapping = Map(embedding1 -> graph1, embedding2 -> graph2, embedding3 -> graph3)

      When("merging the three embeddings")
      val result: Embedding[Int] = merger.merge(mapping)

      Then("the embedding for vertex 1 should contain neighbor 2")
      val emb1 = result.embeddingFor(1)
      emb1.size should be (1)
      assert(emb1.containsEntryFor(2), "the vertex 1 did not contain an edge to 2")

      And("the embedding for vertex 2 should contain neighbors 1 and 3")
      val emb2 = result.embeddingFor(2)
      emb2.size should be (2)
      assert(emb2.containsEntryFor(1), "the vertex 2 did not contain an edge to 1")
      assert(emb2.containsEntryFor(3), "the vertex 2 did not contain an edge to 3")

      And("the embedding for vertex 3 should contain neighbors 2 and 4")
      val emb3 = result.embeddingFor(3)
      emb3.size should be (2)
      assert(emb3.containsEntryFor(2), "the vertex 3 did not contain an edge to 2")
      assert(emb3.containsEntryFor(4), "the vertex 3 did not contain an edge to 4")

      And("the embedding for vertex 4 should contain neighbor 3")
      val emb4 = result.embeddingFor(4)
      emb4.size should be (1)
      assert(emb4.containsEntryFor(3), "the vertex 4 did not contain an edge to 3")

    }

  }

}
