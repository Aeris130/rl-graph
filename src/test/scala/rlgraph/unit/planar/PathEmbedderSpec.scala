package rlgraph.unit.planar

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.pathfinding.BFSPathfinder
import net.cyndeline.rlgraph.planar.demoucron.operation.help.PathEmbedder
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class PathEmbedderSpec extends SpecImports {
  val pathEmbedder = new PathEmbedder[Int]()

  describe("PathEmbedder") {

    it ("should embed a cycle into an embedding with no previous vertices belonging to the cycle") {

      Given("a cycle and an empty embedding")
      val cycle = Vector(1, 2, 3, 1)
      val embedding = UndirectedEmbedding[Int]()

      When("embedding the cycle")
      val embeddingWithCycle = pathEmbedder.embedCycle(cycle, embedding)

      Then("the new embedding should contain all three vertices of the cycle")
      assert(embeddingWithCycle.embeddingFor(1).isDefined, "The vertex 1 was not embedded.")
      assert(embeddingWithCycle.embeddingFor(2).isDefined, "The vertex 2 was not embedded.")
      assert(embeddingWithCycle.embeddingFor(3).isDefined, "The vertex 3 was not embedded.")

      And("vertex 1 should have edges to 2 and 3 embedded")
      val emb1 = embeddingWithCycle.embeddingFor(1)
      assert(emb1.containsEntryFor(2), "Vertex 1 did not contain vertex 2 as a neighbor.")
      assert(emb1.containsEntryFor(3), "Vertex 1 did not contain vertex 3 as a neighbor.")

      And("vertex 2 should have edges to 1 and 3 embedded")
      val emb2 = embeddingWithCycle.embeddingFor(2)
      assert(emb2.containsEntryFor(1), "Vertex 2 did not contain vertex 1 as a neighbor.")
      assert(emb2.containsEntryFor(3), "Vertex 2 did not contain vertex 3 as a neighbor.")

      And("vertex 3 should have edges to 2 and 1 embedded")
      val emb3 = embeddingWithCycle.embeddingFor(3)
      assert(emb3.containsEntryFor(2), "Vertex 3 did not contain vertex 2 as a neighbor.")
      assert(emb3.containsEntryFor(1), "Vertex 3 did not contain vertex 1 as a neighbor.")

    }

    it ("should embed a path with a single internal vertex inside a face") {

      Given("a path 2, 5, 3 and a face containing the start- and stop vertex of the path and an embedding containing the face")
      val g = Graph(2~5, 5~3)
      val path = BFSPathfinder().computePath(2, 3, g).get
      val face = new Face(Vector(1, 2, 3, 4))
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 1)

      When("embedding the path")
      val embeddingWithPath = pathEmbedder.embedPath(path, face, embedding)

      Then("vertex 5 should have 1 and 3 as neighbors")
      val emb5 = embeddingWithPath.embeddingFor(5)
      emb5.size should be (2)
      assert(emb5.containsEntryFor(2), "Vertex 5 didn't have 1 embedded as neighbor.")
      assert(emb5.containsEntryFor(3), "Vertex 5 didn't have 3 embedded as neighbor.")

      And("vertex 3 should have the edge 5~3 embedded after the entry for edge 4")
      val emb3 = embeddingWithPath.embeddingFor(3)
      emb3.size should be (3)
      val neighbor4Entry = emb3.entryFor(4)
      val neighbor5EntryIn3 = emb3.entryFor(5)

      neighbor4Entry.next should equal (neighbor5EntryIn3)
      neighbor5EntryIn3.previous should equal (neighbor4Entry)

      And("vertex 2 should have the edge 2~5 embedded after the entry for edge 3")
      val emb2 = embeddingWithPath.embeddingFor(2)
      emb2.size should be (3)
      val neighbor3Entry = emb2.entryFor(3)
      val neighbor5EntryIn2 = emb2.entryFor(5)

      neighbor3Entry.next should equal (neighbor5EntryIn2)
      neighbor5EntryIn2.previous should equal (neighbor3Entry)

    }

    it ("should embed a path with more than one internal vertices") {

      Given("a path 5, 2, 6, 1 and a face 1, 5, 3, 4")
      val g = Graph(5~2, 2~6, 6~1)
      val path = BFSPathfinder().computePath(5, 1, g).get
      val face = new Face(Vector(1, 5, 3, 4))
      val embedding = UndirectedEmbedding[Int]().embed(1, 5).embed(1, 4, None, Some(5)).embed(4, 3, None, Some(1)).embed(5, 3, Some(4), Some(1))

      When("embedding the path")
      val embeddingWithPath = pathEmbedder.embedPath(path, face, embedding)

      Then("the edge to 2 in 5 should come after the edge to 3")
      val emb5 = embeddingWithPath.embeddingFor(5)
      val entryFor3In5 = emb5.entryFor(3)
      val entryFor2In5 = emb5.entryFor(2)

      entryFor3In5.next should equal (entryFor2In5)
      entryFor2In5.previous should equal (entryFor3In5)

    }

    it ("should embed a path with no internal vertices") {

      Given("a path that only contains vertices on the face, a face 1, 5, 3, 4")
      val g = Graph(7~1)
      val path = BFSPathfinder().computePath(7, 1, g).get
      val face = new Face(Vector(1, 5, 8, 7, 6, 3, 4))

      /* Use a long face so that neither end of the path shares adjacent edges.
       * Useful for triggering errors by detecting non-existent vertices that otherwise would just be
       * incorrectly positioned.
       */
      val embedding = UndirectedEmbedding[Int]()
        .embed(1, 5)
        .embed(1, 4, None, Some(5))
        .embed(4, 3, None, Some(1))
        .embed(3, 6, None, Some(4))
        .embed(6, 7, None, Some(3))
        .embed(5, 8, None, Some(1))
        .embed(8, 7, Some(6), Some(5))

      When("embedding the path")
      val embeddingWithPath = pathEmbedder.embedPath(path, face, embedding)

      Then("the vertices 7 and 1 should have the edge 7~1 between them")
      val emb1 = embeddingWithPath.embeddingFor(1)
      val emb7 = embeddingWithPath.embeddingFor(7)

      assert(emb1.containsEntryFor(7), "Vertex 1 didn't have an edge to 7")
      assert(emb7.containsEntryFor(1), "Vertex 7 didn't have an edge to 1")

      And("the vertex 1 should have the edge to 7 placed after its edge to 5")
      val edgeTo7In1 = emb1.entryFor(7)
      val edgeTo5In1 = emb1.entryFor(5)

      edgeTo5In1.next should be (edgeTo7In1)
      edgeTo7In1.previous should be (edgeTo5In1)

      And("the vertex 7 should have the edge to 1 placed after its edge to 6")
      val edgeTo1In7 = emb7.entryFor(1)
      val edgeTo6In7 = emb7.entryFor(6)

      edgeTo6In7.next should be (edgeTo1In7)
      edgeTo1In7.previous should be (edgeTo6In7)

    }
  }
}
