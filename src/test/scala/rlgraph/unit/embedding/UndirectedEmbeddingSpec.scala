package rlgraph.unit.embedding

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, AdjacencyList, Vertex}
import rlgraph.SpecImports

import scalax.collection.GraphPredef._

class UndirectedEmbeddingSpec extends SpecImports {

  private def fixture = new {
    val e = UndirectedEmbedding[Int]().embed(1).embed(2).embed(3).embed(4)
    val embedding = e.embed(1, 2).embed(3, 4)
  }

  describe("UndirectedEmbedding") {

    it ("should embed a single edge") {

      Given("an empty embedding")
      val emptyEmbedding = UndirectedEmbedding[Int]()

      When("embedding the first edge")
      val result = emptyEmbedding.embed(1, 2)

      Then("Both vertices should be embedded")
      val entry1: AdjacencyList[Int] = result.embeddingFor(1)
      val entry2: AdjacencyList[Int] = result.embeddingFor(2)

      assert(entry1.isDefined, "Vertex 1 wasn't embedded")
      assert(entry2.isDefined, "Vertex 2 wasn't embedded")

      And("both vertices should have a link to the other vertex")
      entry1.head.adjacentVertex should be (2)
      entry2.head.adjacentVertex should be (1)
    }

    it ("should embed a single vertex") {

      Given("an empty embedding")
      val emptyEmbedding = UndirectedEmbedding[Int]()

      When("embedding a vertex")
      val result = emptyEmbedding.embed(1)

      Then("the vertex should have an empty adjacency list")
      val entry1 = result.embeddingFor(1)
      assert(entry1.isEmpty)

      And("an iterator should produce an empty adjacency list")
      val itEntry = result.iterator.next()._2
      assert(itEntry.isEmpty)

    }

    it ("should confirm weather or not a vertex is embedded") {

      Given("an empty embedding and one with a vertex")
      val empty = UndirectedEmbedding[Int]()
      val withVertex = empty.embed(1)

      When("checking if they contain a vertex")
      Then("the empty embedding should respond false")
      empty.isEmbedded(1) should be (false)

      And("the vertex embedding should respond true")
      withVertex.isEmbedded(1) should be (true)

    }

    it ("should confirm that it is empty") {

      Given("an embedding with no edges")
      val embedding = UndirectedEmbedding[Int]()

      When("checking if the embedding is empty")
      val empty = embedding.isEmpty

      Then("the result should be true")
      assert(empty, "the empty embedding did not confirm that it was empty")

    }

    it ("should confirm that it is not empty") {

      Given("an embedding with at least one edge")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("checking if the embedding is empty")
      val empty = embedding.isEmpty

      Then("the result should be false")
      assert(!empty, "the empty embedding did not confirm that it was non-empty")

    }

    it ("should yield an empty iterator") {

      Given("an empty embedding")
      val embedding = UndirectedEmbedding[Int]()

      When("producing an iterator")
      val it = embedding.iterator

      Then("the iterator should be empty")
      it should be ('empty)

    }

    it ("should link a single adjacency entry to itself") {

      Given("an empty embedding")
      val emptyEmbedding = UndirectedEmbedding[Int]()

      When("embedding the first edge")
      val result = emptyEmbedding.embed(1, 2)

      Then("The doubly linked entry in each vertex should point to itself")
      val entry1: AdjacencyEntry[Int] = result.embeddingFor(1).head
      val entry2: AdjacencyEntry[Int] = result.embeddingFor(2).head

      entry1.next should be (entry1)
      entry1.previous should be (entry1)
      entry2.next should be (entry2)
      entry2.previous should be (entry2)
    }

    it ("should embed an edge between two present vertices at the end of an adjacency list if no position is specified") {

      Given("an embedding with edges (1~2) and (3~4) present")
      val f = fixture
      import f._

      When("embedding another edge between two vertices without specifying position in the embedding")
      val result = embedding.embed(1, 3)

      Then("the link to each vertex should be inserted at the end of the others adjacency list")
      val entry1: AdjacencyEntry[Int] = result.embeddingFor(1).head // 2
      val entry3: AdjacencyEntry[Int] = result.embeddingFor(3).head // 4

      /* 2 <-> 3 */
      entry1.next.adjacentVertex should be (3)

      /* 4 <-> 1 */
      entry3.next.adjacentVertex should be (1)

      And("the new entries should point to the beginning of the list")
      entry1.next.next should be (entry1) // 1 > 3 > 1
      entry3.next.next should be (entry3) // 3 > 1 > 3
    }

    it ("should embed an edge between a present vertex and a new one") {

      Given("an embedding with a vertex 1 present")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("embedding an edge from a present vertex to a new one")
      val result = embedding.embed(1, 3) // 3 not present

      Then("the new edge should be embedded in the old one")
      val entry1: AdjacencyEntry[Int] = result.embeddingFor(1).head
      entry1.previous.adjacentVertex should be (3) // Fetch the last item in the list, since the element should be put at the end
      entry1.next.adjacentVertex should be (3)

      And("the new vertex should only have a single edge (1) in it")
      val entry3: AdjacencyEntry[Int] = result.embeddingFor(3).head
      entry3.adjacentVertex should be (1)
      entry3.next.adjacentVertex should be (1)
      entry3.previous.adjacentVertex should be (1)

    }

    it ("should embed an edge between two present ones") {

      Given("an embedding with two edges (2, 3) embedded onto a vertex in the order 2 -> 3")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3)

      When("embedding an edge to 4 after 2")
      val result = embedding.embed(1, 4, None, Option(2))

      Then("The entry of 2 should have 4 as its next entry")
      val entry2: AdjacencyEntry[Int] = result.embeddingFor(1).head // 2
      entry2.next.adjacentVertex should be (4)

      And("the entry of 4 should have 2 as its previous entry, and 3 as its next")
      val entry4: AdjacencyEntry[Int] = entry2.next
      entry4.previous.adjacentVertex should be (2)
      entry4.next.adjacentVertex should be (3)

      And("the entry of 3 should have 4 as its previous entry")
      val entry3 = entry4.next
      entry3.previous.adjacentVertex should be (4)

    }

    it ("should store a list of edge tuples") {

      Given("an embedding with two edges (1,2) and (2, 3) embedded onto a vertex")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3)

      When("retrieving the edge list")
      val edges = embedding.edges

      Then("the list should contain (1,2) and (2,3)")
      edges.size should be (2)
      edges should contain ((1, 2))
      edges should contain ((2, 3))

    }

    it ("should link two entries of an edge, allowing traversal between them") {

      Given("an embedding with two vertices connected by an edge")
      val embedding = UndirectedEmbedding[String]().embed("A", "B")

      Then("the entries of each vertex A, present in the other vertex B, should contain a link back to A")
      val entryB = embedding.embeddingFor("A").head
      val entryA = embedding.embeddingFor("B").head
      entryB.moveTo.adjacentVertex should be ("A")
      entryA.moveTo.adjacentVertex should be ("B")
    }

    it ("should link entries in a circuit") {

      Given("an embedding of the curcuit 1-2-3")
      val triangleEmbedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(1, 3)

      When("checking adjacencies for each vertex")
      val entry1 = triangleEmbedding.embeddingFor(1)
      val entry2 = triangleEmbedding.embeddingFor(2)
      val entry3 = triangleEmbedding.embeddingFor(3)

      Then("vertex 1 should have movement entries linked")
      val e_1_2 = entry1.entryFor(2)
      val e_1_3 = entry1.entryFor(3)
      e_1_2.previous should be (e_1_3)
      e_1_2.next should be (e_1_3)
      e_1_3.previous should be (e_1_2)
      e_1_3.next should be (e_1_2)

      And("vertex 2 should have movement entries linked")
      val e_2_1 = entry2.entryFor(1)
      val e_2_3 = entry2.entryFor(3)
      e_2_1.previous should be (e_2_3)
      e_2_1.next should be (e_2_3)
      e_2_3.previous should be (e_2_1)
      e_2_3.next should be (e_2_1)

      And("vertex 3 should have movement entries linked")
      val e_3_1 = entry3.entryFor(1)
      val e_3_2 = entry3.entryFor(2)
      e_3_1.previous should be (e_3_2)
      e_3_1.next should be (e_3_2)
      e_3_2.previous should be (e_3_1)
      e_3_2.next should be (e_3_1)

    }

    it ("should link entries when embedding multiple edges around a single vertex in varying order") {

      Given("an embedding with the ordering 1, 3, 5, 4 around vertex 2, embedding in the order 1, 3, 4, 5")
      val embedding = UndirectedEmbedding[Int]()
        .embed(1, 2)
        .embed(1, 3, None, Option(2))
        .embed(2, 3, Option(1), Option(1))
        .embed(2, 4, None, Option(3))
        .embed(2, 5, None, Option(3))

      When("checking circular links around each vertex")
      val e_2_1 = embedding.embeddingFor(2).entryFor(1)
      val e_2_3 = embedding.embeddingFor(2).entryFor(3)
      val e_2_4 = embedding.embeddingFor(2).entryFor(4)
      val e_2_5 = embedding.embeddingFor(2).entryFor(5)

      Then("vertex 1 should be between 3 and 4")
      e_2_1.next.adjacentVertex should be (3)
      e_2_1.previous.adjacentVertex should be (4)

      And("vertex 3 should be between 1 and 5")
      e_2_3.next.adjacentVertex should be (5)
      e_2_3.previous.adjacentVertex should be (1)

      And("vertex 4 should be between 5 and 1")
      e_2_4.next.adjacentVertex should be (1)
      e_2_4.previous.adjacentVertex should be (5)

      And("vertex 5 should be between 3 and 4")
      e_2_5.next.adjacentVertex should be (4)
      e_2_5.previous.adjacentVertex should be (3)

    }

    it ("should return itself when attempting to embed an already existing edge") {

      Given("an embedding with an edge between two vertices")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("attempting to embed the same edge again")
      val result = embedding.embed(1, 2)

      Then("the resulting embedding should be the same instance as the embed call was made on")
      assert(result eq embedding, "The resulting embedding was a different object instance")
    }

    it ("should create new edge entries when creating new embeddings of an old one") {

      Given("an embedding with an edge between two vertices")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("embedding a new edge")
      val result = embedding.embed(1, 3)

      Then("the edge entries in the new embedding should use different object instances despite pointing to the same vertex")
      val firstEmbedEntry = embedding.embeddingFor(1)
      val secondEmbedEntry = result.embeddingFor(1)

      firstEmbedEntry.vertex should equal (secondEmbedEntry.vertex)
      assert(!firstEmbedEntry.eq(secondEmbedEntry), "The second embedding did not create new copies of the already present edge entries")

    }

    it ("should throw an exception when attempting to embed an edge at a non-existent position") {

      Given("an embedding with vertex 1")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("embedding an edge after an entry that doesn't exist")

      Then("the embedding should throw an exception")
      intercept[IllegalArgumentException] {
        embedding.embed(1, 4, None, Option(3)) // Put the entry of 4 after edge 3 (doesn't exist) in the vertex 1.
      }

    }

    it ("should join two embeddings together by creating a new embedding with all entries together") {

      Given("two embeddings with edges embedded around a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 3).embed(1, 4)

      When("joining them together and specifying that edges 1~3 and 1~4 of embedding 2 should be inserted after 1~2 in vertex 1")
      val joinedEmbeding = embedding1.addEmbedding(embedding2, Map(1 -> 2))

      Then("vertex 1 should have the edges 2, 3, 4, 5 embedded in that order")
      val adjacencyListForVertex1 = joinedEmbeding.embeddingFor(1)
      val head = adjacencyListForVertex1.head

      head.adjacentVertex should be (2)
      head.next.adjacentVertex should be (3)
      head.next.next.adjacentVertex should be (4)
      head.next.next.next.adjacentVertex should be (5)

      head.previous.adjacentVertex should be (5)
      head.previous.previous.adjacentVertex should be (4)
      head.previous.previous.previous.adjacentVertex should be (3)
      head.previous.previous.previous.previous.adjacentVertex should be (2)

    }

    it ("should create correct vertex entries when performing multiple joins around multiple vertices") {

      Given("three embeddings 1,2 | 2,3 | 3,4")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2)
      val embedding2 = UndirectedEmbedding[Int]().embed(2, 3)
      val embedding3 = UndirectedEmbedding[Int]().embed(3, 4)

      When("merging them")
      val merged = embedding1.addEmbedding(embedding2, Map(2 -> 1)).addEmbedding(embedding3, Map(3 -> 2))

      Then("each vertex should have an entry")
      val e1 = merged.embeddingFor(1)
      val e2 = merged.embeddingFor(2)
      val e3 = merged.embeddingFor(3)
      val e4 = merged.embeddingFor(4)

      e1.vertex should be (1)
      e2.vertex should be (2)
      e3.vertex should be (3)
      e4.vertex should be (4)

    }

    it ("should create correct links when performing multiple joins around a single vertex") {

      Given("three embeddings sharing a single vertex (1)")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3, None, Some(2))
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 4).embed(1, 5, None, Some(4))
      val embedding3 = UndirectedEmbedding[Int]().embed(1, 6).embed(1, 7, None, Some(6))

      When("joining them in the declared order")
      val merged = embedding1.addEmbedding(embedding2, Map(1 -> 3)).addEmbedding(embedding3, Map(1 -> 5))
      val emb1 = merged.embeddingFor(1)

      Then("vertex 2 should be between 3 and 7")
      val entry2 = emb1.entryFor(2)
      entry2.next.adjacentVertex should be (3)
      entry2.previous.adjacentVertex should be (7)

      And("vertex 3 should be between 2 and 4")
      val entry3 = emb1.entryFor(3)
      entry3.next.adjacentVertex should be (4)
      entry3.previous.adjacentVertex should be (2)

      And("vertex 4 should be between 3 and 5")
      val entry4 = emb1.entryFor(4)
      entry4.next.adjacentVertex should be (5)
      entry4.previous.adjacentVertex should be (3)

      And("vertex 5 should be between 4 and 6")
      val entry5 = emb1.entryFor(5)
      entry5.next.adjacentVertex should be (6)
      entry5.previous.adjacentVertex should be (4)

      And("vertex 6 should be between 5 and 7")
      val entry6 = emb1.entryFor(6)
      entry6.next.adjacentVertex should be (7)
      entry6.previous.adjacentVertex should be (5)

      And("vertex 7 should be between 2 and 6")
      val entry7 = emb1.entryFor(7)
      entry7.next.adjacentVertex should be (2)
      entry7.previous.adjacentVertex should be (6)

    }

    it ("should copy edge entries to new instances when joining two embeddings") {

      Given("two embeddings with edges embedded around a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5).embed(2, 7)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 3).embed(1, 4)

      When("joining them together and specifying that edges 1~3 and 1~4 of embedding 2 should be inserted after 1~2 in vertex 1")
      val joinedEmbeding = embedding1.addEmbedding(embedding2, Map(1 -> 2))

      Then("the edge entries the belong to vertices containing entries from both embeddings should not share object instances with the old entries")
      val oldAdjListForEmb1 = embedding1.embeddingFor(1)
      val oldAdjListForEmb2 = embedding2.embeddingFor(1)
      val newAdjacencyListForJoined = joinedEmbeding.embeddingFor(1)

      val oldEntryForEdge2 = oldAdjListForEmb1.entryFor(2)
      val oldEntryForEdge3 = oldAdjListForEmb2.entryFor(3)
      val newEntryForEdge2 = newAdjacencyListForJoined.entryFor(2)
      val newEntryForEdge3 = newAdjacencyListForJoined.entryFor(3)

      assert(!(oldEntryForEdge2 eq newEntryForEdge2), "The new edge entry (" + newEntryForEdge2 + ") did not use a unique object instance")
      assert(!(oldEntryForEdge3 eq newEntryForEdge3), "The new edge entry (" + newEntryForEdge3 + ") did not use a unique object instance")

      And("the edge entries from vertices only present in one embedding should not share object instances with the old entries")
      val oldAdjListForVertex2 = embedding1.embeddingFor(2)
      val newAdjListForVertex2 = joinedEmbeding.embeddingFor(2)
      val oldEntryForEdge7 = oldAdjListForVertex2.head
      val newEntryForEdge7 = newAdjListForVertex2.head

      assert(!(oldEntryForEdge7 eq newEntryForEdge7), "The new edge entry (" + newEntryForEdge7 + ") did not use a unique object instance")

    }

    it ("should arrange move links when joining two embeddings") {

      // Equality is checked using object references instead of vertex values, since the orignal entries should've been copied

      Given("two embeddings with edges embedded around a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 3).embed(1, 4)

      When("joining them together")
      val joinedEmbeding = embedding1.addEmbedding(embedding2, Map(1 -> 2))

      Then("the movement links should be maintained for the old entries")
      val vertex1AdjList = joinedEmbeding.embeddingFor(1)
      val entryForEdge1To2 = vertex1AdjList.entryFor(2)
      val vertex2AdjList = joinedEmbeding.embeddingFor(2)
      val entryForEdge2To1 = vertex2AdjList.entryFor(1)

      entryForEdge1To2.moveTo should equal (entryForEdge2To1)
      entryForEdge2To1.moveTo should equal (entryForEdge1To2)

      And("the movement links should be maintained for the new entries")
      val vertex3AdjList = joinedEmbeding.embeddingFor(3)
      val entryForEdge3To1 = vertex3AdjList.entryFor(1)
      val entryForEdge1To3 = vertex1AdjList.entryFor(3)

      entryForEdge3To1.moveTo should equal (entryForEdge1To3)
      entryForEdge1To3.moveTo should equal (entryForEdge3To1)
    }

    it ("should throw an exception when joining embeddings if every common vertex isn't specified in the position map") {

      Given("two embeddings with edges embedded around a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 3).embed(1, 4)

      When("joining them together and not supplying vertex 1 as a common vertex")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        embedding1.addEmbedding(embedding2, Map())
      }
    }

    it ("should throw an exception when joining embeddings while specifying common vertices that aren't common") {

      Given("two embeddings with edges embedded around a common vertex")
      val embedding1 = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5)
      val embedding2 = UndirectedEmbedding[Int]().embed(1, 3).embed(1, 4)

      When("joining them together and supplying vertex 5 as a common vertex")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        embedding1.addEmbedding(embedding2, Map(5 -> 1))
      }
    }

    it ("should create an embedding from an adjacency list") {

      Given("a vertex mapped to 3 other vertices")
      val map = Map(1 -> Vector(2, 3, 4),
                    2 -> Vector(1),
                    3 -> Vector(1),
                    4 -> Vector(1))

      When("embedding the map")
      val embedding = UndirectedEmbedding[Int]().embed(map)

      Then("the vertex 1 should have the edges 2, 3 and 4")
      val adjacencies = embedding.embeddingFor(1).iterator.toList.map(_.adjacentVertex)
      adjacencies should be (List(2, 3, 4))

    }

    it ("should delete an edge") {

      Given("a vertex with four edges connected to it")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(1, 4).embed(1, 5)

      When("deleting the second edge")
      val deleted = embedding.deleteEdge(1, 3)

      Then("the edge to vertex 2 should have vertex 4 and 5 around it")
      val entryFor2 = deleted.embeddingFor(1).entryFor(2)
      entryFor2.next.adjacentVertex should be (4)
      entryFor2.previous.adjacentVertex should be (5)

      And("the edge to vertex 4 should have vertex 2 and 5 around it")
      val entryFor4 = deleted.embeddingFor(1).entryFor(4)
      entryFor4.next.adjacentVertex should be (5)
      entryFor4.previous.adjacentVertex should be (2)

      And("the edge to vertex 5 should have vertex 2 and 4 around it")
      val entryFor5 = deleted.embeddingFor(1).entryFor(5)
      entryFor5.next.adjacentVertex should be (2)
      entryFor5.previous.adjacentVertex should be (4)

    }

    it ("should delete a vertex") {

      Given("an embedding with three vertices and two edges")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3)

      When("deleting vertex 1")
      val deleted = embedding.deleteVertex(1)

      Then("only the edge 2,3 should remain")
      deleted.edges.map(UnorderedPair(_)) should be (Vector(UnorderedPair(2, 3)))

      And("only the vertices 2 and 3 should remain")
      deleted.embeddedVertices.toSet should be (Set(2, 3))

      And("vertex 2 should be the neighbor of vertex 3")
      deleted.embeddingFor(2).toVector.map(_.adjacentVertex) should be (Vector(3))
      deleted.embeddingFor(3).toVector.map(_.adjacentVertex) should be (Vector(2))

    }

    it ("should reverse an adjacency list") {

      Given("a vertex with four edges connected to it")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(1, 4).embed(1, 5)

      When("reversing the embedding")
      val reversed = embedding.reverse

      Then("the edge to vertex 2 should have 3 before it and 5 after")
      val entryFor2 = reversed.embeddingFor(1).entryFor(2)
      entryFor2.next.adjacentVertex should be (5)
      entryFor2.previous.adjacentVertex should be (3)

      Then("the edge to vertex 4 should have 5 before it and 3 after")
      val entryFor4 = reversed.embeddingFor(1).entryFor(4)
      entryFor4.next.adjacentVertex should be (3)
      entryFor4.previous.adjacentVertex should be (5)

    }

    it ("should map vertices to a different type") {

      Given("an embedding with the vertices 1 and 2")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2)

      When("mapping 1 to A and 2 to B")
      val mapped = embedding.map(v => {
        if (v == 1) "A" else "B"
      })

      Then("the new embedding should contain the vertices A and B with an edge between them")
      val expected = UndirectedEmbedding[String]().embed("A", "B")
      mapped should equal (expected)

    }

    it ("should throw an exception when attempting to specify head/tail positions that doesn't exist") {

      Given("an embedding")
      val embedding = UndirectedEmbedding[Int]()

      When("attempting to embed an edge between two vertices using a non-existent head/tail positions")
      Then("an exception should be thrown")

      intercept[NoSuchElementException] {
        embedding.embed(1, 2, Option(3), None)
      }

      intercept[NoSuchElementException] {
        embedding.embed(1, 2, None, Option(3))
      }

    }

    it ("should produce a string from an embedding with a single vertex") {

      Given("an embedding with a single vertex")
      val e = UndirectedEmbedding[Int]().embed(1)

      When("producing a string")
      val str = e.toString

      Then("the string should not be empty")
      str should not be 'empty

    }

    it ("should retrieve neighbors for a vertex in the embedded order") {

      Given("a vertex with 3 neighbors embedded: 2, 4, 3")
      val embedding = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embedEdge(Vertex(1) withDefaultPositionInVertex 4 withInsertPosition 2)

      When("retrieving neighbors")
      val neighbors = embedding.neighborsOf(1)

      Then("the vertex should have neighbors 2, 4, 3")
      neighbors should be (Vector(2, 4, 3))

    }
  }
}
