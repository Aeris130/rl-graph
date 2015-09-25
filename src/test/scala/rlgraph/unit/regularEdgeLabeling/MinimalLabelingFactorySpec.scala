package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.regularEdgeLabeling.LabelEdge
import net.cyndeline.rlgraph.regularEdgeLabeling.factories.MinimalLabelingFactory
import rlgraph.SpecImports

class MinimalLabelingFactorySpec extends SpecImports {
  private val labelingFactory = new MinimalLabelingFactory()

  // Four vertices 1, 2, 3, 4, with an edge 4, 2
  private def minimalEmbedding = new {
    val north = 11
    val west = 22
    val south = 33
    val east = 44

    val embedding = UndirectedEmbedding()
      .embedEdge(Vertex(north) withDefaultPositionInVertex east withDefaultInsertPosition)
      .embedEdge(Vertex(east) withDefaultPositionInVertex south withDefaultInsertPosition)
      .embedEdge(Vertex(south) withDefaultPositionInVertex west withDefaultInsertPosition)
      .embedEdge(Vertex(west) withDefaultPositionInVertex north withDefaultInsertPosition)
      .embedEdge(Vertex(1) withInsertPosition north inVertex west withDefaultInsertPosition)
      .embedEdge(Vertex(4) withInsertPosition 1 inVertex west withDefaultInsertPosition)
      .embedEdge(Vertex(2) withInsertPosition south inVertex east withDefaultInsertPosition)
      .embedEdge(Vertex(3) withInsertPosition south inVertex east withDefaultInsertPosition)
      .embedEdge(Vertex(4) withInsertPosition west inVertex south withDefaultInsertPosition)
      .embedEdge(Vertex(3) withInsertPosition 4 inVertex south withDefaultInsertPosition)
      .embedEdge(Vertex(1) withInsertPosition east inVertex north withDefaultInsertPosition)
      .embedEdge(Vertex(2) withInsertPosition east inVertex north withDefaultInsertPosition)
      .embedEdge(Vertex(1) withInsertPosition west inVertex 4 withInsertPosition north)
      .embedEdge(Vertex(2) withInsertPosition 1 inVertex 4 withInsertPosition east)
      .embedEdge(Vertex(1) withInsertPosition 4 inVertex 2 withInsertPosition north)
      .embedEdge(Vertex(4) withInsertPosition south inVertex 3 withInsertPosition 2)
      .embedEdge(Vertex(2) withInsertPosition 4 inVertex 3 withInsertPosition east)
  }

  // The embedding seen in fig. 10 in the article that the algorithm is based upon.
  private def articleEmbedding = new {
    val north = 11
    val west = 22
    val south = 33
    val east = 44

    val embedding = UndirectedEmbedding()
      .embed(north, 3).embed(north, 2).embed(north, 1).embed(west, 1).embed(west, 7)
      .embed(south, 7).embed(south, 8).embed(south, 9)
      .embed(5, 4).embed(5, 2).embed(5, 6).embed(5, 10).embed(5, 9).embed(5, 8)
      .embedEdge(Vertex(1) withInsertPosition west inVertex 7 withInsertPosition north)
      .embedEdge(Vertex(1) withDefaultPositionInVertex 4 withInsertPosition north)
      .embedEdge(Vertex(1) withInsertPosition 5 inVertex 2 withInsertPosition north)
      .embedEdge(Vertex(7) withInsertPosition 5 inVertex 4 withInsertPosition 1)
      .embedEdge(Vertex(7) withInsertPosition 8 inVertex 5 withInsertPosition 4)
      .embedEdge(Vertex(2) withInsertPosition 5 inVertex 6 withInsertPosition north)
      .embedEdge(Vertex(2) withInsertPosition north inVertex 3 withInsertPosition north)
      .embedEdge(Vertex(7) withInsertPosition south inVertex 8 withInsertPosition 5)
      .embedEdge(Vertex(8) withInsertPosition south inVertex 9 withInsertPosition 5)
      .embedEdge(Vertex(2) withInsertPosition 1 inVertex 4 withInsertPosition 5)
      .embedEdge(Vertex(9) withDefaultPositionInVertex 10 withInsertPosition 5)
      .embedEdge(Vertex(10) withInsertPosition 2 inVertex 6 withInsertPosition 5)
      .embedEdge(Vertex(6) withInsertPosition north inVertex 3 withInsertPosition 2)
      .embedEdge(Vertex(east) withInsertPosition 10 inVertex 9 withDefaultInsertPosition)
      .embedEdge(Vertex(east) withInsertPosition 6 inVertex 10 withInsertPosition 9)
      .embedEdge(Vertex(east) withInsertPosition 3 inVertex 6 withInsertPosition 10)
      .embedEdge(Vertex(east) withInsertPosition north inVertex 3 withInsertPosition 6)
      .embedEdge(Vertex(west) withInsertPosition 1 inVertex north withInsertPosition 7)
      .embedEdge(Vertex(north) withInsertPosition 3 inVertex east withInsertPosition west)
      .embedEdge(Vertex(east) withInsertPosition 9 inVertex south withInsertPosition north)
      .embedEdge(Vertex(south) withInsertPosition 7 inVertex west withInsertPosition east)

  }

  describe("MinimalLabelingFactory") {

    it ("should assign edge sets for an embedding") {

      Given("a minimal embedding with a 4-cycle 1, 2, 3, 4 with an edge 2, 4")
      val f = minimalEmbedding
      import f._

      When("computing a minimal REL")
      val rel = labelingFactory.produceRegularEdgeLabeling(embedding, north, south, west, east)
      val t1Edges = edgeToTuple(rel.edgesOfT1)
      val t2Edges = edgeToTuple(rel.edgesOfT2)

      Then("edges connecting neighbors of north and south should be in T1")
      t1Edges should contain (south, 3)
      t1Edges should contain (south, 4)
      t1Edges should contain (1, north)
      t1Edges should contain (2, north)

      And("edges connecting neighbors of west and east should be in T2")
      t2Edges should contain (west, 1)
      t2Edges should contain (west, 4)
      t2Edges should contain (2, east)
      t2Edges should contain (3, east)

      And("the edges 4, 1 and 3, 2 should be in T1")
      t1Edges should contain (4, 1)
      t1Edges should contain (3, 2)

      And("the edges 1, 2 and 4, 3 should be in T2")
      t2Edges should contain (1, 2)
      t2Edges should contain (4, 3)

      And("the diagonal edge 4, 2 should be in T1, making the cycle minimal")
      t1Edges should contain (4, 2)

    }

    it ("should compute the example figure") {

      Given("the embedding given in the algorithm article")
      val f = articleEmbedding
      import f._

      When("computing a minimal REL")
      val rel = labelingFactory.produceRegularEdgeLabeling(embedding, north, south, west, east)
      val t1Edges = edgeToTuple(rel.edgesOfT1)
      val t2Edges = edgeToTuple(rel.edgesOfT2)

      Then("T1 should contain red edges")
      t1Edges should contain (south, 7)
      t1Edges should contain (south, 8)
      t1Edges should contain (south, 9)
      t1Edges should contain (7, 1)
      t1Edges should contain (7, 4)
      t1Edges should contain (7, 5)
      t1Edges should contain (8, 5)
      t1Edges should contain (9, 10)
      t1Edges should contain (4, 2)
      t1Edges should contain (5, 6)
      t1Edges should contain (10, 6)
      t1Edges should contain (6, 3)
      t1Edges should contain (1, north)
      t1Edges should contain (2, north)
      t1Edges should contain (3, north)

      And("T2 should contain blue edges")
      t2Edges should contain (west, 1)
      t2Edges should contain (west, 7)
      t2Edges should contain (1, 2)
      t2Edges should contain (1, 4)
      t2Edges should contain (7, 8)
      t2Edges should contain (2, 3)
      t2Edges should contain (2, 6)
      t2Edges should contain (2, 5)
      t2Edges should contain (4, 5)
      t2Edges should contain (8, 9)
      t2Edges should contain (5, 10)
      t2Edges should contain (5, 9)
      t2Edges should contain (3, east)
      t2Edges should contain (6, east)
      t2Edges should contain (10, east)
      t2Edges should contain (9, east)

    }

    it ("should not add the outer edges to T-sets") {

      Given("an embedding")
      Given("the embedding given in the algorithm article")
      val f = articleEmbedding
      import f._

      When("computing a minimal REL")
      val rel = labelingFactory.produceRegularEdgeLabeling(embedding, north, south, west, east)
      val t1Edges = edgeToTuple(rel.edgesOfT1)
      val t2Edges = edgeToTuple(rel.edgesOfT2)

      Then("no edge should be found that contains two outer vertices")
      val outer = Set(north, south, west, east)
      assert(!(t1Edges ++ t2Edges).exists(e => outer.contains(e._1) && outer.contains(e._2)))

    }

  }

  private def edgeToTuple[V](edges: Vector[LabelEdge[V]]): Vector[(V, V)] = edges.map(e => (e.from, e.to))

}
