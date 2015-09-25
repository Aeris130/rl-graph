package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.regularEdgeLabeling.{LabelEdge, RegularEdgeLabeling}
import rlgraph.SpecImports
import rlgraph.help.{CycleOrderMatcher, RegularEdgeLabelingData}

class RegularEdgeLabelingSpec extends SpecImports {
  private val cycleMatcher = new CycleOrderMatcher()

  describe("RegularEdgeLabeling") {

    it ("should assign unique ids to different labelings") {

      Given("the REL factory object")

      When("producing two separate labelings")
      val l1 = RegularEdgeLabeling(Vector(), Vector(), null, null, null, 0, 0, 0, 0)
      val l2 = RegularEdgeLabeling(Vector(), Vector(), null, null, null, 0, 0, 0, 0)

      Then("their id's should differ")
      l1.asInstanceOf[RegularEdgeLabeling[Int]].id should not equal l2.asInstanceOf[RegularEdgeLabeling[Int]].id

    }

    it ("should add a label edge for each tuple in the input vector") {

      Given("two input vectors")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val vectors = edgesT1 ++ edgesT2

      When("constructing the REL")
      Then("each tuple entry should correspond to an edge")
      for (t <- vectors) {
        assert(labeling.edges.exists(e => e.from == t._1 && e.to == t._2), "Did not find an edge corresponding to " + t + ".")
      }

    }

    it ("should split edges between the sets T1 and T2") {

      Given("two input vectors")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the REL")
      Then("each tuple entry in T1 should correspond to an edge in its own set")
      for (t <- edgesT1) {
        assert(labeling.edgesOfT1.exists(e => e.from == t._1 && e.to == t._2), "Did not find an edge corresponding to " + t + ".")
      }

      And("each tuple entry in T2 should correspond to an edge in its own set")
      for (t <- edgesT2) {
        assert(labeling.edgesOfT2.exists(e => e.from == t._1 && e.to == t._2), "Did not find an edge corresponding to " + t + ".")
      }

      And("T1 + T2 should contain the entire edge set")
      labeling.edges.toSet should equal ((labeling.edgesOfT1 ++ labeling.edgesOfT2).toSet)

    }

    it ("should verify membership for each edge") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("checking edge membership")

      Then("every edge in T1 should register as such")
      for (e <- labeling.edgesOfT1) {
        labeling.isMemberOfT1(e) should be (true)
        labeling.isMemberOfT2(e) should be (false)
      }

      And("every edge in T2 should register as such")
      for (e <- labeling.edgesOfT2) {
        labeling.isMemberOfT1(e) should be (false)
        labeling.isMemberOfT2(e) should be (true)
      }

    }

    // Note that this test doesn't utilize a valid REL embedding, it's enough to check adjacencies
    it ("should update its embeddings when flipping an edge") {

      Given("a REL with the vertex 1 having neighbors 5, 6, 2, 4 and and vertex 3 having neighbors 1, 2, 4 with the edge 1~3 initially belonging to T1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get

      When("flipping the edge 1~3 over to T2")
      val flipped = labeling.flipEdge(edge_1_3, false)

      Then("the embedding for vertex 1 in T1 should have the adjacencies 2, west")
      val adj_1_T1 = flipped.embeddingOfT1.embeddingFor(1).toVector.map(_.adjacentVertex)
      assert(cycleMatcher.compares(adj_1_T1, Vector(2, west)))

      And("the embedding for vertex 3 in T1 should have the adjacencies east, 4")
      val adj_3_T1 = flipped.embeddingOfT1.embeddingFor(3).toVector.map(_.adjacentVertex)
      assert(cycleMatcher.compares(adj_3_T1, Vector(east, 4)))

      And("the embedding for vertex 1 in T2 should have the adjacencies north, 3, 4")
      val adj_1_T2 = flipped.embeddingOfT2.embeddingFor(1).toVector.map(_.adjacentVertex)
      assert(cycleMatcher.compares(adj_1_T2, Vector(north, 3, 4)))

      And("the embedding for vertex 3 in T2 should have the adjacency 1, 2, south")
      val adj_3_T2 = flipped.embeddingOfT2.embeddingFor(3).toVector.map(_.adjacentVertex)
      assert(cycleMatcher.compares(adj_3_T2, Vector(1, 2, 7)))

    }

    it ("should change the direction of an edge that's been flipped when specified") {

      Given("a REL with the edge 3->1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get

      When("flipping the edge and setting reverse to true")
      val flipped = labeling.flipEdge(edge_1_3, true)

      Then("no edge from 3 to 1 should exists")
      flipped.edges.count(e => e.from == 1 && e.to == 3) should be (0)

      And("one edge from 1 to 3 should exist")
      flipped.edges.count(e => e.from == 3 && e.to == 1) should be (1)

    }

    it ("should not change the direction of an edge that's been flipped when specified not to") {

      Given("a REL with the edge 3->1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get

      When("flipping the edge and setting reverse to false")
      val flipped = labeling.flipEdge(edge_1_3, false)

      Then("one edge from 3 to 1 should exists")
      flipped.edges.count(e => e.from == 1 && e.to == 3) should be (1)

      And("no edge from 1 to 3 should exist")
      flipped.edges.count(e => e.from == 3 && e.to == 1) should be (0)

    }

    it ("should change the direction of some edges, but not others") {

      Given("a REL with edges 3~1 and 4~1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get
      val edge_4_1 = labeling.edges.find(e => e.from == 4 && e.to == 1).get

      When("flipping the edges and specifying that first edge should be revered, but not the other")
      val flipped = labeling.flipEdges(Vector(edge_1_3, edge_4_1), Vector(true, false))

      Then("the edge 1~3 should be in the labeling")
      assert(flipped.edges.exists(e => e.from == 3 && e.to == 1))
      assert(!flipped.edges.exists(e => e.from == 1 && e.to == 3))

      And("the edge 4~1 should remain as it is")
      assert(flipped.edges.exists(e => e.from == 4 && e.to == 1))
      assert(!flipped.edges.exists(e => e.from == 1 && e.to == 4))

    }

    it ("should restore itself when flipping an edge twice") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get

      When("flipping an edge 1~3 twice")
      val flipped = labeling.flipEdge(edge_1_3, false)
      val flippedBack = flipped.flipEdge(edge_1_3, false)

      Then("the resulting embedding should equal the initial one")
      flippedBack should equal (labeling)

    }

    it ("should flip multiple edges") {

      Given("a REL with edges 1~3 and 4~1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge_1_3 = labeling.edges.find(e => e.from == 1 && e.to == 3).get
      val edge_4_1 = labeling.edges.find(e => e.from == 4 && e.to == 1).get

      When("flipping the edges")
      val flipped = labeling.flipEdges(Vector(edge_1_3, edge_4_1), Vector(false, false))

      Then("the flipped versions of the original edges should belong to a different T-set")
      flipped.isMemberOfT1(edge_1_3) should not equal labeling.isMemberOfT1(edge_1_3)
      flipped.isMemberOfT2(edge_1_3) should not equal labeling.isMemberOfT2(edge_1_3)
      flipped.isMemberOfT1(edge_4_1) should not equal labeling.isMemberOfT1(edge_4_1)
      flipped.isMemberOfT2(edge_4_1) should not equal labeling.isMemberOfT2(edge_4_1)

    }

    // This shouldn't happen in a valid REL, but it might temporarily be the case when flipping multiple edges
    it ("should flip an edge to a T-set when one of the vertices doesn't contain any edges from that set") {

      Given("a REL with all edges in the set T1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val allEdgesInT1 = labeling.flipEdges(labeling.edgesOfT2, labeling.edgesOfT2.map(e => false))

      When("flipping the edge 1, 3 back to T2")
      val edge_1_3 = allEdgesInT1.edges.find(e => e.from == 1 && e.to == 3).get
      val flippedBack = allEdgesInT1.flipEdge(edge_1_3, false)

      Then("the edge 3,1 in the flipped back labeling should belong to T2")
      flippedBack.isMemberOfT2(edge_1_3) should be (true)

    }

    it ("should flip edges using their indices") {

      Given("a REL with the edge 1~3 belonging to T1")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val edge = labeling.edges.find(e => e.from == 1 && e.to == 3).get

      When("flipping the edge using its id")
      val flipped = labeling.flipIndexEdges(Vector(edge.index), Vector(false))

      Then("the edge should belong to T2")
      flipped.isMemberOfT2(edge) should be (true)

    }

    it ("should throw an exception when attempting to access data with an edge having an index higher than the amount of edges in it") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("passing an edge with index < 0 to its methods")
      val edge = LabelEdge(1, 3, labeling.edges.size)

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        labeling.isMemberOfT1(edge)
      }
      intercept[IllegalArgumentException] {
        labeling.isMemberOfT2(edge)
      }
      intercept[IllegalArgumentException] {
        labeling.flipEdge(edge, false)
      }

    }

    it ("should throw an exception when attempting to access data with an edge having different vertices than the one with the same index in the embedding") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("passing an edge to its methods having different vertices than the REL edge with the same index")
      val originalEdge = labeling.edges.head
      val edge = LabelEdge(originalEdge.from + 1, originalEdge.to + 1, originalEdge.index)

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        labeling.isMemberOfT1(edge)
      }
      intercept[IllegalArgumentException] {
        labeling.isMemberOfT2(edge)
      }
      intercept[IllegalArgumentException] {
        labeling.flipEdge(edge, false)
      }

    }

  }
}
