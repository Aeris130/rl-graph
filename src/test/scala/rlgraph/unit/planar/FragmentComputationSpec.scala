package rlgraph.unit.planar

import net.cyndeline.rlgraph.planar.demoucron.operation.help.FragmentComputation
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FragmentComputationSpec extends SpecImports {

  def fixture = new {
    val computation = new FragmentComputation[Int, UnDiEdge]()
  }

  describe("FragmentComputation") {
    val f = fixture
    import f._

    it ("should compute a single edge with contact vertices in both ends") {

      Given("a graph with a cycle and a single edge connected to two points in the cycle")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1, 2~4) // 2~4 being the edge
      val cycle = graph - 2~4

      When("computing all fragments for the graph")
      val result = computation.compute(graph, cycle)

      Then("the result should contain the single fragment entry 2~4")
      result.size should be (1)
      val frag = result(0)
      frag.component should equal (Graph(2~4))

      And("its contact vertices should be vertex 2 and 4")
      frag.contact should equal (Set(2, 4))

    }

    it ("should compute a node with multiple edges connecting to contact vertices") {

      Given("a fragment subset consisting of a vertex not in the embedded graph with two edges connecting the embedding")
      val a = (4~5)
      val b = (3~5)
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1) + a + b
      val embedding = graph - a - b - 5

      When("computing all fragments for the graph")
      val result = computation.compute(graph, embedding)

      Then("the result should contain the single fragment 4~5, 3~5")
      result.size should be (1)
      val frag = result(0)
      frag.component should equal (Graph(a, b))

      And("its contact vertices should be 3 and 4")
      frag.contact should equal (Set(3, 4))
    }

    it ("should compute multiple edges going into the same contact vertex") {

      Given("a fragment subset with two contact vertices, and one of them receiving incoming edges from multiple non-contact vertices")
      val a = (4~5)
      val b = (5~6)
      val c = (6~3)
      val d = (5~3)
      val embedding = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1)
      val graph = embedding + a + b + c + d

      When("computing all fragments for the graph")
      val result = computation.compute(graph, embedding)

      Then("the result should contain the single fragment 4~5, 5~6, 6~3, 5~3")
      result.size should be (1)
      val frag = result(0)
      frag.component should equal (Graph(a, b, c, d))

      And("its contact vertices should be 3 and 4")
      frag.contact should equal (Set(3, 4))

    }

    it ("should compute edges going from one fragment-unique node to another") {

      Given("a fragment subset with two nodes not located in the embedding")
      val a = (4~5)
      val b = (5~6)
      val c = (6~3)
      val embedding = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1)
      val graph = embedding + a + b + c

      When("computing all fragments for the graph")
      val result = computation.compute(graph, embedding)

      Then("the result should contain the single fragment 4~5, 5~6, 6~3")
      result.size should be (1)
      val frag = result(0)
      frag.component should equal (Graph(a, b, c))

      And("its contact vertices should be 3 and 4")
      frag.contact should equal (Set(3, 4))

    }

    it ("should separate fragments if there's a contact vertex acting as cut-point between them") {

      Given("a fragment subset with a contact vertex making it impossible to traverse from one node to another without crossing the vertex")
      val a = (4~5)
      val b = (3~5)
      val c = (3~6)
      val d = (2~6)
      val embedding =  Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1)
      val graph = embedding + a + b + c + d

      When("computing all fragments for the graph")
      val result = computation.compute(graph, embedding)

      Then("the result should contain two fragments")
      result.size should be (2)
      val frag1 = result(0)
      val frag2 = result(1)

      And("the two fragments combined with the embedding should make up the entire fragment section of the graph")
      (frag1.component union frag2.component) should equal (Graph(a, b, c, d))

      And("each fragment should only share the vertex 3")
      (frag1.component intersect frag2.component should equal (Graph[Int, UnDiEdge](3)))

      And("each both fragments should only share 3 as contact vertex")
      (frag1.contact intersect frag2.contact) should equal (Set(3))
    }

    it ("should return an empty fragment list if every edge in the graph is also present in the embedding") {

      Given("a graph with the same edge set as the embedded graph")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1, 2~5, 3~5)
      val embedding = graph

      When("computing all fragments for the graph")
      val result = computation.compute(graph, embedding)

      Then("the result should be empty")
      result.size should be (0)
    }
  }
}
