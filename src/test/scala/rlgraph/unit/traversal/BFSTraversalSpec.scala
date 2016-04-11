package rlgraph.unit.traversal

import net.cyndeline.rlgraph.traversal.BFSTraversal
import rlgraph.SpecImports

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class BFSTraversalSpec extends SpecImports {

  def balancedTree = new {
    val e0 = 1~2
    val e1 = 1~3
    val e2 = 2~4
    val e3 = 2~5
    val e4 = 3~6
    val e5 = 3~7
    val e6 = 4~8
    val e7 = 4~9
    val e8 = 7~10
    val g = Graph[Int, UnDiEdge](e0, e1, e2, e3, e4, e5, e6, e7, e8)
  }

  describe("BFSTraversal") {

    it ("should return a single vertex and no edges if the graph contains a single vertex") {

      Given("a graph with a single vertex")
      val g = Graph[Int, UnDiEdge](1)

      When("traversing the graph starting at 1")
      val bfsTraversal = BFSTraversal[Int, UnDiEdge](g, 1)

      Then("no edges should be found")
      bfsTraversal.edges should be ('empty)

      And("one vertex should be found")
      bfsTraversal.vertices should have size 1
      bfsTraversal.vertices.head should be (1)

    }

    it ("should return two vertices and an edge") {

      Given("a graph with two connected vertices")
      val edge = 1~2
      val g = Graph[Int, UnDiEdge](edge)

      When("traversing the graph starting at 1")
      val bfsTraversal = BFSTraversal[Int, UnDiEdge](g, 1)

      Then("one edge should be found")
      bfsTraversal.edges should have size 1
      bfsTraversal.edges.head should be (edge)

      And("the vertices should be in the order 1, 2")
      bfsTraversal.vertices should have size 2
      bfsTraversal.vertices.head should be (1)
      bfsTraversal.vertices.last should be (2)

    }

    it ("should order multiple layers") {

      Given("a graph with root 1, two children of 1 (2, 3), and a child of 2 (4)")
      val e0 = 1~2
      val e1 = 1~3
      val e2 = 2~4
      val g = Graph[Int, UnDiEdge](e0, e1, e2)

      When("traversing the graph starting at 1")
      val bfsTraversal = BFSTraversal[Int, UnDiEdge](g, 1)

      Then("three edges should be found")
      val edges = bfsTraversal.edges
      edges should have size 3

      And("the first two should be 1-2 and 1-3")
      Set(edges(0), edges(1)) should be (Set(e0, e1))

      And("the third should be 2-4")
      edges(2) should be (e2)

      And("four vertices should be found")
      val vertices = bfsTraversal.vertices
      vertices should have size 4

      And("the first should be 1")
      vertices(0) should be (1)

      And("the middle two should be 2 and 3")
      Set(vertices(1), vertices(2)) should be (Set(2, 3))

      And("the last should be 4")
      vertices.last should be (4)

    }

    it ("should search down to a specified level") {

      Given("a balanced tree with 10 vertices")
      val f = balancedTree
      import f._

      When("traversing the graph starting at 1 with a max depth of 3")
      val bfsTraversal = BFSTraversal[Int, UnDiEdge](g, 1, 3)

      Then("only 7 vertices should be included")
      val vertices = bfsTraversal.vertices
      vertices should have size 7

      And("the first should be 1")
      vertices(0) should be (1)

      And("the next level should be 2 and 3")
      Set(vertices(1), vertices(2)) should be (Set(2, 3))

      And("the next level should be 4, 5, 6 and 7")
      Set(vertices(3), vertices(4), vertices(5), vertices(6)) should be (Set(4, 5, 6, 7))

      And("only 6 edges should be included")
      val edges = bfsTraversal.edges
      edges should have size 6

      And("the first two should be 1-2 and 1-3")
      Set(edges(0), edges(1)) should be (Set(e0, e1))

      And("the remaining four should be 2-4, 2-5, 3-6, 3-7")
      Set(edges(2), edges(3), edges(4), edges(5)) should be (Set(e2, e3, e4, e5))

    }

    it ("should call the visitor on edges in the order they are traversed") {

      Given("a balanced tree with 10 vertices")
      val f = balancedTree
      import f._

      When("traversing the tree starting at its root with a visitor that registers every edge")
      val visitedEdges = new ArrayBuffer[UnDiEdge[Int]]()
      def visitor(from: Int, to: Int, e: UnDiEdge[Int]): Unit = { visitedEdges += e }
      val bfsTraversal = BFSTraversal[Int, UnDiEdge](g, 1, 0, visitor _)

      Then("the visitors edges should mirror the ones in the traversal edge list")
      bfsTraversal.edges should equal (visitedEdges.toVector)

    }

    it ("should call the visitor on edges along with nodes that represent the direction the edge is traversed in") {

      Given("a graph 1-2-3 and a visitor that registers visitor data")
      val visitedEdges1 = new ArrayBuffer[(Int, Int, UnDiEdge[Int])]() // From, To, Edge
      val visitedEdges2 = new ArrayBuffer[(Int, Int, UnDiEdge[Int])]()
      def visitor1(from: Int, to: Int, e: UnDiEdge[Int]): Unit = { visitedEdges1 += ((from, to, e)) }
      def visitor2(from: Int, to: Int, e: UnDiEdge[Int]): Unit = { visitedEdges2 += ((from, to, e)) }

      val e0 = 1~2
      val e1 = 2~3
      val g = Graph[Int, UnDiEdge](e0, e1)

      When("traversing the graph twice starting at 1 and 3")
      val bfsTraversal1 = BFSTraversal[Int, UnDiEdge](g, 1, 0, visitor1 _)
      val bfsTraversal2 = BFSTraversal[Int, UnDiEdge](g, 3, 0, visitor2 _)

      Then("the first traversal should register edge data [1 -> e0 -> 2], [2 -> e1 -> 3]")
      visitedEdges1.toVector should be (Vector((1, 2, e0), (2, 3, e1)))

      And("the second traversal should register edge data [3 -> e1 -> 2], [2 -> e0 -> 1]")
      visitedEdges2.toVector should be (Vector((3, 2, e1), (2, 1, e0)))

    }

  }
}
