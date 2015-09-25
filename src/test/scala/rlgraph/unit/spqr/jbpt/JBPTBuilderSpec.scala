package rlgraph.unit.spqr.jbpt

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.spqr.jbpt.{JBPTBuilder, OrderedEdge}
import net.cyndeline.rlgraph.spqr.{Edge, Parallel, Rigid, SPQRTree}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class JBPTBuilderSpec extends SpecImports {
  private val builder = new JBPTBuilder[Int, UnDiEdge]()

  private def rootSelection(vertices: Int*)(candidates: Array[Set[Int]]): Int = {
    var i = 0
    val vs = vertices.toSet
    while (i < candidates.size) {
      if (candidates(i) == vs)
        return i
      else
        i += 1
    }

    fail("No root candidate found in : " + candidates.mkString(", "))
  }

  /** Example from: http://en.wikipedia.org/wiki/SPQR_tree */
  private def wikiTree: Graph[Int, UnDiEdge] = {
    Graph(1~2, 1~4, 1~3, 2~6, 2~5, 5~8, 5~4, 4~7, 8~6, 8~7, 7~3, 6~9, 6~10, 9~11, 9~10, 10~12, 11~13,
      11~12, 12~13, 3~14, 14~13, 14~15, 14~16, 13~16, 13~15, 15~16)
  }

  private def randomizerTestGraph = new {
    val component1 = Graph(1~2, 1~3, 2~3, 1~4, 3~4, 3~5)
    val component2 = Graph(4~6, 4~7, 5~7, 5~8, 7~6, 7~8, 6~8)

    // Root making the component 1,2,3,4,5 an inner parent
    val root = Graph(0~1, 0~2, 1~2)

    // The inner parent only has one child though, this adds another one
    val secondChildComponent = Graph(2~10, 2~9, 10~9, 9~11, 10~11, 5~11, 5~10)

    // The vertices 2 and 5 originally has an edge between them, remove it to test if an edge is added
    val graph = component1 ++ component2 ++ root ++ secondChildComponent
  }

  describe("JBPTBuilder") {

    it ("should produce a tree from a graph with a single triconnected component") {

      Given("a triangular graph")
      val graph = Graph(1~2, 2~3, 3~1)

      When("parsing the graph into an SPQR tree")
      val spqr = builder.buildTree(graph)

      Then("the tree should have size 1")
      spqr.size should be (1)

      And("the root should have the solid edges 1~2, 2~3 and 3~1")
      val e1: Edge[Int] = OrderedEdge(1, 2)
      val e2: Edge[Int] = OrderedEdge(1, 2)
      val e3: Edge[Int] = OrderedEdge(1, 2)
      spqr.root.solidEdges should contain (e1)
      spqr.root.solidEdges should contain (e2)
      spqr.root.solidEdges should contain (e3)

      And("the root should have no virtual edges")
      spqr.root.virtualEdges should be ('empty)

      And("the root should contain the vertices 1, 2 and 3")
      spqr.root.vertices should have size 3
      spqr.root.vertices should contain (1)
      spqr.root.vertices should contain (2)
      spqr.root.vertices should contain (3)

    }

    it ("should produce a tree from a graph that parses into S, P and R nodes") {

      Given("a biconnected graph")
      val graph = wikiTree

      When("parsing the graph into an SPQR tree using the component 13, 14, 15, 16 as a root")
      val spqr = builder.buildTreeWithRoot(graph, rootSelection(13, 14, 15, 16))

      Then("the root should be rigid and have the solid edges 13~14, 14~15, 14~16, 15~13, 15~16, 13~16")
      spqr.root.nodeType should be (Rigid)
      spqr.root.solidEdges should have size 5
      spqr.root.solidEdges should contain (OrderedEdge(14, 15).asInstanceOf[Edge[Int]])
      spqr.root.solidEdges should contain (OrderedEdge(14, 16).asInstanceOf[Edge[Int]])
      spqr.root.solidEdges should contain (OrderedEdge(15, 13).asInstanceOf[Edge[Int]])
      spqr.root.solidEdges should contain (OrderedEdge(15, 16).asInstanceOf[Edge[Int]])
      spqr.root.solidEdges should contain (OrderedEdge(13, 16).asInstanceOf[Edge[Int]])

      And("the root should have the virtual edge 13~14")
      spqr.root.virtualEdges should have size 1
      spqr.root.virtualEdges should contain (OrderedEdge(13, 14).asInstanceOf[Edge[Int]])

      And("the child of the root should be a bond")
      spqr.root.children should have size 1
      val child1 = spqr.root.children.head
      child1.nodeType should be (Parallel)

      And("the child should have the single solid edge 13~14")
      child1.solidEdges should have size 1
      child1.solidEdges should contain (OrderedEdge(13, 14).asInstanceOf[Edge[Int]])

      And("the child should have 2 virtual edges 13~14")
      child1.virtualEdges should have size 2
      child1.virtualEdges.count(e => e == OrderedEdge(13, 14)) should be (2)

      And("the root child should have a Series child (child 2)")
      child1.children should have size 1
      val child2 = child1.children.head

      And("child 2 should have the solid edge 3~14")
      child2.solidEdges should have size 1
      child2.solidEdges should contain (OrderedEdge(3, 14).asInstanceOf[Edge[Int]])

      And("child 2 should have the virtual edges 3~6, 6~13, 13~14")
      child2.virtualEdges should have size 3
      child2.virtualEdges should contain (OrderedEdge(3, 6).asInstanceOf[Edge[Int]])
      child2.virtualEdges should contain (OrderedEdge(6, 13).asInstanceOf[Edge[Int]])
      child2.virtualEdges should contain (OrderedEdge(13, 14).asInstanceOf[Edge[Int]])

      //Skip testing the remaining children

      And("All children should store their parent")
      child2.parent.get should be (child1)
      child1.parent.get should be (spqr.root)
      spqr.root.parent should be ('empty)

    }

    it ("should specify the cutpair between a parent and its child") {

      Given("a biconnected graph")
      val graph = wikiTree

      When("parsing the graph into an SPQR tree using the component 13, 14, 15, 16 as a root")
      val spqr = builder.buildTreeWithRoot(graph, rootSelection(13, 14, 15, 16))

      Then("the root should not have a cutpair")
      spqr.root.parentCutPair should be (None)

      And("the component 3,6,13,14 should have 13 and 14 as its cut pair")
      val pair = spqr.root.children.head.parentCutPair
      val unordered = UnorderedPair(pair.get)
      unordered should equal (UnorderedPair(13, 14))

      And("the leaf contain vertex 1 should have the pair 3, 6")
      val leaf = spqr.leaves.find(_.vertices.contains(1)).get
      UnorderedPair(leaf.parentCutPair.get) should equal (UnorderedPair(3, 6))

    }

    it ("should produce the same graph when not specifying the root") {

      Given("a graph with multiple triconnected components")
      val f = randomizerTestGraph
      import f._

      When("parsing the graph into an SPQR tree multiple times")
      Then("each tree should be identical")
      val initialTree = builder.buildTree(graph)

      for (i <- 0 to 1000) {
        val spqr = builder.buildTree(graph)
        testGraphEquality(initialTree, spqr)
      }

    }

    it ("should produce the same graph when specifying the root") {

      Given("a graph with multiple triconnected components")
      val f = randomizerTestGraph
      import f._

      When("parsing the graph into an SPQR tree multiple times using a specified root")
      Then("each tree should be identical")
      val initialTree = builder.buildTreeWithRoot(graph, rootSelection(0, 1, 2))
      for (i <- 0 to 1000) {
        val spqr = builder.buildTreeWithRoot(graph, rootSelection(0, 1, 2))
        testGraphEquality(initialTree, spqr)
      }
    }

  }

  private def testGraphEquality(a: SPQRTree[Int], b: SPQRTree[Int]) {
    a.nodes should equal (b.nodes) // Only compares id and hash

    for (n <- 0 until b.nodes.size) {
      val previousNode = a.nodes(n)
      val currentNode = b.nodes(n)

      previousNode.children should equal (currentNode.children)
      previousNode.nodeType should equal (currentNode.nodeType)
      previousNode.parent should equal (currentNode.parent)
      previousNode.parentCutPair should equal (currentNode.parentCutPair)
      previousNode.solidEdges should equal (currentNode.solidEdges)
      previousNode.virtualEdges should equal (currentNode.virtualEdges)
      previousNode.vertices should equal (currentNode.vertices)
    }

  }
}
