package rlgraph.unit.subgraph.isomorphism

import net.cyndeline.rlgraph.subgraph.isomorphism.NegativeCondition
import net.cyndeline.rlgraph.subgraph.isomorphism.jGraphT.IsomorphismInspector
import net.cyndeline.rlgraph.subgraph.isomorphism.util.DefaultVertexEquivalence
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class IsomorphismInspectorSpec extends SpecImports {
  private val inspector = new IsomorphismInspector()
  private val defaultComparator = new DefaultVertexEquivalence[Int, UnDiEdge]()

  describe("IsomorphismInspector") {

    it ("should return true if two graphs are equal in topology") {

      Given("two graphs with edges 1~2, 2~3")
      val graph = Graph(1~2, 2~3)

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](graph, graph, defaultComparator, None, graph)

      Then("the result should be true")
      assert(result, "The graphs " + graph + " and " + graph + " were not considered isomorphic.")

    }

    it ("should return true if two graphs have the same vertices and no edges") {

      Given("two graphs with vertices 1, 2, 3")
      val graph = Graph[Int, UnDiEdge](1, 2, 3)

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](graph, graph, defaultComparator, None, graph)

      Then("the result should be true")
      assert(result, "The graphs " + graph + " and " + graph + " were not considered isomorphic.")

    }

    it ("should return false if two graphs share the same vertices, but not edges") {

      Given("a graph 1~2, 2~3 and a graph 1~2, 1~3")
      val graph1 = Graph(1~2, 2~3)
      val graph2 = Graph(1~2, 1~3)

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](graph1, graph2, defaultComparator, None, graph2)

      Then("the result should be false")
      assert(!result, "The graphs " + graph1 + " and " + graph2 + " were considered isomorphic.")

    }

    it ("should return false if two graphs share the same vertices, but one has more edges than the other") {

      Given("the graphs 1~2, 2~3 and 1~2, 2~3, 3~1")
      val graph1 = Graph(1~2, 2~3)
      val graph2 = Graph(1~2, 2~3, 3~1)

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](graph1, graph2, defaultComparator, None, graph2)

      Then("the result should be false")
      assert(!result, "The graphs " + graph1 + " and " + graph2 + " were considered isomorphic.")

    }

    it ("should return false if one graph contains the same edges as the other, as well as additional edges") {

      Given("a graph A that is a subset of graph B")
      val A = Graph(1~2)
      val B = Graph(1~2, 1~3)

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](A, B, defaultComparator, None, B)

      Then("the result should be false")
      assert(!result, "The graphs " + A + " and " + B + " were considered isomorphic.")

    }

    it ("should let the user specify custom vertex equivalence checks") {

      Given("a graph A and a graph B with vertex tuples of (String, Int), where only the strings match")
      val stringOnlyComparator = new DefaultVertexEquivalence[(String, Int), UnDiEdge]() {
        override def compares(e1: (String, Int),
                              e2: (String, Int),
                              contextForE1: Graph[(String, Int), UnDiEdge],
                              contextForE2: Graph[(String, Int), UnDiEdge]): Boolean = e1._1 == e2._1
        override def elementHash(element: (String, Int)): Int = element._1.##
      }
      val v1 = ("A", 1)
      val v2 = ("B", 2)
      val v3 = ("C", 3)

      val v4 = ("A", 4)
      val v5 = ("B", 5)
      val v6 = ("C", 6)

      val g1 = Graph[(String, Int), UnDiEdge](v1~v2, v2~v3)
      val g2 = Graph(v4~v5, v5~v6)

      When("checking if the graphs are isomorphic using a comparator that only compares strings")
      val inspector = new IsomorphismInspector()
      val result = inspector.isIsomorphic[(String, Int), UnDiEdge](g1, g2, stringOnlyComparator, None, g2)

      Then("the result should be true")
      assert(result, "The graphs " + g1 + " and " + g2 + " were not considered isomorphic.")

    }

    it ("should compute a mapping of vertices for graphs that are isomorphic") {

      Given("two graphs with edges 1~2, 2~3")
      val graph = Graph(1~2, 2~3)

      When("checking if the graphs are isomorphic")
      val result: Option[Map[Int, Int]] = inspector.isomorphicMapping[Int, UnDiEdge](graph, graph, defaultComparator, None, graph)

      Then("the result should be a mapping between the vertices")
      assert(result.isDefined, "No mapping was returned.")
      val mapping = result.get
      mapping should equal (Map(1 -> 1, 2 -> 2, 3 -> 3))

    }

    it ("should return None when computing a mapping for two non-isomorphic graphs") {

      Given("a graph 1~2, 2~3 and a graph 1~2, 1~3")
      val graph1 = Graph(1~2, 2~3)
      val graph2 = Graph(1~2, 1~3)

      When("checking if the graphs are isomorphic")
      val result = inspector.isomorphicMapping[Int, UnDiEdge](graph2, graph1, defaultComparator, None, graph2)

      Then("the result should be None")
      result should be (None)

    }

    it ("should supply the correct context for both graphs") {

      Given("a graph A with a custom equivalence that makes its vertices equal to those in B if the vertex in B does not appear in th context of A")
      val stringOnlyComparator = new DefaultVertexEquivalence[(String, Int), UnDiEdge]() {
        override def compares(e1: (String, Int),
                              e2: (String, Int),
                              contextForE1: Graph[(String, Int), UnDiEdge],
                              contextForE2: Graph[(String, Int), UnDiEdge]): Boolean = e1._1 == e2._1 && contextForE1.contains(e1) && contextForE2.contains(e2)
        override def elementHash(element: (String, Int)): Int = element._1.##
      }

      val A = Graph[(String, Int), UnDiEdge](("A", 1))
      val B = Graph[(String, Int), UnDiEdge](("A", 2))

      When("checking if A is isomorphic to B")
      val inspector = new IsomorphismInspector()
      val result = inspector.isIsomorphic[(String, Int), UnDiEdge](A, B, stringOnlyComparator, None, B)

      Then("the result should be true")
      assert(result, "The graphs " + A + " and " + B + " were not considered isomorphic.")

    }

    it ("should reject graphs that fail a negative condition") {
      class Negative extends NegativeCondition[Int, UnDiEdge] {
        override def isValid(map: Map[Int, Int], g: Graph[Int, UnDiEdge]) = {
          g.nodes.size < 3
        }
      }

      Given("two graphs with edges 1~2, 2~3 and a condition that fails if the second graph has 3+ vertices")
      val graph = Graph(1~2, 2~3)
      val negativeCondition = new Negative()

      When("checking if the graphs are isomorphic")
      val result = inspector.isIsomorphic[Int, UnDiEdge](graph, graph, defaultComparator, Some(negativeCondition), graph)

      Then("the result should be true")
      assert(!result, "The graphs " + graph + " and " + graph + " were considered isomorphic.")

    }

    it ("should select a mapping that doesn't fail a negative condition") {
      val intOnlyComparator = new DefaultVertexEquivalence[(String, Int), UnDiEdge]() {
        override def compares(e1: (String, Int),
                              e2: (String, Int),
                              contextForE1: Graph[(String, Int), UnDiEdge],
                              contextForE2: Graph[(String, Int), UnDiEdge]): Boolean = e1._2 == e2._2
        override def elementHash(element: (String, Int)): Int = element._2.##
      }

      Given("a graph with tuples ('A', 1), ('B', 1), ('C', 1) and ('D', 1) and a subgraph that matches its integer values")
      val v1 = ("A", 1)
      val v2 = ("B", 1)
      val v3 = ("C", 1)

      val v4 = ("D", 1)
      val v5 = ("E", 1)
      val v6 = ("F", 1)

      val g1 = Graph(v1~v2, v2~v3)
      val g2 = Graph(v4~v5, v5~v6)

      When("computing the isomorphic mapping using a condition that fails if the sub graph matches its integer 1 with a vertex of string D")
      val inspector = new IsomorphismInspector()
      class Negative extends NegativeCondition[(String, Int), UnDiEdge] {
        override def isValid(map: Map[(String, Int), (String, Int)], g: Graph[(String, Int), UnDiEdge]) = {
          val n1: (String, Int) = map(v1)
          n1._1 != "D"
        }
      }
      val result: Option[Map[(String, Int), (String, Int)]] = inspector.isomorphicMapping[(String, Int), UnDiEdge](g1, g2, intOnlyComparator, Some(new Negative()), g2)

      Then("the resulting mapping should be A -> F, B -> E, C -> D")
      assert(result.isDefined, "Graph " + g1 + " was not an isomorphic subgraph of " + g2)
      val mapping = result.get
      mapping(v1) should be (v6)
      mapping(v2) should be (v5)
      mapping(v3) should be (v4)

    }

  }
}
