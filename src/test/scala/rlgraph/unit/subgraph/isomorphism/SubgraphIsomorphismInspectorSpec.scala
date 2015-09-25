package rlgraph.unit.subgraph.isomorphism

import net.cyndeline.rlgraph.subgraph.isomorphism.NegativeCondition
import net.cyndeline.rlgraph.subgraph.isomorphism.jGraphT.SubGraphIsomorphismInspector
import net.cyndeline.rlgraph.subgraph.isomorphism.util.DefaultVertexEquivalence
import rlgraph.SpecImports

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class SubgraphIsomorphismInspectorSpec extends SpecImports {
  private val subGraphInspector = new SubGraphIsomorphismInspector()
  private val defaultComparator = new DefaultVertexEquivalence[Int, UnDiEdge]()

  describe("SubgraphIsomorphismInspector") {

    it ("confirm that a sub graph is isomorphic") {

      Given("a graph 1~2, 2~3 that is isomorphic to 0~1, 1~2, 2~3, 3~4")
      val g1 = Graph(1~2, 2~3)
      val g2 = Graph(0~1, 1~2, 2~3, 3~4)

      When("computing the isomorphic mapping")
      val mapping = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](g1, g2, defaultComparator, None)

      Then("the resulting mapping should be 1->1, 2->2, 3->3")
      assert(mapping.isDefined, "Graph " + g1 + " was not an isomorphic subgraph of " + g2)
      mapping.get should be (Map(1->1, 2->2, 3->3))

    }

    it ("should confirm sub graph isomorphism for graphs without edges") {

      Given("two graphs with vertices 1, 2, 3")
      val g = Graph[Int, UnDiEdge](1, 2, 3)

      When("computing the isomorphic mapping")
      val mapping = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](g, g, defaultComparator, None)

      Then("the resulting mapping should be 1->1, 2->2, 3->3")
      assert(mapping.isDefined, "Graph " + g + " was not an isomorphic subgraph of " + g)
      mapping.get should be (Map(1->1, 2->2, 3->3))

    }

    it ("should reject two graphs that are not isomorphic") {

      Given("a graph 1~2, 2~3 and a graph 1~2, 2~4")
      val g1 = Graph(1~2, 2~3)
      val g2 = Graph(1~2, 2~4)

      When("computing the isomorphic mapping")
      val mapping = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](g1, g2, defaultComparator, None)

      Then("no mapping should be returned")
      mapping should be (None)

    }

    it ("should reject two graphs sharing the same vertices but not edges") {

      Given("a graph 1~2, 2~3, 3~4 and a graph 1~2, 2~4, 4~1, 3~2")
      val g1 = Graph(1~2, 2~3, 3~4)
      val g2 = Graph(1~2, 2~4, 4~1, 3~2)

      When("computing the isomorphic mapping")
      val mapping = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](g1, g2, defaultComparator, None)

      Then("no mapping should be returned")
      mapping should be (None)

    }

    it ("should compute isomorphism for a sub graph of a cycle") {

      Given("a graph 1~2, 2~3 as a sub graph of 1~2, 2~3, 3~1")
      val g1 = Graph(1~2, 2~3)
      val g2 = Graph(1~2, 2~3, 3~1)

      When("computing the isomorphic mapping")
      val mapping = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](g1, g2, defaultComparator, None)

      Then("the resulting mapping should be 1->1, 2->2, 3->3")
      assert(mapping.isDefined, "Graph " + g1 + " was not an isomorphic subgraph of " + g2)
      mapping.get should be (Map(1->1, 2->2, 3->3))

    }

    it ("should comply with custom equivalence") {

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
      val v7 = ("D", 7)

      val g1 = Graph[(String, Int), UnDiEdge](v1~v2, v2~v3)
      val g2 = Graph(v4~v5, v5~v6, v6~v7)

      When("computing the isomorphic mapping")
      val subGraphInspector = new SubGraphIsomorphismInspector()
      val mapping = subGraphInspector.firstIsomorphicMapping[(String, Int), UnDiEdge](g1, g2, stringOnlyComparator, None)

      Then("the resulting mapping should be v1->v4, v2->v5, v3->v6")
      assert(mapping.isDefined, "Graph " + g1 + " was not an isomorphic subgraph of " + g2)
      mapping.get should be (Map(v1->v4, v2->v5, v3->v6))

    }

    it ("should reject graphs that fail a negative condition") {
      class Negative extends NegativeCondition[Int, UnDiEdge] {
        override def isValid(map: Map[Int, Int], g: Graph[Int, UnDiEdge]) = {
          g.nodes.size < 3
        }
      }

      Given("two graphs with edges 1~2, 2~3 and a condition that fails if the second graph has 3+ vertices")
      val graph1 = Graph(1~2, 2~3)
      val graph2 = Graph(1~2, 2~3, 3~1)
      val negativeCondition = new Negative()

      When("checking if the graphs are isomorphic")
      val result = subGraphInspector.firstIsomorphicMapping[Int, UnDiEdge](graph1, graph2, defaultComparator, Some(negativeCondition))

      Then("the result should be true")
      assert(result.isEmpty, "The graphs " + graph1 + " and " + graph2 + " were considered isomorphic.")

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
      val v7 = ("G", 2)

      val g1 = Graph(v1~v2, v2~v3)
      val g2 = Graph(v4~v5, v5~v6, v6~v7)

      When("computing the isomorphic mapping using a condition that fails if the sub graph matches its integer 1 with a vertex of degree 1")
      val subGraphInspector = new SubGraphIsomorphismInspector()
      class Negative extends NegativeCondition[(String, Int), UnDiEdge] {
        override def isValid(map: Map[(String, Int), (String, Int)], g: Graph[(String, Int), UnDiEdge]) = {
          val n1: (String, Int) = map(v1)
          g.get(n1).degree > 1
        }
      }
      val result: Option[Map[(String, Int), (String, Int)]] = subGraphInspector.firstIsomorphicMapping[(String, Int), UnDiEdge](g1, g2, intOnlyComparator, Some(new Negative()))

      Then("the resulting mapping should be A -> F, B -> E, C -> D")
      assert(result.isDefined, "Graph " + g1 + " was not an isomorphic subgraph of " + g2)
      val mapping = result.get
      mapping(v1) should be (v6)
      mapping(v2) should be (v5)
      mapping(v3) should be (v4)

    }

  }

  it ("should return None if the pattern is empty") {

    Given("an empty pattern and a graph with vertices and edges in it")
    val g1 = Graph[Int, UnDiEdge]()
    val g2 = Graph[Int, UnDiEdge](1~2)

    When("computing the isomorphic mapping")
    val subGraphInspector = new SubGraphIsomorphismInspector()
    val mapping = subGraphInspector.firstIsomorphicMapping(g1, g2, defaultComparator, None)

    Then("the mapping should be empty")
    mapping should be ('empty)
    mapping should not be ('defined)

  }

  /**
   * This test doesn't actually test out-data, it only checks if the random object has had any operations performed on it.
   */
  it ("should use the random object supplied") {

    Given("a random object")
    val seed = 1
    val random = new Random(seed)

    val g1 = Graph(1~2, 2~3)
    val g2 = Graph(0~1, 1~2, 2~3, 3~4)

    When("computing the isomorphic mapping")
    val mapping = subGraphInspector.randomIsomorphicMapping[Int, UnDiEdge](g1, g2, defaultComparator, random, None)

    Then("the randomizer should not return the same value as one that has had no operations performed on it")
    val unchangedExpectedValue = new Random(seed).nextInt

    random.nextInt should not equal (unchangedExpectedValue)

  }
}