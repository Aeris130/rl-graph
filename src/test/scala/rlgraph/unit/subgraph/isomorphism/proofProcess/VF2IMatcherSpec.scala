package rlgraph.unit.subgraph.isomorphism.proofProcess

import net.cyndeline.rlgraph.subgraph.isomorphism.proofProcess.VF2IMatcher
import rlgraph.SpecImports
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

class VF2IMatcherSpec extends SpecImports {
  private val matcher = new VF2IMatcher[Int, UnDiEdge, String, UnDiEdge]()

  describe("VF2IMatcher") {

    it ("should not return a match if the pattern contains more vertices than the graph") {

      Given("a pattern with 3 vertices, and a graph with 2")
      val graph = Graph(1~2)
      val pattern = Graph("A"~"B", "B"~"C")

      When("computing isomorphic matches in the graph")
      val result = matcher.firstIsomorphicMapping(graph, pattern)

      Then("no result should be found")
      result should be ('empty)

    }

    it ("should return a result from a graph matching the pattern") {

      Given("a graph with three vertices 1-2-3 matching the pattern A-B-C")
      val graph = Graph(1~2, 2~3)
      val pattern = Graph("A"~"B", "B"~"C")

      When("computing isomorphic matches in the graph")
      val result = matcher.firstIsomorphicMapping(graph, pattern)

      Then("a result should be found")
      result should be ('defined)

      And("B should be matched to vertex 2")
      result.get.nodes("B") should be (2)

      And("A should be 3 or 1")
      val aMatch = result.get.nodes("A")
      assert(aMatch == 3 || aMatch == 1)

      And("C should matche whichever vertex A didn't match")
      val cMatch = result.get.nodes("C")
      if (aMatch == 3)
        cMatch should be (1)
      else
        cMatch should be (3)

    }

    it ("should match edges in a mixed graph") {

      Given("a graph with an undirected edge 1-2 and a directed edge 2->3")
      val graph = Graph(1~2, 2~>3)

      When("matching against the pattern A-B, B->C")
      val pattern = Graph("A"~"B", "B"~>"C")
      val result = matcher.firstIsomorphicMapping(graph, pattern)

      Then("a result should be found")
      result should be ('defined)

      And("A should be matched to vertex 1")
      result.get.nodes("A") should be (1)

      And("B should be matched to vertex 2")
      result.get.nodes("B") should be (2)

      And("C should be matched to vertex 3")
      result.get.nodes("C") should be (3)

    }

    it ("should return all matches of a graph") {

      Given("a graph 1-2-3-4")
      val graph = Graph(1~2, 2~3, 3~4)

      When("matching against the pattern A-B-C")
      val a = "A"
      val b = "B"
      val c = "C"
      val pattern = Graph(a~b, b~c)
      val result = matcher.allIsomorphicMappings(graph, pattern).map(_.nodes)

      Then("4 results should be returned")
      result should have size 4

      And("the result should contain A:1 B:2 C:3")
      result should contain (Map(a -> 1, b -> 2, c -> 3))

      And("the result should contain A:3 B:2 C:1")
      result should contain (Map(a -> 3, b -> 2, c -> 1))

      And("the result should contain A:2 B:3 C:4")
      result should contain (Map(a -> 2, b -> 3, c -> 4))

      And("the result should contain A:4 B:3 C:2")
      result should contain (Map(a -> 4, b -> 3, c -> 2))

    }

  }
}
