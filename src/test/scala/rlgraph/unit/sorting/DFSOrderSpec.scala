package rlgraph.unit.sorting

import net.cyndeline.rlgraph.sorting.DFSOrder
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DFSOrderSpec extends SpecImports {

  describe("DFSOrder") {

    it ("should order a sequence of vertices") {

      Given("a path 1 -> 5")
      val graph = Graph(1~2, 2~3, 3~4, 4~5)

      When("sorting the vertices in DFS order starting at vertex 1")
      val dfsOrder = DFSOrder(graph, 1)

      Then("the order should be 1, 2, 3, 4, 5")
      dfsOrder should be (Vector(1, 2, 3, 4, 5))

    }

    it ("should order a tree") {

      Given("a binary tree with root 1, and levels 2, 3 | 4, 5 + 6, 7")
      val graph = Graph(1~2, 1~3, 2~4, 2~5, 3~6, 3~7)

      When("sorting the vertices in DFS order starting at vertex 1")
      val dfsOrder = DFSOrder(graph, 1)

      Then("the first vertex should be 1")
      dfsOrder.head should be (1)

      And("the next vertex should be 2 or 3")
      val v2 = dfsOrder(1)
      assert(v2 == 2 || v2 == 3)

      And("the next vertex should be 4 or 5 if the second vertex was 2, or 6 or 7 if it was 3")
      val v3 = dfsOrder(2)
      if (v2 == 2) {
        assert(v3 == 4 || v3 == 5)
      } else {
        assert(v3 == 6 || v3 == 7)
      }

      And("the next vertex should be the opposite children of v3")
      val v4 = dfsOrder(3)
      v3 match {
        case 4 => v4 should be (5)
        case 5 => v4 should be (4)
        case 6 => v4 should be (7)
        case 7 => v4 should be (6)
      }

    }

    it ("should only order the vertices reachable from the start vertex") {

      Given("a disconnected graph")
      val graph = Graph(1~2, 2~3, 4~5)

      When("sorting the vertices in DFS order starting at vertex 1")
      val dfsOrder = DFSOrder(graph, 1)

      Then("the order should be 1, 2, 3, 4, 5")
      dfsOrder should be (Vector(1, 2, 3))

    }

  }

}
