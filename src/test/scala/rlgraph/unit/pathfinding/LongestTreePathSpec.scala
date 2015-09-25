package rlgraph.unit.pathfinding

import net.cyndeline.rlgraph.pathfinding.{LongestTreePath, Path}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class LongestTreePathSpec extends SpecImports {
  private val pathFinder = new LongestTreePath[Int, UnDiEdge]()

  describe("LongestTreePath") {

    it ("should find the longest path in a tree with a single path") {

      Given("a path 1-2-3-4")
      val edges = Vector(1~2, 2~3, 3~4)
      val tree = Graph.from(Nil, edges)

      When("searching for the longest path from node 1")
      val path = pathFinder.computeLongestPath(tree, 1)

      Then("the resulting path should be 1, 2, 3, 4")
      path.start should be (1)
      path.stop should be (4)
      path.vertices should be (Vector(1, 2, 3, 4))
      path.edges should be (edges)

    }

    it ("should select the longer of two paths") {

      Given("a root 1 with paths 1-2-3 and 1-4-5-6-7")
      val p1 = Vector(1~2, 2~3)
      val p2 = Vector(1~4, 4~5, 5~6, 6~7)
      val tree = Graph.from(Nil, p1 ++ p2)

      When("searching for the longest path from node 1")
      val path = pathFinder.computeLongestPath(tree, 1)

      Then("the resulting path should be 1, 4, 5, 6, 7")
      path.start should be (1)
      path.stop should be (7)
      path.vertices should be (Vector(1, 4, 5, 6, 7))
      path.edges should be (p2)

    }

    it ("should select a path when two paths have the same length") {

      Given("a tree with paths 1-2-3 and 1-4-5")
      val p1 = Vector(1~2, 2~3)
      val p2 = Vector(1~4, 4~5)
      val tree = Graph.from(Nil, p1 ++ p2)

      When("searching for the longest path from node 1")
      val path = pathFinder.computeLongestPath(tree, 1)

      Then("one of the paths should be selected")
      val candidate1 = Path(1, p1)
      val candidate2 = Path(1, p2)
      assert(path == candidate1 || path == candidate2)

    }

    it ("should return a path containing a single node") {

      Given("a graph containing the node 1")
      val tree = Graph[Int, UnDiEdge](1)

      When("searching for the longest path from node 1")
      val path = pathFinder.computeLongestPath(tree, 1)

      Then("the path 1 should be returned")
      path.start should be (1)
      path.stop should be (1)
      path.vertices should be (Vector(1))
      path.edges should be ('empty)

    }

    it ("should throw an exception if the specified node is not in the graph") {

      Given("a graph containing the node 1 and 2")
      val tree = Graph(1~2)

      When("searching for the longest path from node 3")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        pathFinder.computeLongestPath(tree, 3)
      }

    }

  }

}
