package rlgraph.unit.pathfinding

import net.cyndeline.rlgraph.pathfinding.LongestDAGPath
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.immutable.Graph

class LongestDAGPathSpec extends SpecImports {

  describe("LongestDAGPath") {

    it ("should compute the longest path for a single vertex") {

      Given("a graph with a single vertex 1")
      val graph = Graph[Int, DiEdge](1)

      When("computing the longest paths from 1")
      val longestPath = LongestDAGPath.withoutWeights(1, graph)

      Then("the path to 1 should be 1")
      longestPath.pathTo(1).get.vertices should be (Vector(1))

    }

    it ("should compute the longest path between two vertices") {

      Given("a graph with two vertices")
      val graph = Graph(1~>2)

      When("computing the longest paths from 1 to 2")
      val longestPath = LongestDAGPath.withoutWeights(1, graph)
      val pathTo2 = longestPath.pathTo(2).get.vertices

      Then("the path to 2 from 1 should be 1 -> 2")
      pathTo2 should be (Vector(1, 2))

    }

    it ("should return None if there's no path between two vertices") {

      Given("a graph with edges 1->3 and 1->2")
      val graph = Graph(1~>2, 1~>3)

      When("computing the longest paths from 3 to 2")
      val longestPath = LongestDAGPath.withoutWeights(3, graph)
      val pathTo2 = longestPath.pathTo(2)

      Then("an empty list should be returned")
      pathTo2 should be (None)

    }

    it ("should compute the longest path when there are multiple paths available") {

      Given("a graph with two paths: 1 > 2 > 3 and 1 > 4 > 5 > 6 > 3")
      val graph = Graph(1~>2, 2~>3, 1~>4, 4~>5, 5~>6, 6~>3)

      When("computing the longest paths from 1 to 6")
      val longestPath = LongestDAGPath.withoutWeights(1, graph)
      val pathTo3 = longestPath.pathTo(3).get.vertices

      Then("the path should be 1, 4, 5, 6, 3")
      pathTo3 should be (Vector(1, 4, 5, 6, 3))

    }

    it ("should compute the longest path when two paths has the same length") {

      Given("a graph with path 1 > 2 and two paths of equal length 1 > 3 > 2, 1 > 4 > 2")
      val graph = Graph(1~>2, 1~>3, 3~>2, 1~>4, 4~>2)

      When("computing the longest paths from 1 to 2")
      val longestPath = LongestDAGPath.withoutWeights(1, graph)
      val pathTo2 = longestPath.pathTo(2).get.vertices

      Then("the longest path should either be 1, 3, 2 or 1, 4, 2")
      assert(pathTo2 == Vector(1, 3, 2) || pathTo2 == Vector(1, 4, 2))

    }

    it ("should take edge weights into consideration") {

      Given("a path 1 > 2 > 3 and a path 1 > 4 > 5 > 3, with edge 2 > 3 having weight 5 and every other edge weight 1")
      val graph = Graph(1~>2 % 1, 2~>3 % 5, 1~>4 % 1, 4~>5 % 1, 5~>3 % 1)

      When("computing the longest paths from 1 to 3")
      val longestPath = LongestDAGPath.withWeights(1, graph)
      val pathTo3 = longestPath.pathTo(3).get.vertices

      Then("the shorter (vertex-wise) path should be selected")
      pathTo3 should be (Vector(1, 2, 3))

    }

    it ("should take negative edge weights into consideration") {

      Given("a long path 1 > 4 > 5 > 3 and a short path 1 > 2 > 3, with edge 1 > 4 having a negative weight -10")
      val graph = Graph(1~>2 % 1, 2~>3 % 1, 1~>4 % -10, 4~>5 % 1, 5~>3 % 1)

      When("computing the longest paths from 1 to 3")
      val longestPath = LongestDAGPath.withWeights(1, graph)
      val pathTo3 = longestPath.pathTo(3).get.vertices

      Then("the shorter path should be selected")
      pathTo3 should be (Vector(1, 2, 3))

    }

    it ("should compute a path when all edges have weight 0") {

      Given("two paths with weight 0")
      val graph = Graph(1~>2 % 0, 2~>3 % 0, 1~>4 % 0, 4~>5 % 0, 5~>3 % 0)

      When("computing the longest paths from 1 to 3")
      val longestPath = LongestDAGPath.withWeights(1, graph)
      val pathTo3 = longestPath.pathTo(3).get.vertices

      Then("Either path should be selected")
      assert(pathTo3 == Vector(1, 2, 3) || pathTo3 == Vector(1, 4, 5, 3))

    }

    it ("should compute a shortest path with multiple vertices having no incoming edges") {

      Given("a graph where vertex 2 has incoming edges from 1, 3, 4, 5, 6, 7.")
      val graph = Graph(1~>2, 3~>2, 4~>2, 5~>2, 6~>2, 7~>2)

      When("computing the longest path from 4 to 2")
      val longestPath = LongestDAGPath.withoutWeights(4, graph)
      val pathTo2 = longestPath.pathTo(2).get.vertices

      Then("the path should be 4, 2")
      pathTo2 should be (Vector(4, 2))

    }

    it ("should compute the cost of traversing the graphs edges") {

      Given("a path with an edge having weight 2, an edge with weight 5 and an edge with weight -1")
      val graph = Graph(1~>2 % 2, 2~>3 % 5, 3~>4 % -1)

      When("computing the path cost from 1 to 4")
      val longestPath = LongestDAGPath.withWeights(1, graph)
      val cost = longestPath.distanceTo(4)

      Then("cost should be 6")
      cost should be (6)

    }

    it ("should compute the start and stop values of the path") {

      Given("a path from 1 to 3")
      val graph = Graph(1~>2, 2~>3, 3~>4)
      val longestPath = LongestDAGPath.withoutWeights(1, graph)

      When("computing the path from 1 to 4")
      val pathTo4 = longestPath.pathTo(4).get

      Then("the start and stop should be 1 and 4")
      pathTo4.start should be (1)
      pathTo4.stop should be (4)

    }

    it ("should compute the edges of a path") {

      Given("a path from 1 to 3")
      val graph = Graph(1~>2, 2~>3, 3~>4)
      val longestPath = LongestDAGPath.withoutWeights(1, graph)

      When("computing the path from 1 to 4")
      val pathTo4 = longestPath.pathTo(4).get

      Then("the edges should be 1->2, 2->3, 3->4")
      pathTo4.edges should be (Vector(1~>2, 2~>3, 3~>4))

    }

  }

}
