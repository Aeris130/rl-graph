package rlgraph.unit.pathfinding.djikstra

import net.cyndeline.rlgraph.pathfinding.djikstra.DjikstraShortestPath
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DjikstraShortestPathSpec extends SpecImports {

  describe("DjikstraShortestPath") {

    it ("should compute the shortest path of a simple tree") {

      Given("a graph with 3 vertices connected by 2 edges")
      val g = Graph(1~2, 2~3)

      When("computing the shortest path from 1 to 3")
      val shortestPath = new DjikstraShortestPath(g)
      val path = shortestPath.nodesOnShortestPath(1, 3)

      Then("the path should be 1, 2, 3")
      path should be (Vector(1, 2, 3))

    }

    it ("should compute a path from and to a single vertex") {

      Given("a graph with 1 vertex")
      val g = Graph[Int, UnDiEdge](1)

      When("computing the shortest path from 1 to 3")
      val shortestPath = new DjikstraShortestPath(g)
      val path = shortestPath.nodesOnShortestPath(1, 1)

      Then("the path should be 1")
      path should be (Vector(1))

    }

    it ("should compute the shortest path when multiple paths exists") {

      Given("a graph with two paths from vertex 1 to 5")
      val path1 = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~5)
      val path2 = Graph[Int, UnDiEdge](1~6, 6~5)
      val g = path1 union path2

      When("computing the shortest path from 1 to 3")
      val shortestPath = new DjikstraShortestPath(g)
      val path = shortestPath.nodesOnShortestPath(1, 5)

      Then("the resulting path should be 1, 6, 5")
      path should be (Vector(1, 6, 5))

    }

    it ("should exclude the start vertex when computing distances") {

      Given("a graph with 3 vertices connected by 2 edges")
      val g = Graph(1~2, 2~3)

      When("computing the distance of the shortest path from 1 to 3")
      val shortestPath = new DjikstraShortestPath(g)
      val pathDistance = shortestPath.distance(1, 3)

      Then("the distance should be 2")
      pathDistance should be (2)

    }
  }
}
