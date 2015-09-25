package rlgraph.unit.pathfinding

import net.cyndeline.rlgraph.pathfinding.BFSPathfinder
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BFSPathfinderSpec extends SpecImports {
  private val pathfinder = new BFSPathfinder()

  describe("BFSPathfinder") {

    it ("should compute a path from a single edge") {

      Given("a graph with an edge 1-2")
      val graph = Graph(1~2)

      When("computing a path between 1 and 2")
      val path = pathfinder.computePath(1, 2, graph).get.vertices

      Then("the path 1, 2 should be found")
      path should be (Vector(1, 2))

    }

    it ("should compute a path from a single vertex") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing a path between 1 and 1")
      val path = pathfinder.computePath(1, 1, graph).get.vertices

      Then("the resulting path should contain a single vertex")
      path should be (Vector(1))

    }

    it ("should compute an empty edge-list from a single vertex") {

      Given("a graph with a single vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing an edge path between 1 and 1")
      val path = pathfinder.computePath(1, 1, graph).get.edges

      Then("the resulting path should be empty")
      path should be (Vector())

    }

    it ("should find a path between two vertices in a graph with a single path") {

      Given("a path 1, 2, 3")
      val graph = Graph(1~2, 2~3)

      When("computing a path between 1 and 3")
      val path = pathfinder.computePath(1, 3, graph).get.vertices

      Then("the result should be 1, 2, 3")
      path should be (Vector(1, 2, 3))

    }

    it ("should take edge directions into consideration") {

      Given("a vertex 2 with edges pointing to 1 and 3")
      val graph = Graph(2~>3, 2~>1)

      When("computing a path between 1 and 3")
      val path = pathfinder.computePath(1, 3, graph)

      Then("No result should be found")
      path should be (None)

    }

    it ("should find a path when multiple paths exist") {

      Given("a graph with 2 paths between 1 and 4: (1, 2, 3, 4) and (1, 5, 4)")
      val graph = Graph(1~2, 2~3, 3~4, 1~5, 5~4)

      When("computing a path between 1 and 4")
      val path = pathfinder.computePath(1, 4, graph)

      Then("the path should be (1, 2, 3, 4) or (1, 5, 4)")
      path should be ('defined)
      assert(path.get.vertices == Vector(1, 2, 3, 4) || path.get.vertices == Vector(1, 5, 4))

    }

    it ("should stop at dead ends") {

      Given("a tree with vertex 1 as the root and vertex 6 as a leaf not connected to the root")
      val graph = Graph(1~2, 1~3, 2~4, 2~5, 3~6, 3~7)

      When("computing a path between 1 and 6")
      val path = pathfinder.computePath(1, 6, graph).get.vertices

      Then("the path 1, 3, 6 should be found")
      path should be (Vector(1, 3, 6))

    }

    it ("should not return a path when start and stop are disconnected") {

      Given("a graph with two disconnected vertices 1 and 2")
      val graph = Graph(1~3, 2~5, 6~5)

      When("computing a path between 1 and 2")
      val path = pathfinder.computePath(1, 2, graph)

      Then("No result should be found")
      path should be (None)

    }

    it ("should compute edges on a path") {

      Given("a path with edges 1, 2 and 2, 3")
      val e1 = 1~2
      val e2 = 2~>3
      val graph = Graph(e1, e2)

      When("computing a path between 1 and 3")
      val path = pathfinder.computePath(1, 3, graph)

      Then("the edges used in the graph should be returned")
      path should be ('defined)
      path.get.edges should be (Vector(e1, e2))

    }

  }
}
