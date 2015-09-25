package rlgraph.unit.subgraph

import net.cyndeline.rlgraph.subgraph.triangles.TriangleFinder
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TriangleFinderSpec extends SpecImports {
  private val triangleFinder = new TriangleFinder()
  private val co = new CycleOrderMatcher()

  describe("TriangleFinder") {

    it ("should find a single triangle in a triangular graph") {

      Given("a triangular graph")
      val graph = Graph(1~2, 2~3, 3~1)

      When("parsing triangles")
      val triangles = triangleFinder.computeTriangles(graph)

      Then("the triangle 1, 2, 3 should be found")
      triangles should have size 1
      val triangle = triangles.head
      assert(co.compareBothDirections(triangle.toVector, Vector(1, 2, 3)))

    }

    it ("should find two rectangles") {

      Given("two rectangles 1,2,3 | 2,3,4 ")
      val graph = Graph(1~2, 2~3, 3~1, 2~4, 3~4)

      When("parsing triangles")
      val tr = triangleFinder.computeTriangles(graph)
      val triangles = tr.map(t => t.toVector)

      Then("triangle 1, 2, 3 should be found")
      assert(triangles.exists(t => co.compareBothDirections(t, Vector(1, 2, 3))))

      And("triangle 2, 3, 4 should be found")
      assert(triangles.exists(t => co.compareBothDirections(t, Vector(2, 3, 4))))

    }

    it ("should not include vertices that aren't part of a triangle") {

      Given("a biconnected graph with the triangle 1, 2, 3 and a path 3, 4, 5, 2")
      val graph = Graph(1~2, 2~3, 3~1, 3~4, 4~5, 5~2)

      When("parsing triangles")
      val triangles = triangleFinder.computeTriangles(graph)

      Then("a single triangle should be found")
      triangles should have size 1

      And("that triangle should be 1, 2, 3")
      val triangle = triangles.head.toVector
      assert(co.compareBothDirections(triangle, Vector(1, 2, 3)))

    }

  }
}
