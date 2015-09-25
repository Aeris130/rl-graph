package rlgraph.unit.pathfinding.disjointElementaryCycle

import net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help.DisjointPathParser
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DisjointPathParserSpec extends SpecImports {
  private val disjointPathParser = new DisjointPathParser[Int, UnDiEdge]()

  describe("DisjointPathParser") {

    it ("should parse an empty list from a graph only containing the elementary cycle") {

      Given("an elementary cycle 1,2,3,4 and a graph containing the cycle")
      val graph = Graph(1~2, 2~3, 3~4, 4~1)
      val cycle = Vector(1, 2, 3, 4)

      When("parsing disjoint paths")
      val paths = disjointPathParser.computePaths(cycle, graph)

      Then("the result should be empty")
      paths should be ('empty)

    }

    it ("should parse a path leading from the elementary cycle to a dead end") {

      Given("an elementary cycle 1,2,3,4 and a path 2, 5, 6")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 2~5, 5~6)

      When("parsing disjoint paths")
      val paths = disjointPathParser.computePaths(Vector(1, 2, 3, 4), graph)

      Then("a single path should be found")
      paths should have size 1

      And("that path should go from 6 to 2")
      val disjointPath = paths.head
      disjointPath.path should be (Vector(6, 5, 2))

      And("vertex 6 should be a dead end")
      disjointPath.deadEnd should be ('defined)
      disjointPath.deadEnd.get should be (6)

    }

    it ("should parse a path leading from a vertex on the elementary cycle to another") {

      Given("an elementary cycle 1,2,3,4 and a disjoint path between 1 and 3 (1,5,3)")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)
      val path1 = Graph(1~5, 5~3)
      val graph = cycle ++ path1

      When("parsing disjoint paths")
      val paths = disjointPathParser.computePaths(Vector(1, 2, 3, 4), graph)

      Then("a single path should be found")
      paths should have size 1

      And("the path should either be 1,5,3 or 3,5,1 and contain no dead end")
      assert(paths.head.path == Vector(1, 5, 3) || paths.head.path == Vector(3, 5, 1))
      paths.head.deadEnd should be (None)

    }

    it ("should parse a path leading from a dead end to a path between two vertices on the elementary cycle") {

      Given("an elementary cycle 1,2,3,4 and a disjoint path between 1 and 3 (1,5,3) as well as another path 5,6,7")
      val cycle = Graph(1~2, 2~3, 3~4, 4~1)
      val path1 = Graph(1~5, 5~3)
      val path2 = Graph(5~6, 6~7)
      val graph = cycle ++ path1 ++ path2

      When("parsing disjoint paths")
      val paths = disjointPathParser.computePaths(Vector(1, 2, 3, 4), graph)

      Then("2 paths should be found")
      paths should have size 2

      And("one of the paths should be 7,6,5 with 7 being a dead end")
      val vList2 = Vector(7, 6, 5)
      val p2 = paths.find(_.path == vList2).getOrElse(fail("No path " + vList2 + " found."))
      p2.deadEnd should be (Some(7))

      And("the other path should be 1, 5, 3 (in either direction) and have no dead end")
      val vList1 = Vector(1, 5, 3)
      val p1 = paths.find(dp => dp.path == vList1 || dp.path == vList1.reverse).getOrElse(fail("No path " + vList1 + " found."))
      p1.deadEnd should be (None)

    }

    it ("should parse a disjoint path when no elementary cycle exists") {

      Given("a graph with a single disjoint path")
      val graph = Graph(1~2, 2~3)

      When("parsing disjoint paths using an empty elementary cycle")
      val paths = disjointPathParser.computePaths(Vector(), graph)

      Then("a single path should be found")
      paths should have size 1

      And("that path should be 1,2,3 or 3,2,1")
      val expected1 = Vector(1, 2, 3)
      val expected2 = Vector(3, 2, 1)
      assert(paths.head.path == expected1 || paths.head.path == expected2, "No path " + expected1 + " or " + expected2 + " found, only " + paths.head.path)

    }

    it ("should parse multiple disjoint paths when no elementary cycle exists") {

      Given("a tree with the edges paths 1,2,3,4,5 and 3,6,7")
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 3~6, 6~7)

      When("parsing disjoint paths using an empty elementary cycle")
      val paths = disjointPathParser.computePaths(Vector(), graph)

      Then("two paths should be found")
      paths should have size 2

      And("one of the paths should either be 1,2,3 or 5,4,3 or 7,6,3")
      val p1 = Vector(1, 2, 3)
      val p2 = Vector(5, 4, 3)
      val p3 = Vector(7, 6, 3)
      val path = paths.find(p => p.path == p1 || p.path == p2 || p.path == p3).getOrElse(fail(paths + " did not contain " + p1 + " or " + p2 + " or " + p3))

      And("the other path should be the remaining vertices")
      val expectedRemaining = path.path match {
        case Vector(1, 2, 3) => Vector(7, 6, 3, 4, 5)
        case Vector(7, 6, 3) => Vector(1, 2, 3, 4, 5)
        case Vector(5, 4, 3) => Vector(1, 2, 3, 6, 7)
      }
      val remainingPath = paths.filterNot(_ == path).head
      val remaining = remainingPath.path
      assert(remaining == expectedRemaining || remaining == expectedRemaining.reverse, remaining + " did not match the expected path " + expectedRemaining + " or its reverse.")

      And("the first element in the path should be a dead end")
      remainingPath.deadEnd should be ('defined)
      remainingPath.deadEnd should be (Some(remaining.head))

    }

  }

}
