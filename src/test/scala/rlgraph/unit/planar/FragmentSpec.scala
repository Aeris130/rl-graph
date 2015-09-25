package rlgraph.unit.planar

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.planar.demoucron.operation.help.Fragment
import rlgraph.SpecImports

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FragmentSpec extends SpecImports {

  def fixture = new {

    val g1 = Graph[Int, UnDiEdge](1~4, 2~4, 2~5, 3~5, 4~5)
    val c1 = Set(1, 2, 3)
    val fragment1 = new Fragment(g1, c1)

    val invalidContactSet = Set(1)
    val nonPresentContact = Set(1, 99)
    val disconnectedGraph = Graph[Int, UnDiEdge](1~2, 3~4)

  }

  describe("Fragment") {

    it ("should report a face containing all its contact vertices as admissible") {
      val f = fixture
      import f._

      Given("a fragment with contact vertices 1, 2, 3 and a face containing vertices 1 to 5")
      val fragment = fragment1
      val face = new Face(Vector(1, 3, 4, 2, 5))

      When("checking if the face is admissible")
      val admissible = fragment.admissible(face)

      Then("result should be true")
      admissible should be (true)
    }

    it ("should report a face not containing every contact vertex as inadmissible") {
      val f = fixture
      import f._

      Given("a fragment with contact vertices 1, 2, 3 and a face not containing vertex 3")
      val fragment = fragment1
      val face = new Face(Vector(1, 4, 2, 5))

      When("checking if the face is admissible")
      val admissible = fragment.admissible(face)

      Then("result should be true")
      admissible should be (false)
    }

    it ("should compute alpha paths that doesn't traverse contact vertices beyond start/stop") {
      val f = fixture
      import f._

      Given("a fragment with paths between contact vertices that may include other contact vertices")
      val fragment = fragment1

      When("computing the alpha path")
      val path = fragment.alphaPath
      val edges: Iterable[UnDiEdge[Int]] = path.edges.toIterable

      Then("the returned path should only contain two contact vertices")
      val resultingGraph = Graph.from(Set.empty, edges)
      val verticesInGraph = resultingGraph.nodes.toOuter.toSet[Int]

      c1 diff verticesInGraph should have size (1)

      And("the contact vertices that are present should be at the beginning and end of the path")
      val contactNodeT: Set[Int] = for(node <- c1) yield {
        val outer: Int = g1.get(node)
        outer
      }
      val nodeOrder: Vector[Int] = path.edges.map(e => Vector(e._1, e._2)).toVector.flatten.distinct
      val start = nodeOrder(0)
      val stop = nodeOrder(nodeOrder.size - 1)

      contactNodeT should contain (start)
      contactNodeT should contain (stop)

    }

    it ("should throw an exception when attempting to create fragments with less than 2 contact vertices") {
      val f = fixture
      import f._

      Given("a set of contact vertices of size 1")
      val contacts = invalidContactSet

      When("creating a fragment with the contact set")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        val illegalFragment = new Fragment(g1, contacts)
      }
    }

    it ("should throw an exception when attempting to create fragments with disconnected components") {
      val f = fixture
      import f._

      Given("a disconnected graph component")
      val graph = disconnectedGraph

      When("creating a fragment with the graph")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        val illegalFragment = new Fragment(graph, Set(1, 2))
      }
    }

    it ("should throw an exception when attempting to create fragments with contact vertices not present in the component") {
      val f = fixture
      import f._

      Given("a contact set with a contact vertex not present in the graph")
      val contact = nonPresentContact

      When("creating a fragment with the contact set")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        val illegalFragment = new Fragment(g1, contact)
      }
    }

    it ("should throw an exception when computing alpha paths on a component where the path crosses contact vertices on the way to its goal") {

      Given("a graph component where all paths from 1 to 5 passes vertex 3")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~5)
      val contacts = Set(1, 3, 5)
      val fragment = new Fragment(graph, contacts)

      When("computing an alphaPath that contains contact vertex 3 inside the path")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        fragment.alphaPath(1, 5)
      }
    }

    it ("should throw an exception when computing alpha paths using the same start and stop vertex") {
      val f = fixture
      import f._

      Given("a valid fragment")
      val valid = fragment1

      When("computing an alpha path from and to the same contact vertex")
      Then("an IllegalArgumentException should be thrown")
      intercept[IllegalArgumentException] {
        valid.alphaPath(1, 1)
      }
    }

    it ("should throw an exception when computing alpha paths using vertices that aren't contact vertices") {
      val f = fixture
      import f._

      Given("a valid fragment")
      val valid = fragment1

      When("computing an alpha path from and to a vertex that aren't in the contact set")
      Then("an IllegalArgumentException should be thrown")

      /* 1 is a contact vertex, 4 is not. */
      intercept[IllegalArgumentException] {
        valid.alphaPath(1, 4)
      }
      intercept[IllegalArgumentException] {
        valid.alphaPath(4, 1)
      }
    }

    it ("should throw an exception when supplying a graph where at least one vertex with degree 1 isn't a contact vertex") {
      val f = fixture

      Given("a fragment where the vertex 1 has one outgoing edge but isn't marked as a contact vertex")
      Then("the constructor should throw an illegal argument exception")
      intercept[IllegalArgumentException] {
        val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~5)
        val contacts = Set(3, 5) // Missing 1
        val fragment = new Fragment(graph, contacts)
      }
    }
  }
}
