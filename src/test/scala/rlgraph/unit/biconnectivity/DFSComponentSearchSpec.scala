package rlgraph.unit.biconnectivity

import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import rlgraph.SpecImports

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class DFSComponentSearchSpec extends SpecImports {

  def fixture = new {
    val dfsComponentSearch = new DFSComponentSearch[Int, UnDiEdge]()
  }

  describe("DFSComponentSearch") {

    /*
     * Biconnectivity check
     */

    it ("should confirm that a biconnected graph is thus") {

      Given("a biconnected graph")
      val f = fixture
      import f._
      val graph = Graph(1~2, 2~3, 3~1)

      When("checking if the graph is biconnected")
      val isBiconnected = dfsComponentSearch.isBiconnected(graph)

      Then("the result should be true")
      isBiconnected should be (true)

    }

    it ("should reject that a graph is biconnected if at least one cutpoint exist") {

      Given("a graph with a cutpoint 3")
      val f = fixture
      import f._
      val graph = Graph(1~2, 2~3, 3~1, 3~4)

      When("checking if the graph is biconnected")
      val isBiconnected = dfsComponentSearch.isBiconnected(graph)

      Then("the result should be false")
      isBiconnected should be (false)

    }

    /*
     * Biconnected components
     */

    it ("should return the entire graph if the graph is a single biconnected component") {

      Given("a graph that is biconnected")
      val f = fixture
      import f._
      val graph = Graph[Int, UnDiEdge](1~2, 1~3, 2~3)

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("the result should be a single entry")
      result.size should be (1)

      And("it should contain the same vertices and edges as the original graph")
      result(0) should equal (graph)

      And("it should be returned as a separate graph instance")
      assert(!result(0).eq(graph), "The biconnected component returned was the original graph instance")

    }

    it ("should assign each bridge a component of its own") {
      val f = fixture
      import f._

      Given("a graph consisting of a chain of 3 edges")
      val graph = Graph[Int, UnDiEdge](2~3, 3~4, 4~5)

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("each edge should be assigned its own component")
      result.size should be (3)
      result(0) intersect result(1) intersect result(2) should equal (Graph[Int, UnDiEdge]())

      val resultSet = result.toSet
      resultSet should contain (Graph[Int, UnDiEdge](2~3))
      resultSet should contain (Graph[Int, UnDiEdge](3~4))
      resultSet should contain (Graph[Int, UnDiEdge](4~5))

      And("the total edge- and vertex set should be the original graph")
      result(0) union result(1) union result(2) should equal (graph)
    }

    it ("should separate components joined by a cutvertex") {
      val f = fixture
      import f._

      Given("a butterfly graph joining its wings in vertex 3")
      val graph = Graph[Int, UnDiEdge](1~2, 1~3, 2~3, 3~4, 3~5, 4~5)

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("each wing should be assigned its own component")
      result.size should be (2)
      result(0) intersect result(1) should equal (Graph[Int, UnDiEdge](3))
      result(0) union result(1) should equal (graph)
    }

    it ("should separate components joined by a bridge") {
      val f = fixture
      import f._

      Given("""a "butterfly" graph joining its wings in an edge going from vertex 3 to 4""")
      val graph = Graph[Int, UnDiEdge](1~2, 1~3, 2~3, 3~4, 4~5, 4~6, 5~6)

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("each wing should be assigned its own component along with the bridge")
      result.size should be (3)
      val resultSet = result.toSet
      resultSet should contain (Graph[Int, UnDiEdge](3~4)) // Bridge
      resultSet should contain (Graph[Int, UnDiEdge](1~2, 1~3, 2~3)) // Wing A
      resultSet should contain (Graph[Int, UnDiEdge](4~5, 4~6, 5~6)) // Wing B

    }

    it ("should separate a chain of components") {
      val f = fixture
      import f._

      Given("3 components A, B, C with A and C only connected to 1 other component")
      val A = Graph[Int, UnDiEdge](1~2, 1~3, 2~3)
      val B = Graph[Int, UnDiEdge](3~4, 3~5, 4~5)
      val C = Graph[Int, UnDiEdge](5~6, 5~7, 6~7)
      val graph = A union B union C

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("each component should be separated")
      result.size should be (3)
      val resultSet = result.toSet
      resultSet should contain (A)
      resultSet should contain (B)
      resultSet should contain (C)
    }

    it ("should compute components for a disconnected graph") {
      val f = fixture
      import f._

      Given("a graph consisting of one biconnected component and one bridge, both disjoint")
      val graph = Graph[Int, UnDiEdge](1~2, 1~3, 2~3, 99~100)

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("both parts of the graph should be assigned their own component")
      result.size should be (2)
      val resultSet = result.toSet
      resultSet should contain(Graph[Int, UnDiEdge](1~2, 1~3, 2~3))
      resultSet should contain(Graph[Int, UnDiEdge](99~100))
    }

    it ("should assign every edge in a component ot the same component") {
      val f = fixture
      import f._

      Given("a graph that can be divided into multiple sub-components")
      val A = Graph[Int, UnDiEdge](1~2, 1~3, 2~3)
      val B = Graph[Int, UnDiEdge](2~3, 3~4, 4~2)
      val graph = A union B

      When("computing biconnected components")
      val result: Vector[Graph[Int, UnDiEdge]] = dfsComponentSearch.components(graph)

      Then("all edges should be assigned to a single component")
      result.size should be (1)
      result(0) should equal (graph)
    }

    it ("should return an empty list for an empty graph") {
      val f = fixture
      import f._

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("computing biconnected components")
      val components = dfsComponentSearch.components(graph)

      Then("the result should be an empty list")
      components should be ('empty)

    }

    it ("should return an empty list for a single vertex") {
      val f = fixture
      import f._

      Given("a graph with a single vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing biconnected components")
      val components = dfsComponentSearch.components(graph)

      Then("the result should be an empty list")
      components should be ('empty)

    }

    /*
     * Cutpoints
     */

    it ("shouldn't find any cutpoints if the graph is empty") {
      val f = fixture
      import f._

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("no cutpoints should be found")
      cutpoints should be (Vector())

    }

    it ("shouldn't find any cutpoints if the graph contains a single biconnected component") {
      val f = fixture
      import f._

      Given("a biconnected component")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~1)

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("no cutpoints should be found")
      cutpoints should be (Vector())

    }

    it ("should find a single cutpoint in a chain of three vertices") {
      val f = fixture
      import f._

      Given("a cain of vertices 1-2-3 with vertex 2 as a cutpoint")
      val graph = Graph(1~2, 2~3)

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("vertex 2 should be marked as a cutpoint")
      cutpoints should be (Vector(2))

    }

    it ("should find two cutpoints in a chain of four vertices") {
      val f = fixture
      import f._

      Given("a cain of vertices 1-2-3-4 with vertex 2 and 3 as a cutpoint")
      val graph = Graph(1~2, 2~3, 3~4)

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("vertex 2 and 4 should be marked as a cutpoint")
      cutpoints.toSet should be (Set(2, 3))
      cutpoints.size should be (2)

    }

    it ("should find a single cutpoint connecting three components") {
      val f = fixture
      import f._

      Given("a cutpoint 1 connecting three components")
      val comp1 = Graph(1~0, 0~2, 2~1)
      val comp2 = Graph(1~6)
      val comp3 = Graph(1~3, 3~4, 4~5, 5~1)
      val graph = comp1 ++ comp2 ++ comp3

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("vertex 1 should be marked as a cutpoint")
      cutpoints should be (Vector(1))

    }

    it ("should find two cutpoints connecting three components") {
      val f = fixture
      import f._

      Given("a cutpoint 0 and 3 connecting three components")
      val comp1 = Graph(1~0, 0~2, 2~1)
      val comp2 = Graph(0~3)
      val comp3 = Graph(3~4)
      val graph = comp1 ++ comp2 ++ comp3

      When("computing cutpoints")
      val cutpoints = dfsComponentSearch.articulationPoints(graph)

      Then("vertex 0 and 3 should be marked as a cutpoint")
      cutpoints.toSet should be (Set(0, 3))
      cutpoints.size should be (2)

    }

    /*
     * Cutpoints and components
     */

    it ("should return an empty set of cutpoints for a single biconnected component") {

      Given("a graph that is biconnected")
      val f = fixture
      import f._
      val graph = Graph[Int, UnDiEdge](1~2, 1~3, 2~3)

      When("computing biconnected components and cutpoints")
      val result = dfsComponentSearch.componentsAndArticulationPoints(graph)

      Then("the result should contain a single entry")
      result.size should be (1)

      And("the component should be equal to the input graph")
      result.head._1 should equal (graph)

      And("the cutpoint set should be empty")
      result.head._2 should be ('empty)

    }

    it ("should assign a cutpoint to both components it belongs to") {
      val f = fixture
      import f._

      Given("a cutpoint 0 connecting two components")
      val comp1 = Graph(1~0, 0~2, 2~1)
      val comp2 = Graph(0~3)
      val graph = comp1 ++ comp2

      When("computing biconnected components and cutpoints")
      val result = dfsComponentSearch.componentsAndArticulationPoints(graph)

      Then("the result should contain two entries")
      result.size should be (2)

      And("one of the entries should contain component 1 and cutpoint 0")
      val c1 = result.find(v => v._1 == comp1).get
      c1._2 should equal (Set(0))

      And("one of the entries should contain component 2 and cutpoint 0")
      val c2 = result.find(v => v._1 == comp2).get
      c2._2 should equal (Set(0))

    }

    it ("should assign cutpoints to a chain of biconnected components sharing a single vertex") {
      val f = fixture
      import f._

      Given("two biconnected components joined by a third at vertices 4 and 5")
      val comp1 = Graph(1~2, 2~4, 4~3, 3~1, 2~3)
      val comp2 = Graph(5~6, 6~8, 8~7, 7~5, 6~7)
      val comp3 = Graph(4~5, 5~9, 9~4)
      val graph = comp1 ++ comp2 ++ comp3

      When("computing biconnected components and cutpoints")
      val result = dfsComponentSearch.componentsAndArticulationPoints(graph)

      Then("the result should contain two entries")
      result.size should be (3)

      And("one of the entries should contain component 1 and cutpoint 4")
      val c1 = result.find(v => v._1 == comp1).get
      c1._2 should equal (Set(4))

      And("one of the entries should contain component 2 and cutpoint 5")
      val c2 = result.find(v => v._1 == comp2).get
      c2._2 should equal (Set(5))

      And("one of the entries should contain component 3 and cutpoints 4 and 5")
      val c3 = result.find(v => v._1 == comp3).get
      c3._2 should equal (Set(4, 5))

    }

    it ("should not assign cutpoints to components they're not a part of") {
      val f = fixture
      import f._

      Given("a cutpoint 0 and 3 connecting three components")
      val comp1 = Graph(1~0, 0~2, 2~1)
      val comp2 = Graph(0~3)
      val comp3 = Graph(3~4)
      val graph = comp1 ++ comp2 ++ comp3

      When("computing cutpoints")
      val result = dfsComponentSearch.componentsAndArticulationPoints(graph)

      Then("component 1 should only contain vertex 0 as a cutpoint")
      val c1Cutpoints = result.find(v => v._1 == comp1).get._2
      c1Cutpoints should equal (Set(0))

      And("component 2 should contain vertex 0 and 3 as cutpoints")
      val c2Cutpoints = result.find(v => v._1 == comp2).get._2
      c2Cutpoints should equal (Set(0, 3))

      And("component 3 should only contain vertex 3 as a cutpoint")
      val c3Cutpoints = result.find(v => v._1 == comp3).get._2
      c3Cutpoints should equal (Set(3))

    }

  }
}
