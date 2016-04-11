package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.BellmanFordShortestPath
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex, ResidualEdge, ResidualNetwork}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BellmanFordShortestPathSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val shortestPath = new BellmanFordShortestPath()

  def singlePath = new {
    val v1 = new FlowVertex(1)
    val v2 = new FlowVertex(2)
    val v3 = new FlowVertex(3)

    val e1 = v1 ~> v2 ## (12, 0, 4, -3, 1)
    val e2 = v2 ~> v3 ## (23, 0, 4, -3, 2)

    val network = Graph.from(Nil, List(e1, e2))
    val residual = new ResidualNetwork(network, v1, v3)
  }

  /**
   * Contains two paths from source to sink. One is shorter but with higher cost, while the other
   * contains more edges but has a total cheaper cost.
   */
  def longerCheaperPath = new {
    val v1 = new FlowVertex(1)
    val v2 = new FlowVertex(2)
    val v3 = new FlowVertex(3)

    // max flow to avoid negative cycles
    // Costs will be negative in the residual networks backward edges, so giving e1 and e2 higher cost will result in
    // lower (more negative) costs in the residual backward edges
    val e1 = v1 ~> v2 ## (12, 0, 4, 3, 4)
    val e2 = v2 ~> v3 ## (23, 0, 4, 4, 4)
    val e3 = v1 ~> v3 ## (13, 0, 4, 2, 4)

    val network = Graph.from(Nil, List(e1, e2, e3))
    val residual = new ResidualNetwork(network, v1, v3)
  }

  describe("BellmanFordShortestPath") {

    it ("should compute parents for a single network path") {

      Given("a network with a single path going from v1 -> v2 -> v3")
      val f = singlePath
      import f._

      When("computing shortest path parents using the sink")
      val parents: Map[Int, ResidualEdge[Int]] = shortestPath.findParentRelationWithNoCycle(3, residual.underlyingNetwork)

      Then("the parent of v1 should be v2")
      val parentEdge1: ResidualEdge[Int] = parents.get(1).get
      parentEdge1.from should be (2)

      And("the parent of v2 should be v3")
      val parentEdge2: ResidualEdge[Int] = parents.get(2).get
      parentEdge2.from should be (3)

      And("the vertex that all other vertices compute their distances from should not have a parent")
      parents.get(3) should be (None)

    }

    it ("should compute parents based on the cost of the network edges") {

      Given("a network with a path that contains more edges than another, but has lower cost")
      val f = longerCheaperPath
      import f._

      When("computing shortest path parents using the sink")
      val parents: Map[Int, ResidualEdge[Int]] = shortestPath.findParentRelationWithNoCycle(3, residual.underlyingNetwork)

      Then("the parent of v1 should be v2")
      val parentEdge1: ResidualEdge[Int] = parents.get(1).get
      parentEdge1.from should be (2)

      And("the parent of v2 should be v3")
      val parentEdge2: ResidualEdge[Int] = parents.get(2).get
      parentEdge2.from should be (3)

    }

    it ("should throw an exception if a negative cycle is found") {

      Given("a network with a negative cycle")
      val v1 = new FlowVertex(1)
      val v2 = new FlowVertex(2)
      val v3 = new FlowVertex(3)

      // Using capacity 4 and flow 1, backward edges with cost -4 and flow 3 will be present and create negative cycles
      val e1 = v1 ~> v2 ## (12, 0, 4, 3, 1)
      val e2 = v2 ~> v3 ## (23, 0, 4, 4, 1)
      val e3 = v1 ~> v3 ## (13, 0, 4, 2, 1)

      val network = Graph.from(Nil, List(e1, e2, e3))
      val residual = new ResidualNetwork(network, v1, v3)

      When("computing shortest path parents using the sink")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        shortestPath.findParentRelationWithNoCycle(3, residual.underlyingNetwork)
      }

    }

    it ("should not compute parents for vertices that cannot be reached from the specified path-source") {

      Given("a network with edges v1 -> v2 -> v3")
      val v1 = new FlowVertex(1)
      val v2 = new FlowVertex(2)
      val v3 = new FlowVertex(3)

      val e1 = v1 ~> v2 ## (12, 0, 4, -3, 0)
      val e2 = v2 ~> v3 ## (23, 0, 4, -3, 0)

      val network = Graph.from(Nil, List(e1, e2))
      val residual = new ResidualNetwork(network, v1, v3)

      When("computing the shortest path from v2")
      val parents: Map[Int, ResidualEdge[Int]] = shortestPath.findParentRelationWithNoCycle(2, residual.underlyingNetwork)

      Then("the parent of v3 should be v2")
      val parentEdge3: ResidualEdge[Int] = parents.get(3).get
      parentEdge3.from should be (2)

      And("v2 and v1 should not be assigned parents")
      parents.get(2) should be (None)
      parents.get(1) should be (None)
    }

  }
}
