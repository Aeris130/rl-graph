package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.BellmanFordCycleDetection
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowVertex, _}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BellmanFordCycleDetectionSpec extends SpecImports {
  implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val bfDetection = new BellmanFordCycleDetection()

  describe("BellmanFordCycleDetection") {

    it ("should detect a cycle in a graph consisting of a single cycle") {

      Given("a network consisting of a single negative cycle")
      var network = Graph[FlowVertex, FlowEdge]()

      // Vertices
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // The only thing that should matter is ## (id, _, capacity, cost, _, _)
      val a = v1 ~> v2 ## (12, 0, 4, 1, 0)
      val b = v1 ~> v3 ## (13, 0, 4, 4, 0)
      val c = v2 ~> v3 ## (23, 0, 4, 1, 0)

      network += a
      network += b
      network += c

      val residual = new ResidualNetwork(network, v1, v3)

      /* With these settings, the residual network will look like:
       *
       *  a -> b 2 flow, cost 2
       *  b -> a 2 flow, cost -2
       *  c -> a 4 flow, cost -16 <- Full flow, meaning no edge from a to c
       *  b -> c 2 flow, cost 2
       *  c -> b 2 flow, cost -2
       *
       *  A negative cycle exists: a -> b -> c -> a = 2 + 2 + -16 = -12
       */
      residual.setFlow(a, 2)
      residual.setFlow(b, 4)
      residual.setFlow(c, 2)

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v1.id)

      Then("the cycle v1 -> v2 -> v3 should be returned")
      assert(negativeCycle.isDefined, "No negative cycle detected.")

      val v1_v2: ResidualEdge[Int] = residual.residualValues(a)._1.get // Forward edge
      val v2_v3: ResidualEdge[Int] = residual.residualValues(c)._1.get // Forward edge
      val v3_v1: ResidualEdge[Int] = residual.residualValues(b)._2.get // Backward edge

      val cycleToSearchFor = Vector(v1_v2, v2_v3, v3_v1)
      assert(cycleMatches(negativeCycle.get, cycleToSearchFor), "The cycle " + negativeCycle.get + " didn't match any rotation of " + cycleToSearchFor)

    }

    it ("should return None if the graph contains no cycles") {

      Given("a network with negative weights, but no cycle")

      // Vertices
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // Edges
      val a = v1 ~> v2 ## (12, 0, 4, 1, 0)
      val b = v1 ~> v3 ## (13, 0, 4, 1, 0)
      val c = v2 ~> v3 ## (23, 0, 4, 1, 0)

      val network = Graph.from(Vector(v1, v2, v3), Vector(a, b, c))

      /* With max flow in all edges, there won't be any cycle in the graph, negative or otherwise, since the original graph is directed. */
      val residual = new ResidualNetwork(network, v1, v3)
      residual.setFlow(a, 4)
      residual.setFlow(b, 4)
      residual.setFlow(c, 4)

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v3.id)

      Then("the result should be None")
      negativeCycle should be (None)

    }

    it ("should return None of the graph contains a cycle, but it isn't negative") {

      Given("a network with a positive cycle")

      // Vertices
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)

      // Edges
      val a = v1 ~> v2 ## (12, 0, 4, 1, 0)
      val b = v1 ~> v3 ## (13, 0, 4, 0, 0)
      val c = v2 ~> v3 ## (23, 0, 4, 1, 0)

      val network = Graph.from(Vector(v1, v2, v3), Vector(a, b, c))

      /* With max flow in only v1 -> v3, it will only leave a residual backwards edge with
       * negative cost (hence completing the cycle 1 -> 2 -> 3. But since the cost is so low for that edge (0),
       * the cycle will be positive.
       */
      val residual = new ResidualNetwork(network, v1, v3)
      residual.setFlow(a, 0)
      residual.setFlow(b, 4)
      residual.setFlow(c, 0)

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v1.id)

      Then("the result should be None")
      negativeCycle should be (None)

    }

    it ("should detect a cycle consisting of a single edge beginning and ending at the same vertex") {

      Given("a network with a single edge starting and stopping in the same vertex")
      val v1 = new FlowVertex(1, 2)
      val v2 = new FlowVertex(2, -2)
      val e1 = v1 ~> v1 ## (11, 0, 4, 1, 0) // Loop, can hold 4 flow
      val e2 = v1 ~> v2 ## (12, 0, 4, 1, 0)
      val graph = Graph.from(Vector(v1, v2), Vector(e1, e2))
      val residual = new ResidualNetwork(graph, v1, v2)
      residual.setFlow(e1, 2) // Leaves a positive edge with flow 2, cost 2 and a negative edge with flow 2 cost -2

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v1.id)

      Then("the negative self loop e1 should be found")
      val v1_v1: ResidualEdge[Int] = residual.residualValues(e1)._2.get // Backward edge
      val cycleToSearchFor = Vector(v1_v1)

      assert(negativeCycle.isDefined, "No negative cycle detected.")
      assert(cycleMatches(negativeCycle.get, cycleToSearchFor), "The cycle " + negativeCycle.get + " didn't match any rotation of " + cycleToSearchFor)

    }

    /**
     * An example taken from http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=minimumCostFlow2,
     * figure 2(b).
     *
     * Vertex 6 has been omitted, and 5 is used as source.
     */
    it ("should find a negative cycle in a more complex network") {

      Given("a graph with both negative and positive cycles")
      val v1 = new FlowVertex(1, 0)
      val v2 = new FlowVertex(2, 0)
      val v3 = new FlowVertex(3, 0)
      val v4 = new FlowVertex(4, 0)
      val v5 = new FlowVertex(5, 0)

      val a = v1 ~> v2 ## (12, 0, 3, 1, 2)
      val b = v1 ~> v3 ## (13, 0, 3, 4, 3)
      val c = v2 ~> v3 ## (23, 0, 7, 2, 4)
      val d = v3 ~> v5 ## (35, 0, 7, 5, 4)
      val e = v3 ~> v4 ## (34, 0, 5, 2, 2)

      // In the original graph, this edge has cost 1. But we'll have to increase it to 10 to prevent a negative cycle 3->4->5
      val f = v4 ~> v5 ## (45, 0, 3, 10, 0)

      val network = Graph.from(Vector(v1, v2, v3, v4, v5), Vector(a, b, c, d, e, f))
      val residual = new ResidualNetwork(network, v1, v5)

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v1.id)

      Then("the result should be the cycle 1->2->3")
      assert(negativeCycle.isDefined, "No negative cycle detected.")

      val v1_v2: ResidualEdge[Int] = residual.residualValues(a)._1.get // Forward edge
      val v2_v3: ResidualEdge[Int] = residual.residualValues(c)._1.get // Forward edge
      val v3_v1: ResidualEdge[Int] = residual.residualValues(b)._2.get // Backward edge

      val cycleToSearchFor = Vector(v1_v2, v2_v3, v3_v1)
      assert(cycleMatches(negativeCycle.get, cycleToSearchFor), "The cycle " + negativeCycle.get + " didn't match any rotation of " + cycleToSearchFor)

    }

    it ("should not count edges with less or equal flow than the minimum bound into cycles") {

      // A loop with flow that can be decreased will always be found as a negative cycle
      Given("a network with a loop of flow 2 and lower bound 2")
      val v1 = new FlowVertex(1, 2)
      val v2 = new FlowVertex(1, -2)
      val e1 = v1 ~> v2 ## (12, 2, 20, 1, 2) // Loop, can hold 4 flow
      val graph = Graph.from(Vector(v1, v2), Vector(e1))
      val residual = new ResidualNetwork(graph, v1, v2)

      When("computing negative cycles")
      val negativeCycle = bfDetection.findCycle(residual.underlyingNetwork, v1.id)

      Then("no cycle should be found")
      negativeCycle should be (None)

    }
  }

  /**
   * Since the cycle list isn't cyclic, every rotation of the expected cycle is compared to the actual result.
    *
    * @param cycle Resulted cycle.
   * @param cycleToMatch Cycle to look for. Does not contain the initial element last in the list.
   * @return True if some rotation of the expected result matches the actual cycle, otherwise false.
   */
  private def cycleMatches(cycle: Vector[ResidualEdge[Int]], cycleToMatch: Vector[ResidualEdge[Int]]): Boolean = {
    if (cycle.size != cycleToMatch.size) return false
    var current = cycleToMatch
    for (i <- 0 to cycle.size) {
      if (cycle == current) {
        return true
      } else {
        val head = current.head
        current = current.drop(1) :+ head
      }
    }

    false
  }
}
