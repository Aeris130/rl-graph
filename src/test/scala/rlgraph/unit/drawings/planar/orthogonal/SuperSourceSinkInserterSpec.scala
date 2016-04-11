package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdge, FlowEdgeAssoc, FlowVertex, SuperSourceSinkInserter}
import rlgraph.SpecImports

import scala.collection.Set
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * All tests are ran twice, once for sources and once for sinks.
 */
class SuperSourceSinkInserterSpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val inserter = new SuperSourceSinkInserter()

  def twoSourcesToSink = new {
    val v1 = new FlowVertex(1, 2) // Source
    val v2 = new FlowVertex(2, 1) // Source
    val v3 = new FlowVertex(3, -3) // Sink
    val e1 = v1 ~> v3 ## (13, 0, 0, 0, 0)
    val e2 = v2 ~> v3 ## (23, 0, 0, 0, 0)
    val network = Graph.from(List(v1, v2, v3), List(e1, e2))
  }

  def oneSourceToTwoSinks = new {
    val v1 = new FlowVertex(1, 5) // Source
    val v2 = new FlowVertex(2, -2) // Sink
    val v3 = new FlowVertex(3, -3) // Sink
    val e1 = v1 ~> v2 ## (12, 0, 0, 0, 0)
    val e2 = v1 ~> v3 ## (13, 0, 0, 0, 0)
    val network = Graph.from(List(v1, v2, v3), List(e1, e2))
  }

  def singleSourceSingleSink = new {
    val v1 = new FlowVertex(1, 1) // Source
    val v2 = new FlowVertex(2, -1) // Sink
    val e1 = v1 ~> v2 ## (0, 0, 0, 0, 0)
    val network = Graph.from(List(v1, v2), List(e1))
  }

  def twoSourceTwoSink = new {
    val v1 = new FlowVertex(1, 5) // Source
    val v2 = new FlowVertex(2, 5) // Source
    val v3 = new FlowVertex(3, 0) // Intermediate
    val v4 = new FlowVertex(4, -5) // Sink
    val v5 = new FlowVertex(5, -5) // Sink

    val e1 = v1 ~> v3 ## (13, 0, 0, 0, 0)
    val e2 = v2 ~> v3 ## (23, 0, 0, 0, 0)
    val e3 = v3 ~> v4 ## (34, 0, 0, 0, 0)
    val e4 = v3 ~> v5 ## (35, 0, 0, 0, 0)

    val network = Graph.from(Nil, List(e1, e2, e3, e4))
  }

  /* The intermediary vertex has edges going back the source, and the sink has edges going back to the intermediary. */
  def sourceAndSinkWithEdgesToIntermediary = new {
    val v1 = new FlowVertex(1, 5) // Source
    val v2 = new FlowVertex(2, 5) // Source
    val v3 = new FlowVertex(3, 0) // Intermediate
    val v4 = new FlowVertex(4, -5) // Sink
    val v5 = new FlowVertex(5, -5) // Sink

    val e1 = v1 ~> v3 ## (13, 0, 0, 0, 0)
    val e2 = v3 ~> v1 ## (31, 0, 0, 0, 0)
    val e3 = v2 ~> v3 ## (23, 0, 0, 0, 0)
    val e4 = v3 ~> v2 ## (32, 0, 0, 0, 0)
    val e5 = v3 ~> v4 ## (34, 0, 0, 0, 0)
    val e6 = v4 ~> v3 ## (43, 0, 0, 0, 0)
    val e7 = v3 ~> v5 ## (35, 0, 0, 0, 0)
    val e8 = v5 ~> v3 ## (53, 0, 0, 0, 0)

    val network = Graph.from(Nil, List(e1, e2, e3, e4, e5, e6, e7, e8))
  }

  describe("SuperSourceSinkInserter") {

    /*
     * Misc tests --------------------------------------------------------------------------
     */
    it ("should set the cost of each edge going from/to the super-source/sink as 0 by default") {

      Given("a network with multiple sources and sinks")
      val f = twoSourceTwoSink
      import f._

      When("adding super-sources/sinks")
      val networkWithSS = inserter.addSourceAndSink(network)

      Then("every outgoing edge from the super-source should have cost 0")
      val source = networkWithSS.nodes.find(_.production > 0).get
      val outgoingEdges = source.outgoing.toVector
      outgoingEdges(0).cost should be (0)
      outgoingEdges(1).cost should be (0)

      And("every incoming edge to the super-sink should have cost 0")
      val sink = networkWithSS.nodes.find(_.production < 0).get
      val incomingEdges = sink.incoming.toVector
      incomingEdges(0).cost should be (0)
      incomingEdges(1).cost should be (0)

    }


    /*
     *
     * Source tests  -------------------------------------------------------------------------
     *
     */

    // Lower edge case
    it ("should not add any sources to a network with a single source") {

      Given("a network with a single source")
      val f = singleSourceSingleSink
      import f._

      When("attempting to add a super-source")
      val networkWithSS = inserter.addSource(network)

      Then("the returned network should be the same as the one inserted")
      networkWithSS should equal (network)

    }

    // In-between edge case
    it ("should connect two sources") {

      Given("a graph with two sources pointing at a sink")
      val f = twoSourcesToSink
      import f._

      When("adding a super-source to the two source vertices")
      val networkWithSS = inserter.addSource(network)

      Then("the number of edges should increase by 2")
      networkWithSS.edges.size should be (network.edges.size + 2)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (network.nodes.size + 1)

      And("there should only be a single vertex with production > 0")
      val sources = networkWithSS.nodes.filter(_.production > 0)
      sources.size should be (1)

      And("that source vertex should have two outgoing edges")
      sources.head.outDegree should be (2)

      And("that source vertex should have outgoing edges to vertices v1 and v2")
      val superToV1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v1))
      val superToV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v2))

      superToV1 should have size (1)
      superToV2 should have size (1)

    }

    // Upper edge case
    it ("should connect 3 sources") {

      Given("a graph with 3 sources pointing at a sink")
      val v1 = new FlowVertex(1, 1) // Source
      val v2 = new FlowVertex(2, 1) // Source
      val v3 = new FlowVertex(3, 1) // Source
      val v4 = new FlowVertex(4, -3) // Sink
      val e1 = v1 ~> v4 ## (13, 0, 0, 0, 0)
      val e2 = v2 ~> v4 ## (23, 0, 0, 0, 0)
      val e3 = v3 ~> v4 ## (34, 0, 0, 0, 0)
      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3))

      When("adding a super-source to the three source vertices")
      val networkWithSS = inserter.addSource(network)

      Then("the number of edges should increase by 3")
      networkWithSS.edges.size should be (network.edges.size + 3)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (network.nodes.size + 1)

      And("there should only be a single vertex with production > 0")
      val sources = networkWithSS.nodes.filter(_.production > 0)
      sources.size should be (1)

      And("that source vertex should have three outgoing edges")
      sources.head.outDegree should be (3)

      And("that source vertex should have outgoing edges to vertices v1, v2 and v3")
      val superToV1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v1))
      val superToV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v2))
      val superToV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v3))

      superToV1 should have size (1)
      superToV2 should have size (1)
      superToV3 should have size (1)

    }

    it ("should set the production of regular sources to 0") {

      Given("a graph with two sources pointing at a sink")
      val f = twoSourcesToSink
      import f._

      When("adding a super-source to the three source vertices")
      val networkWithSS = inserter.addSource(network)

      Then("all vertices being pointed at by the super source should have production 0")
      val source = networkWithSS.nodes.filter(_.production > 0).head // The super source
      val prod0Vertices = networkWithSS.edges.filter(x => x.from == networkWithSS.get(source) && x.to.production == 0)

      prod0Vertices should have size (2)

      And("these vertices should be v1 and v2")
      val vertices: Set[Graph[FlowVertex, FlowEdge]#NodeT] = for { edge <- prod0Vertices } yield edge.to
      val innerV1: Graph[FlowVertex, FlowEdge]#NodeT = networkWithSS.get(v1)
      val innerV2: Graph[FlowVertex, FlowEdge]#NodeT = networkWithSS.get(v2)

      vertices should contain (innerV1)
      vertices should contain (innerV2)

    }

    it ("should set the production of the super source equal to the combined production of all regular sources") {

      Given("a graph with two sources having the combined production 3 pointing at a sink")
      val f = twoSourcesToSink
      import f._

      When("adding a super-source to the three source vertices")
      val networkWithSS = inserter.addSource(network)

      Then("the super source should have production 3")
      val source = networkWithSS.nodes.filter(_.production > 0).head
      source.production should be (3)

    }

    it ("should preserve data from edges going out from the original sources") {

      Given("a graph with two sources pointing at a sink")

      // Custom graph to set edge values
      val v1 = new FlowVertex(1, 2) // Source
      val v2 = new FlowVertex(2, 1) // Source
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v3 ## (13, 1, 12, 3, 2, Option((22, 33)))
      val e2 = v2 ~> v3 ## (23, 5, 16, 7, 1, Option((44, 55)))
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("adding a super-source to the three source vertices")
      val networkWithSS = inserter.addSource(network)

      Then("the new graph should be different from the old")
      networkWithSS should not equal (network)

      And("the edge e1 should have lower bound 1, capacity 2, cost 3, flow 4 and crossing (22, 33)")
      val e1AfterInsert = networkWithSS.get(e1)
      e1AfterInsert.lowerBound should be (1)
      e1AfterInsert.capacity should be (12)
      e1AfterInsert.cost should be (3)
      e1AfterInsert.flow should be (2)
      e1AfterInsert.crosses should be (Some((22, 33)))

      Then("the edge e2 should have lower bound 5, capacity 6, cost 7, flow 8 and crossing (44, 55)")
      val e2AfterInsert = networkWithSS.get(e2)
      e2AfterInsert.lowerBound should be (5)
      e2AfterInsert.capacity should be (16)
      e2AfterInsert.cost should be (7)
      e2AfterInsert.flow should be (1)
      e2AfterInsert.crosses should be (Some((44, 55)))

    }

    it ("should insert a super source to sources connected to each other") {

      Given("a graph with 2 sources connected to a sink, and one source being connected to the other")
      val f = twoSourcesToSink
      import f._
      val edgeBetweenSources = v1 ~> v2 ## (11, 0, 0, 0, 0)
      val nGraph = network + edgeBetweenSources

      When("adding a super-source to the two source vertices")
      val networkWithSS = inserter.addSource(nGraph)

      Then("the number of edges should increase by 2")
      networkWithSS.edges.size should be (nGraph.edges.size + 2)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (nGraph.nodes.size + 1)

      And("there should only be a single vertex with production > 0")
      val sources = networkWithSS.nodes.filter(_.production > 0)
      sources.size should be (1)

      And("that source vertex should have two outgoing edges")
      sources.head.outDegree should be (2)

      And("that source vertex should have outgoing edges to vertices v1 and v2")
      val superToV1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v1))
      val superToV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(sources.head) && x.to == networkWithSS.get(v2))

      superToV1 should have size (1)
      superToV2 should have size (1)

    }

    it ("should assign a maximum capacity to each extra edge based on the regular source it connects to") {

      Given("a graph with two sources pointing at a sink, with source v1 having production 2, and v2 production 1")
      val f = twoSourcesToSink
      import f._

      When("adding a super-source to the two source vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSource(network)
      val source = networkWithSS.nodes.filter(_.production > 0).head

      Then("the edge from the super-source to v1 should have max capacity 2")
      val superToV1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(source) && x.to == networkWithSS.get(v1)).head

      superToV1.capacity should be (2)

      Then("the edge from the super-source to v2 should have max capacity 1")
      val superToV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(source) && x.to == networkWithSS.get(v2)).head

      superToV2.capacity should be (1)

    }

    it ("should add flow to the new outgoing edges from the super source, according to the flow connected to the previous source") {

      Given("a graph with two sources pointing at a sink, with source v1 sending 2 flow along its edge, and v2 sending 3 flow")
      val v1 = new FlowVertex(1, 2) // Source
      val v2 = new FlowVertex(2, 1) // Source
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v3 ## (13, 0, 2, 0, 2)
      val e2 = v2 ~> v3 ## (23, 0, 1, 0, 1)
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("adding a super-source to the two source vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSource(network)
      val source = networkWithSS.nodes.filter(_.production > 0).head

      Then("the edge from the super-source to v1 should have flow 2")
      val superToV1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(source) && x.to == networkWithSS.get(v1)).head

      superToV1.flow should be (2)

      Then("the edge from the super-source to v2 should have flow 3")
      val superToV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(source) && x.to == networkWithSS.get(v2)).head

      superToV2.flow should be (1)

    }

    it ("should restore edges going from an intermediary vertex to the source") {

      Given("a network with edges going from an intermediary vertex v3 to the sources v1 and v2")
      val f = sourceAndSinkWithEdgesToIntermediary
      import f._

      When("adding a super-source to the two source vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSource(network)

      Then("vertices v1 and v2 should still have edges incoming from v3")
      val v3tov1 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(v1))
      val v3tov2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(v2))

      v3tov1.size should be (1)
      v3tov2.size should be (1)

    }

   /*
    *
    * Sink tests  -------------------------------------------------------------------------
    *
    */

    // Lower edge case
    it ("should not add any sinks to a network with a single sink") {

      Given("a network with a single source")
      val f = singleSourceSingleSink
      import f._

      When("attempting to add a super-source")
      val networkWithSS = inserter.addSink(network)

      Then("the returned network should be the same as the one inserted")
      networkWithSS should equal (network)

    }

    it ("should connect two sinks") {

      Given("a graph with one source pointing at two sinks")
      val f = oneSourceToTwoSinks
      import f._

      When("adding a super-sink to the two sink vertices")
      val networkWithSS = inserter.addSink(network)

      Then("the number of edges should increase by 2")
      networkWithSS.edges.size should be (network.edges.size + 2)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (network.nodes.size + 1)

      And("there should only be a single vertex with production < 0")
      val sources = networkWithSS.nodes.filter(_.production < 0)
      sources.size should be (1)

      And("that sink vertex should have two incoming edges")
      sources.head.inDegree should be (2)

      And("that sink vertex should have incoming edges from vertices v2 and v3")
      val superFromV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v2) && x.to == networkWithSS.get(sources.head))
      val superFromV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(sources.head))

      superFromV2 should have size (1)
      superFromV3 should have size (1)

    }

    // Upper edge case
    it ("should connect 3 sinks") {

      Given("a graph with 3 sinks receiving flow from a source")
      val v1 = new FlowVertex(1, 6) // Source
      val v2 = new FlowVertex(2, -1) // Sink
      val v3 = new FlowVertex(3, -2) // Sink
      val v4 = new FlowVertex(4, -3) // Sink
      val e1 = v1 ~> v2 ## (13, 0, 0, 0, 0)
      val e2 = v1 ~> v3 ## (23, 0, 0, 0, 0)
      val e3 = v1 ~> v4 ## (34, 0, 0, 0, 0)
      val network = Graph.from(List(v1, v2, v3, v4), List(e1, e2, e3))

      When("adding a super-sink to the three sink vertices")
      val networkWithSS = inserter.addSink(network)

      Then("the number of edges should increase by 3")
      networkWithSS.edges.size should be (network.edges.size + 3)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (network.nodes.size + 1)

      And("there should only be a single vertex with production < 0")
      val sources = networkWithSS.nodes.filter(_.production < 0)
      sources.size should be (1)

      And("that sink vertex should have three incoming edges")
      sources.head.inDegree should be (3)

      And("that sink vertex should have incoming edges from vertices v2, v3 and v4")
      val superFromV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v2) && x.to == networkWithSS.get(sources.head))
      val superFromV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(sources.head))
      val superFromV4 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v4) && x.to == networkWithSS.get(sources.head))

      superFromV2 should have size (1)
      superFromV3 should have size (1)
      superFromV4 should have size (1)

    }

    it ("should set the production of regular sinks to 0") {

      Given("a graph with one source pointing at two sinks")
      val f = oneSourceToTwoSinks
      import f._

      When("adding a super-sink to the two sink vertices")
      val networkWithSS = inserter.addSink(network)

      Then("all vertices pointing at the super sink should have production 0")
      val sink = networkWithSS.nodes.filter(_.production < 0).head // The super sink
      val prod0Vertices = networkWithSS.edges.filter(x => x.to == networkWithSS.get(sink) && x.from.production == 0)
      prod0Vertices should have size (2)

      And("these vertices should be v2 and v3")
      val vertices: Set[Graph[FlowVertex, FlowEdge]#NodeT] = for { edge <- prod0Vertices } yield edge.from
      val innerV2: Graph[FlowVertex, FlowEdge]#NodeT = networkWithSS.get(v2)
      val innerV3: Graph[FlowVertex, FlowEdge]#NodeT = networkWithSS.get(v3)

      vertices should contain (innerV2)
      vertices should contain (innerV3)
    }

    it ("should set the production of the super sink equal to the combined production of all regular sinks") {

      Given("a graph with two sinks having the combined production -5")
      val f = oneSourceToTwoSinks
      import f._

      When("adding a super-sink to the three sink vertices")
      val networkWithSS = inserter.addSink(network)

      Then("the super sink should have production -5")
      val source = networkWithSS.nodes.filter(_.production < 0).head
      source.production should be (-5)

    }

    it ("should preserve data from edges going into the original sinks") {

      Given("a graph with a source pointing at two sinks")

      // Custom graph to set edge values
      val v1 = new FlowVertex(1, 5) // Source
      val v2 = new FlowVertex(2, -2) // Sink
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v3 ## (13, 1, 12, 3, 3, Option((22, 33)))
      val e2 = v1 ~> v2 ## (23, 5, 16, 7, 2, Option((44, 55)))
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("adding a super-sink to the three sink vertices")
      val networkWithSS = inserter.addSink(network)

      Then("the new graph should be different from the old")
      networkWithSS should not equal (network)

      And("the edge e1 should have lower bound 1, capacity 2, cost 3, flow 4 and crossing (22, 33)")
      val e1AfterInsert = networkWithSS.get(e1)
      e1AfterInsert.lowerBound should be (1)
      e1AfterInsert.capacity should be (12)
      e1AfterInsert.cost should be (3)
      e1AfterInsert.flow should be (3)
      e1AfterInsert.crosses should be (Some((22, 33)))

      Then("the edge e2 should have lower bound 5, capacity 6, cost 7, flow 8 and crossing (44, 55)")
      val e2AfterInsert = networkWithSS.get(e2)
      e2AfterInsert.lowerBound should be (5)
      e2AfterInsert.capacity should be (16)
      e2AfterInsert.cost should be (7)
      e2AfterInsert.flow should be (2)
      e2AfterInsert.crosses should be (Some((44, 55)))

    }

    it ("should insert a super sink to sinks connected to each other") {

      Given("a graph with 2 sinks connected to a source, and one sink being connected to the other")
      val f = oneSourceToTwoSinks
      import f._
      val edgeBetweenSinks = v2 ~> v3 ## (23, 0, 0, 0, 0)
      val nGraph = network + edgeBetweenSinks

      When("adding a super-sink to the two sink vertices")
      val networkWithSS = inserter.addSink(nGraph)

      Then("the number of edges should increase by 2")
      networkWithSS.edges.size should be (nGraph.edges.size + 2)

      And("the number of vertices should increase by 1")
      networkWithSS.nodes.size should be (nGraph.nodes.size + 1)

      And("there should only be a single vertex with production < 0")
      val sources = networkWithSS.nodes.filter(_.production < 0)
      sources.size should be (1)

      And("that sink vertex should have two incoming edges")
      sources.head.inDegree should be (2)

      And("that sink vertex should have incoming edges from vertices v2 and v3")
      val superFromV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v2) && x.to == networkWithSS.get(sources.head))
      val superFromV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(sources.head))

      superFromV2 should have size (1)
      superFromV3 should have size (1)

    }

    it ("should assign a maximum capacity to each extra edge based on the regular sink it connects from") {

      Given("a graph with a source pointing at two sinks, with sink v2 having production -2, and v3 production -3")
      val f = oneSourceToTwoSinks
      import f._

      When("adding a super-source to the two source vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSink(network)
      val source = networkWithSS.nodes.filter(_.production < 0).head

      Then("the edge from the super-source to v1 should have max capacity 2")
      val superFromV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v2) && x.to == networkWithSS.get(source)).head

      superFromV2.capacity should be (2)

      Then("the edge from the super-source to v2 should have max capacity 1")
      val superFromV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(source)).head

      superFromV3.capacity should be (3)

    }

    it ("should add flow to the new incoming edges to the super sink, according to the flow consumbed by the previous sink") {

      Given("a network with a source pointing at two sinks, with sink v2 receiving 2 flow, and v3 receiving 3")
      val v1 = new FlowVertex(1, 5) // Source
      val v2 = new FlowVertex(2, -2) // Sink
      val v3 = new FlowVertex(3, -3) // Sink
      val e1 = v1 ~> v2 ## (12, 0, 2, 0, 2)
      val e2 = v1 ~> v3 ## (13, 0, 3, 0, 3)
      val network = Graph.from(List(v1, v2, v3), List(e1, e2))

      When("adding a super-source to the two source vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSink(network)
      val source = networkWithSS.nodes.filter(_.production < 0).head

      Then("the edge from the super-source to v1 should have max capacity 2")
      val superFromV2 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v2) && x.to == networkWithSS.get(source)).head

      superFromV2.flow should be (2)

      Then("the edge from the super-source to v2 should have max capacity 1")
      val superFromV3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v3) && x.to == networkWithSS.get(source)).head

      superFromV3.flow should be (3)

    }

    it ("should restore edges going from a sink to an intermediary vertex") {

      Given("a network with edges going from sinks v4, v5 to an intermediary vertex v3")
      val f = sourceAndSinkWithEdgesToIntermediary
      import f._

      When("adding a super-sink to the two sink vertices")
      val networkWithSS: Graph[FlowVertex, FlowEdge] = inserter.addSink(network)

      Then("vertices v4 and v5 should still have edges going to v3")
      val v4tov3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v4) && x.to == networkWithSS.get(v3))
      val v5tov3 = networkWithSS.edges.filter(x => x.from == networkWithSS.get(v5) && x.to == networkWithSS.get(v3))

      v4tov3.size should be (1)
      v5tov3.size should be (1)

    }

  }
}
