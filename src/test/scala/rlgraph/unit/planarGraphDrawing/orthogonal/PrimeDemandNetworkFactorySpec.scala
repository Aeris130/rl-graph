package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.PrimeDemandNetworkFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class PrimeDemandNetworkFactorySpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val primeDemandFact = new PrimeDemandNetworkFactory()

  def singleEdge = new {
    val source = new FlowVertex(1, 1)
    val sink = new FlowVertex(2, -1)
    val e1 = source ~> sink ## (12, 2, 6, 0, 0) // Lower bound 2, capacity 6
    val network = Graph.from(List(source, sink), List(e1))
  }

  def doubleEdgeNoLowerBound = new {
    val source = new FlowVertex(1, 1)
    val v1 = new FlowVertex(3, 0)
    val sink = new FlowVertex(2, -1)
    val e1 = source ~> v1 ## (12, 0, 6, 0, 0) // No lower bound
    val e2 = v1 ~> sink ## (13, 2, 6, 0, 0)
    val network = Graph.from(List(source, sink, v1), List(e1, e2))
  }

  def doubleEdge = new {
    val source = new FlowVertex(1, 1)
    val v1 = new FlowVertex(3, 0)
    val sink = new FlowVertex(2, -1)

    val e1 = source ~> v1 ## (12, 2, 6, 0, 0) // No lower bound
    val e2 = v1 ~> sink ## (13, 2, 6, 0, 0)
    val network = Graph.from(List(source, sink, v1), List(e1, e2))
  }

  def doubleSource = new {
    val source1 = new FlowVertex(1, 1)
    val source2 = new FlowVertex(3, 1)
    val sink = new FlowVertex(2, -2)

    val e1 = source1 ~> source2 ## (12, 2, 6, 0, 0) // No lower bound
    val e2 = source2 ~> sink ## (13, 2, 6, 0, 0)
    val network = Graph.from(Nil, List(e1, e2))
  }

  def doubleSink = new {
    val source = new FlowVertex(1, 2)
    val sink1 = new FlowVertex(3, -1)
    val sink2 = new FlowVertex(2, -1)

    val e1 = source ~> sink1 ## (12, 2, 6, 0, 0) // No lower bound
    val e2 = source ~> sink2 ## (13, 2, 6, 0, 0)
    val network = Graph.from(Nil, List(e1, e2))
  }

  describe("PrimeDemandNetworkFactory") {

    it ("should replace every edge in a network with an edge having capacity c, where c is the capacity - lower bounds on the edge") {

      Given("a network with an edge")
      val f = singleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("edge e1 in the saturating network should have capacity 6 - 2")
      saturatingNetwork.get(e1).capacity should be (4)

    }

    it ("should turn the original source and sink into vertices with 0 production/consumption") {

      Given("a network with a source and sink")
      val f = singleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("the original source should have production 0")
      saturatingNetwork.get(source).production should be (0)

      And("the original sink should have production 0")
      saturatingNetwork.get(sink).production should be (0)

    }

    it ("should add an edge between the original sink -> source with lower bound 0 and infinite capacity") {

      Given("a network with a source and sink")
      val f = singleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("there should exist 1 edge between the original source and sink vertices")
      val newSource = saturatingNetwork.get(source)
      val newSink = saturatingNetwork.get(sink)
      val edgesBetweenOldSourceAndSink = saturatingNetwork.edges.filter(e => e.from == newSink && e.to == newSource)

      edgesBetweenOldSourceAndSink.size should be (1)

      And("that edge should have lower bound 0")
      val edge = edgesBetweenOldSourceAndSink.head
      edge.lowerBound  should be (0)

      And("that edge should have infinite capacity")
      assert(edge.hasInfiniteCapacity, "The edge between the old sink -> source did not have infinite capacity.")

    }

    it ("should add an edge between the prime source and a vertex v in every edge u -> v with capacity equal to all incoming lower bounds to v") {

      Given("a network with an edge v1 ~> v2 having lower bound 2")
      val f = singleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("there should be an edge between prime source and vertex v2")
      val primeSource = saturatingNetwork.find( saturatingNetwork.having(node = _.incoming.size == 0)).get
      val edges = saturatingNetwork.edges.filter(e => e.from == primeSource && e.to == saturatingNetwork.get(sink))

      assert(!edges.isEmpty, "No edge found between prime source and vertex v")
      assert(edges.size == 1, "More than one edge found between prime source and vertex v")

      And("that edge should have capacity 2")
      edges.head.capacity should be (2)

      And("that edge should have lower bound 0")
      edges.head.lowerBound should be (0)

    }

    it ("should only add one edge between a prime vertex and an intermediate vertex v") {

      Given("a graph with a vertex v1 having both incoming and outgoing lower bounds through a single edge each")
      val f = doubleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("v1 should have 2 additional edges")
      saturatingNetwork.get(v1).outgoing should have size (2) // 1 old + 1 new
      saturatingNetwork.get(v1).incoming should have size (2) // 1 old + 1 new

      And("v1 should only have one incoming edge from the prime source")
      val primeSource = saturatingNetwork.find( saturatingNetwork.having(node = _.incoming.size == 0)).get
      saturatingNetwork.get(v1).incoming.filter(e => e.from == primeSource) should have size (1)

      And("v1 should only have one outgoing edge to the prime sink")
      val primeSink = saturatingNetwork.find( saturatingNetwork.having(node = _.outgoing.size == 0)).get
      saturatingNetwork.get(v1).outgoing.filter(e => e.to == primeSink) should have size (1)

    }

    it ("should add an edge between a vertex u in (u -> v) and the prime sink with capacity equal to all outgoing lower bounds from u") {

      Given("a network with an edge v1 ~> v2 having lower bound 2")
      val f = singleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("there should be an edge between v1 and prime sink")
      val primeSink = saturatingNetwork.find( saturatingNetwork.having(node = _.outgoing.size == 0)).get
      val edges = saturatingNetwork.edges.filter(e => e.from == saturatingNetwork.get(source) && e.to == primeSink)

      assert(!edges.isEmpty, "No edge found between prime source and vertex v")
      assert(edges.size == 1, "More than one edge found between prime source and vertex v")

      And("that edge should have capacity 2")
      edges.head.capacity should be (2)

      And("that edge should have lower bound 0")
      edges.head.lowerBound should be (0)

    }

    it ("should not add any edges between the prime source and a vertex v if all incoming lower bounds equals 0") {

      Given("a network with an edge v1, u -> v, having 0 lower bound")
      val f = doubleEdgeNoLowerBound
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("there shouldn't be an edge between prime source and v")
      val primeSource = saturatingNetwork.find( saturatingNetwork.having(node = _.incoming.size == 0)).get
      saturatingNetwork.edges.filter(e => e.from == primeSource && e.to == saturatingNetwork.get(v1)) should be ('empty)

    }

    it ("should not add any edges between the a vertex u and prime sink if all outgoing lower bounds in u equals 0") {

      Given("a network with an edge u -> v having 0 lower bound")
      val f = doubleEdgeNoLowerBound
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("there shouldn't be an edge between prime source and v")
      val primeSink = saturatingNetwork.find( saturatingNetwork.having(node = _.outgoing.size == 0)).get
      saturatingNetwork.edges.filter(e => e.from == saturatingNetwork.get(source) && e.to == primeSink) should be ('empty)

    }

    it ("should set the production of the prime source to the total capacity of its outgoing edges") {

      Given("a network with lower bounds")
      val f = doubleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("the production of the prime source should equal the total capacity of all outgoing edges")
      val primeSource = saturatingNetwork.nodes.find(_.incoming.isEmpty).get
      var totalCapacity = 0
      val sourceNode = saturatingNetwork.get(primeSource)
      sourceNode.outgoing.foreach(e => totalCapacity += e.capacity)

      sourceNode.production should equal (totalCapacity)

    }

    it ("should set the consumption of the prime sink to the total capacity of its incoming edges") {

      Given("a network with lower bounds")
      val f = doubleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("the consumption of the prime sink should equal the total capacity of all incoming edges")
      val primeSink = saturatingNetwork.nodes.find(_.outgoing.isEmpty).get
      var totalCapacity = 0
      val sinkNode = saturatingNetwork.get(primeSink)
      sinkNode.incoming.foreach(e => totalCapacity += e.capacity)

      sinkNode.production should equal (-totalCapacity)

    }

    it ("should set the production of the prime source to equal the consumption of the prime sink") {

      Given("a network with lower bounds")
      val f = doubleEdge
      import f._

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("the prime source should have the same production as the negative consumption of the prime sink")
      val primeSource = saturatingNetwork.nodes.find(_.incoming.isEmpty).get
      val primeSink = saturatingNetwork.nodes.find(_.outgoing.isEmpty).get

      primeSource.production should equal (-primeSink.production)

    }

    it ("should throw an exception if the specified graph has more than one source") {

      Given("a network with two sources")
      val f = doubleSource
      import f._

      When("adding prime network vertices and edges")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        primeDemandFact.augmentNetwork(network, source1, sink)
      }

    }

    it ("should throw an exception if the specified graph has more than one sink") {

      Given("a network with two sink")
      val f = doubleSink
      import f._

      When("adding prime network vertices and edges")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        primeDemandFact.augmentNetwork(network, source, sink1)
      }

    }

    it ("should not throw an exception if multiple graph components exists connected to a source/sink") {

      Given("a network with multiple edges connected to both source and sink")
      val source = new FlowVertex(1, 1)
      val v1 = new FlowVertex(3, 0)
      val sink = new FlowVertex(2, -1)

      val e1 = source ~> source ## (12, 2, 6, 0, 0) // No lower bound
      val e2 = sink ~> sink ## (13, 2, 6, 0, 0)
      val e3 = source ~> sink ## (14, 2, 3, 0, 0)
      val network = Graph.from(List(source, sink, v1), List(e1, e2, e3))

      When("adding prime network vertices and edges")
      val saturatingNetwork = primeDemandFact.augmentNetwork(network, source, sink)

      Then("no exception should be thrown")
      // Simply getting here means the test succeeded
      assert(true)

    }

  }

}
