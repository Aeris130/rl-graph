package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.{Segment, SegmentOrientation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.DualMinimumCostFlow.DualNetworkFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdge, FlowVertex}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.VertexWrapper
import rlgraph.SpecImports

import scalax.collection.edge.Implicits._
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

class DualNetworkFactorySpec extends SpecImports {
  val dualFactory = new DualNetworkFactory()

  def exampleShapeGraph = new {
    val v1 = new VertexWrapper(1, "v1")
    val v2 = new VertexWrapper(2, "v2")
    val v3 = new VertexWrapper(3, "v3")
    val v4 = new VertexWrapper(4, "v4")
    val v5 = new VertexWrapper(5, "v5")
    val v6 = new VertexWrapper(6, "v6")
    val v7 = new VertexWrapper(7, "v7")

    val S1 = new Segment(Horizontal, v1)
    val S2 = new Segment(Horizontal, v2, v4, v7)
    val S3 = new Segment(Horizontal, v3, v5)
    val S4 = new Segment(Horizontal, v6)

    val e1 = (S1 ~%+#> S2)(1, "S1 to S2")
    val e2 = (S4 ~%+#> S2)(2, "S4 to S2")
    val e3 = (S2 ~%+#> S3)(3, "S2 to S3")
    val e4 = (S2 ~%+#> S3)(4, "S2 to S3 # 2")

    val shapeGraph: Graph[Segment[String], WLkDiEdge] = Graph.from(Nil, List(e1, e2, e3, e4))
  }

  describe("DualNetworkFactory") {

    it ("should set the production of a flow vertex equal to the negative net degree of the segment-vertex it is created from") {

      Given("a shape graph")
      val f = exampleShapeGraph
      import f._

      When("computing the dual network")
      val dualInfo = dualFactory.computeDual(shapeGraph)
      val network: Graph[FlowVertex, FlowEdge] = dualInfo._1
      val vertices: Map[Segment[String], FlowVertex] = dualInfo._2.map(_.swap)

      Then("the vertex corresponding to S1 should have production 1")
      val flowV1 = vertices.get(S1).get
      val s1Vertex: FlowVertex = network.get(flowV1)
      s1Vertex.production should be (1)

      And("the vertex corresponding to S2 should have production 0")
      val flowV2 = vertices.get(S2).get
      val s2Vertex: FlowVertex = network.get(flowV2)
      s2Vertex.production should be (0)

      And("the vertex corresponding to S3 should have production -2")
      val flowV3 = vertices.get(S3).get
      val s3Vertex: FlowVertex = network.get(flowV3)
      s3Vertex.production should be (-2)

    }

    it ("should set the capacity of all edges equal to the total production of all sources") {

      Given("a shape graph")
      val f = exampleShapeGraph
      import f._

      When("computing the dual network")
      val dualInfo = dualFactory.computeDual(shapeGraph)
      val network: Graph[FlowVertex, FlowEdge] = dualInfo._1
      val edges: Map[WLkDiEdge[Segment[String]], FlowEdge[FlowVertex]] = dualInfo._3.map(_.swap)
      val totalProduction = 2 // Segment 1 and 6 has production 1

      Then("all 4 edges should have max capacity 1")
      val edge1 = edges.get(e1).get
      val netE1 = network.get(edge1)
      val edge2 = edges.get(e2).get
      val netE2 = network.get(edge2)
      val edge3 = edges.get(e3).get
      val netE3 = network.get(edge3)
      val edge4 = edges.get(e4).get
      val netE4 = network.get(edge4)

      netE1.capacity should be (totalProduction)
      netE2.capacity should be (totalProduction)
      netE3.capacity should be (totalProduction)
      netE4.capacity should be (totalProduction)

    }

    it ("should set the cost of each flow edge to the negative cost of the equivalent shape edge") {

      Given("a shape graph with edges having weights 1, 2, 3 and 4")
      val f = exampleShapeGraph
      import f._

      When("computing the dual network")
      val dualInfo = dualFactory.computeDual(shapeGraph)
      val network: Graph[FlowVertex, FlowEdge] = dualInfo._1
      val edges: Map[WLkDiEdge[Segment[String]], FlowEdge[FlowVertex]] = dualInfo._3.map(_.swap)

      Then("edge corresponding to e1 should have cost -1")
      val edge1 = edges.get(e1).get
      val netE1 = network.get(edge1)
      netE1.cost should be (-1)

      And("edge corresponding to e2 should have cost -2")
      val edge2 = edges.get(e2).get
      val netE2 = network.get(edge2)
      netE2.cost should be (-2)

      And("edge corresponding to e3 should have cost -3")
      val edge3 = edges.get(e3).get
      val netE3 = network.get(edge3)
      netE3.cost should be (-3)

      And("edge corresponding to e3 should have cost -3")
      val edge4 = edges.get(e4).get
      val netE4 = network.get(edge4)
      netE4.cost should be (-4)

    }

    it ("should set the lower bound of all edges to 0") {

      Given("a shape graph with 4 edges")
      val f = exampleShapeGraph
      import f._

      When("computing the dual network")
      val dualInfo = dualFactory.computeDual(shapeGraph)
      val network: Graph[FlowVertex, FlowEdge] = dualInfo._1

      Then("all edges should have lower bound 0")
      val edges = network.edges.toVector
      edges(0).lowerBound should be (0)
      edges(1).lowerBound should be (0)
      edges(2).lowerBound should be (0)
      edges(3).lowerBound should be (0)

    }

    it ("should set the flow of all edges to 0") {

      Given("a shape graph with 4 edges")
      val f = exampleShapeGraph
      import f._

      When("computing the dual network")
      val dualInfo = dualFactory.computeDual(shapeGraph)
      val network: Graph[FlowVertex, FlowEdge] = dualInfo._1
      val edges: Map[WLkDiEdge[Segment[String]], FlowEdge[FlowVertex]] = dualInfo._3.map(_.swap)

      Then("the flow of all edges should be 0")
      val edge1 = edges.get(e1).get
      val netE1 = network.get(edge1)
      val edge2 = edges.get(e2).get
      val netE2 = network.get(edge2)
      val edge3 = edges.get(e3).get
      val netE3 = network.get(edge3)
      val edge4 = edges.get(e4).get
      val netE4 = network.get(edge4)

      netE1.flow should be (0)
      netE2.flow should be (0)
      netE3.flow should be (0)
      netE4.flow should be (0)
    }
  }
}
