package rlgraph.unit.util.graphConverters

import graphStructure.{Edge, Graph => JGraphEd, Node}
import net.cyndeline.rlgraph.util.graphConverters.ConverterData
import net.cyndeline.rlgraph.util.graphConverters.jgraphed.{EdgeWrapper, JGraphEdConverter, NodeWrapper}
import rlgraph.SpecImports

import scala.collection.JavaConverters._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class JGraphEdConverterSpec extends SpecImports {
  private val converter = new JGraphEdConverter[Int, UnDiEdge]()

  // JGraphEd uses Vectors to store edges and nodes, this allows usage of exists(...) etc. to simplify tests
  implicit def vectorToList(v: java.util.Vector[_]) = v.asScala

  describe("JGraphEdConverter") {

    /*
     *
     *  Scala --> JGraphEd
     *
     */
    it ("should convert an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("converting the graph")
      val jGraphEd = converter.convertTo(graph).convertedGraph

      Then("the resulting graph should have no nodes or edges")
      jGraphEd.getNodes.size should be (0)
      jGraphEd.getEdges.size should be (0)

    }

    it ("should convert nodes with no incoming/outgoing edges") {

      Given("a graph with nodes having no edges")
      val graph = Graph[Int, UnDiEdge](1, 2, 3)

      When("converting the graph")
      val jGraphEd = converter.convertTo(graph).convertedGraph

      Then("the resulting graph should have 3 nodes and no edges")
      jGraphEd.getNodes.size should be (3)
      jGraphEd.getEdges.size should be (0)

    }

    it ("should store original vertices in the converted nodes") {

      Given("a graph with with 3 vertices")
      val graph = Graph[Int, UnDiEdge](1, 2, 3)

      When("converting the graph")
      val jGraphEd = converter.convertTo(graph).convertedGraph

      Then("there should exist a node with original vertex 1, 2 and 3")
      jGraphEd.getNodes.exists(n => n.asInstanceOf[Node].getExtender.asInstanceOf[NodeWrapper[Int]].vertex == 1) should be (true)
      jGraphEd.getNodes.exists(n => n.asInstanceOf[Node].getExtender.asInstanceOf[NodeWrapper[Int]].vertex == 2) should be (true)
      jGraphEd.getNodes.exists(n => n.asInstanceOf[Node].getExtender.asInstanceOf[NodeWrapper[Int]].vertex == 3) should be (true)

    }

    it ("should convert edges") {

      Given("a graph with 2 edges")
      val graph = Graph(1~2, 2~3)

      When("converting the graph")
      val jGraphEd = converter.convertTo(graph).convertedGraph

      Then("the resulting graph should have 2 edges")
      jGraphEd.getEdges.size should be (2)

    }

    // Tests both node connectivity and original edges since the edge extenders are retrieved as part of the test anyway
    it ("should store original edges in the converted edges") {

      Given("a graph with three nodes having edges between them")
      val edge1 = 1~2
      val edge2 = 2~3
      val graph = Graph(edge1, edge2)

      When("converting the graph")
      val jGraphEd = converter.convertTo(graph).convertedGraph

      Then("there should be an edge having edge1 as its original and 1 / 2 as its vertices")
      val jGraphEdEdge1 = jGraphEd.getEdgeExtenders.find(ext => ext.asInstanceOf[EdgeWrapper[UnDiEdge[Int]]].edge == edge1).getOrElse {
        fail("No edge between nodes 1 and 2 found in the converted graph")
      }.asInstanceOf[EdgeWrapper[UnDiEdge[Int]]]

      jGraphEdEdge1.getStartNode.getNode.getExtender.asInstanceOf[NodeWrapper[Int]].vertex should be (1)
      jGraphEdEdge1.getEndNode.getNode.getExtender.asInstanceOf[NodeWrapper[Int]].vertex should be (2)

      And("there should be an edge having edge2 as its original")
      val jGraphEdEdge2 = jGraphEd.getEdgeExtenders.find(ext => ext.asInstanceOf[EdgeWrapper[UnDiEdge[Int]]].edge == edge2).getOrElse {
        fail("No edge between nodes 2 and 3 found in the converted graph")
      }.asInstanceOf[EdgeWrapper[UnDiEdge[Int]]]

      jGraphEdEdge2.getStartNode.getNode.getExtender.asInstanceOf[NodeWrapper[Int]].vertex should be (2)
      jGraphEdEdge2.getEndNode.getNode.getExtender.asInstanceOf[NodeWrapper[Int]].vertex should be (3)

    }

    /*
     *
     *  JGraphEd --> Scala
     *
     */

    it ("should convert an empty graph to scala") {

      Given("an empty graph")
      val graph = new JGraphEd()

      When("converting back to scala")
      val data = ConverterData[Int, UnDiEdge, JGraphEd, Node, Edge](graph, Map(), Map())
      val scalaGraph = converter.convertFrom(data)

      Then("the resulting graph should be empty")
      scalaGraph should equal (Graph[Int, UnDiEdge]())

    }

    it ("should convert nodes back to scala") {

      Given("a graph with three nodes")
      val original = Graph[Int, UnDiEdge](1, 2, 3)
      val graph = converter.convertTo(original).convertedGraph

      When("converting back to scala")
      val data = ConverterData[Int, UnDiEdge, JGraphEd, Node, Edge](graph, Map(), Map())
      val scalaGraph = converter.convertFrom(data)

      Then("the resulting graph should have the three vertices 1, 2, 3")
      scalaGraph should equal (original)

    }

    it ("should convert edges back to scala") {

      Given("an original graph with nodes and edges")
      val original = Graph(1~2, 2~3, 3~1, 3~4, 4~5)
      val graph = converter.convertTo(original).convertedGraph

      When("converting back to scala")
      val data = ConverterData[Int, UnDiEdge, JGraphEd, Node, Edge](graph, Map(), Map())
      val scalaGraph = converter.convertFrom(data)

      Then("the resulting graph should have a topology that matches the original")
      scalaGraph should equal (original)

    }

  }

}
