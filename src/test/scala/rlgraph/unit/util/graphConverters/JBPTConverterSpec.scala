package rlgraph.unit.util.graphConverters

import java.util

import net.cyndeline.rlgraph.util.VertexFactory
import net.cyndeline.rlgraph.util.graphConverters.ConverterData
import net.cyndeline.rlgraph.util.graphConverters.jbpt.JBPTConverter
import net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure.{JBPTHashGraph, VertexHash}
import org.jbpt.graph.Edge
import org.jbpt.hypergraph.abs.Vertex
import rlgraph.SpecImports
import rlgraph.help.UndirectedEdgeFactory

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class JBPTConverterSpec extends SpecImports {
  val vertexFactory = new VertexFactory[Int, Vertex]() {
    def produceVertex(input: Vertex): Int = input.##
  }
  val edgeFactory = new UndirectedEdgeFactory[Int]()
  val converter = new JBPTConverter(vertexFactory, edgeFactory)

  describe("JBPTConverter") {

    /*
     *
     *  Scala -> JBPT
     *
     */
    it ("should map every old vertex to the ones used in the new graph") {

      Given("a scala graph with vertices and edges")
      val scalaGraph = Graph[Int, UnDiEdge](1~2, 2~3, 4)

      When("converting the graph to JBPT")
      val JBPTConversion = converter.convertTo(scalaGraph)

      Then("the vertex map should contain entries for vertices 1, 2, 3 and 4")
      val vertexMap = JBPTConversion.vertexMap
      vertexMap.size should be (4)
      vertexMap.keySet should contain (1)
      vertexMap.keySet should contain (2)
      vertexMap.keySet should contain (3)
      vertexMap.keySet should contain (4)

      And("the new vertices mapped to the old should be unique instances")
      assert(!vertexMap.filter(kv => kv._1 != 1).exists(kv => kv._2 eq vertexMap(1)))
      assert(!vertexMap.filter(kv => kv._1 != 2).exists(kv => kv._2 eq vertexMap(2)))
      assert(!vertexMap.filter(kv => kv._1 != 3).exists(kv => kv._2 eq vertexMap(3)))
      assert(!vertexMap.filter(kv => kv._1 != 4).exists(kv => kv._2 eq vertexMap(4)))

    }

    it ("should map every old edge to the ones used in the new graph") {

      Given("a scala graph with vertices and edges")
      val e1 = 1~2
      val e2 = 2~3
      val scalaGraph = Graph[Int, UnDiEdge](e1, e2, 4)

      When("converting the graph to JBPT")
      val JBPTConversion = converter.convertTo(scalaGraph)

      Then("the edge map should contain entries for edges 1~2 and 2~3")
      val edgeMap = JBPTConversion.edgeMap
      edgeMap.size should be (2)
      edgeMap.keySet should contain(e1)
      edgeMap.keySet should contain(e2)

      And("the new edges mapped to the old should be unique instances")
      assert(!(edgeMap(e1) eq edgeMap(e2)))

    }

    it ("should add the new vertices to a graph structure") {

      Given("a scala graph with vertices and edges")
      val scalaGraph = Graph[Int, UnDiEdge](1~2, 2~3, 4)

      When("converting the graph to JBPT")
      val JBPTConversion = converter.convertTo(scalaGraph)

      Then("the converted graph should contain all vertices mapped to the old ones")
      val newVerticesInMap = JBPTConversion.vertexMap.values
      val newVertices: Set[AnyRef] = JBPTConversion.convertedGraph.getVertices.toArray.toSet // Some java -> scala
      newVertices should have size 4
      for (newVertex <- newVerticesInMap) {
        newVertices should contain (newVertex.asInstanceOf[AnyRef])
      }

    }

    it ("should add the new edges to a graph structure") {

      Given("a scala graph with vertices and edges")
      val scalaGraph = Graph[Int, UnDiEdge](1~2, 2~3, 4)

      When("converting the graph to JBPT")
      val JBPTConversion = converter.convertTo(scalaGraph)

      Then("the graph structure should contain edges between the new vertices mapped to 1, 2 and 3")
      val edges: util.Collection[Edge] = JBPTConversion.convertedGraph.getEdges // Ambiguous reference when calling size directly
      edges.size should be (2)

      val v1 = JBPTConversion.vertexMap(1)
      val v2 = JBPTConversion.vertexMap(2)
      val v3 = JBPTConversion.vertexMap(3)
      val e1 = JBPTConversion.convertedGraph.getEdge(v1, v2)
      val e2 = JBPTConversion.convertedGraph.getEdge(v2, v3)

      e1 should not be null
      e2 should not be null

    }

    /*
     *
     *  JBPT -> Scala
     *
     */

    it ("should throw an exception if converting unmapped vertices without a vertex factory") {

      Given("a JBPT graph with an unmapped vertex")
      val graph = new JBPTHashGraph()
      graph.addVertex(new VertexHash(1))
      val conversionData = ConverterData[Int, UnDiEdge, JBPTHashGraph, Vertex, Edge](graph, Map(), Map())

      When("converting the graph to scala-graph using a converter with no vertex factory")
      val converter = new JBPTConverter[Int, UnDiEdge]()

      Then("an error should be thrown")
      intercept[Error] {
        converter.convertFrom(conversionData)
      }

    }

    it ("should throw an exception if converting unmapped edges without an edge factory") {

      Given("a JBPT graph with an unmapped edge")
      val graph = new JBPTHashGraph()
      val v1 = new VertexHash(1)
      val v2 = new VertexHash(2)
      graph.addEdge(v1, v2)
      val vMap = Map(1 -> v1, 2 -> v2)
      val conversionData = ConverterData[Int, UnDiEdge, JBPTHashGraph, Vertex, Edge](graph, vMap, Map())

      When("converting the graph to scala-graph using a converter with no edge factory")
      val converter = new JBPTConverter[Int, UnDiEdge]()

      Then("an error should be thrown")
      intercept[Error] {
        converter.convertFrom(conversionData)
      }

    }

    it ("should map old vertices and edges back to scala") {

      Given("a JBPT graph with 4 vertices, edges between v1, v2 and v2, v3 and a mapping of vertices v(i) to scala vertex i (i <- 1 to 4)")
      val graph = new JBPTHashGraph()
      val v1 = new VertexHash(1)
      val v2 = new VertexHash(2)
      val v3 = new VertexHash(3)
      val v4 = new VertexHash(4)
      graph.addVertex(v1)
      graph.addVertex(v2)
      graph.addVertex(v3)
      graph.addVertex(v4)
      val e1 = graph.addEdge(v1, v2)
      val e2 = graph.addEdge(v2, v3)
      val vMap = Map(1 -> v1, 2 -> v2, 3 -> v3, 4 -> v4)
      val eMap = Map(1~2 -> e1, 2~3 -> e2)
      val conversionData = ConverterData[Int, UnDiEdge, JBPTHashGraph, Vertex, Edge](graph, vMap, eMap)

      When("converting the graph to scala-graph")
      val scalaGraph = converter.convertFrom(conversionData)

      Then("the resulting graph should match the topology of the input")
      scalaGraph should equal (Graph(1~2, 2~3, 4))

    }

    it ("should use vertex factories to produce unmapped vertices") {

      Given("a JBPT graph that contains 2 vertices, but only maps a scala-graph vertex to one of them")
      val graph = new JBPTHashGraph()
      val v1 = new VertexHash(1)
      val v2 = new VertexHash(2)
      graph.addVertex(v1)
      graph.addVertex(v2)
      val vMap = Map(1 -> v1)
      val conversionData = ConverterData[Int, UnDiEdge, JBPTHashGraph, Vertex, Edge](graph, vMap, Map())

      When("converting the graph to scala-graph")
      val scalaGraph = converter.convertFrom(conversionData)

      Then("the resulting graph should contain a new vertex not specified in the map")
      // The factory produces integer vertices based on hash, see top of this spec
      scalaGraph should equal (Graph[Int, UnDiEdge](1, v2.##))

    }

    it ("should use factories to produce unmapped edges") {

      Given("a JBPT graph with an edge between two vertices, but without a mapping to a scala-graph edge")
      val graph = new JBPTHashGraph()
      val v1 = new VertexHash(1)
      val v2 = new VertexHash(2)
      graph.addVertex(v1)
      graph.addVertex(v2)
      graph.addEdge(v1, v2)
      val vMap = Map(1 -> v1, 2 -> v2)
      val eMap = Map[UnDiEdge[Int], Edge]() // No mapping
      val conversionData = ConverterData[Int, UnDiEdge, JBPTHashGraph, Vertex, Edge](graph, vMap, eMap)

      When("converting the graph to scala-graph")
      val scalaGraph = converter.convertFrom(conversionData)

      Then("the resulting graph should have an edge between vertices 1 and 2")
      scalaGraph should equal (Graph(1~2))

    }

  }
}
