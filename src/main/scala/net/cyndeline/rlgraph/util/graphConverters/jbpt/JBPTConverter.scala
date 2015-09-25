package net.cyndeline.rlgraph.util.graphConverters.jbpt

import java.util

import net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure.{JBPTHashGraph, VertexHash}
import net.cyndeline.rlgraph.util.graphConverters.{ConverterData, GraphConverter}
import net.cyndeline.rlgraph.util.{EdgeFactory, VertexFactory}
import org.jbpt.graph.Edge
import org.jbpt.hypergraph.abs.Vertex

import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Converts scala-graphs to/from the graph structure used by the JBPT library.
 * @param vertexFactory Factory that creates new vertices from unmapped JBPT vertices, or None if unmapped vertices
 *                      shouldn't be allowed.
 * @param edgeFactory Factory that creates new edges from unmapped JBPT edges, or None if unmapped edges
 *                    shouldn't be allowed.
 * @tparam VType Vertex type used in the scala graph.
 * @tparam EType Edge type used in the scala graph.
 */
class JBPTConverter[VType, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
  (vertexFactory: Option[VertexFactory[VType, Vertex]], edgeFactory: Option[EdgeFactory[VType, EType]])
  extends GraphConverter[VType, EType, JBPTHashGraph, Vertex, Edge] {

  /**
   * @return A new JBPT converter that doesn't allow unmapped vertices or edges.
   */
  def this() = this(None, None)

  /**
   * @param vertexFactory Factory that creates new vertices when none are found in the vertex mapping.
   * @param edgeFactory Factory that creates new edges when none are found in the edge mapping.
   * @return A new JBPT converter that allows unmapped vertices and edges.
   */
  def this(vertexFactory: VertexFactory[VType, Vertex], edgeFactory: EdgeFactory[VType, EType]) = this(Some(vertexFactory), Some(edgeFactory))

  /**
   * Converts from a scala graph to another graph structure.
   * @param graph A scala graph.
   * @return Graph structure type to convert to, as well as edges and vertices of the new graph mapped to the ones
   *         they're based off of in the input graph.
   */
  def convertTo(graph: Graph[VType, EType]): ConverterData[VType, EType, JBPTHashGraph, Vertex, Edge] = {
    var oldToNewVertex = Map[VType, Vertex]()
    var oldToNewEdge = Map[EType[VType], Edge]()
    val jbptGraph = new JBPTHashGraph()
    var hashId = 0

    for (n <- graph.nodes) {
      val outer: VType = n
      val newVertex = new VertexHash(hashId)
      hashId += 1
      oldToNewVertex += (outer -> newVertex)
      jbptGraph.addVertex(newVertex)
    }

    for (e <- graph.edges) {
      val from: VType = e._1
      val to: VType = e._2
      val newEdge = jbptGraph.addEdge(oldToNewVertex(from), oldToNewVertex(to))
      oldToNewEdge += (e.toOuter -> newEdge)
    }

    ConverterData[VType, EType, JBPTHashGraph, Vertex, Edge](jbptGraph, oldToNewVertex, oldToNewEdge)
  }

  /**
   * Converts another graph structure to scala graphs.
   * @param data Other graph type to convert to scala graph. Maps vertices in the input graph to the vertices used
   *             in scala-graph.
   * @return A scala graph structure based on the input graph.
   */
  def convertFrom(data: ConverterData[VType, EType, JBPTHashGraph, Vertex, Edge]): Graph[VType, EType] = {
    val edgesCollection: util.Collection[Edge] = data.convertedGraph.getEdges // Ambiguous reference when chaining iterator()
    val edges = edgesCollection.iterator()
    val vertices = data.convertedGraph.getVertices.iterator()

    var newToOldVertices = data.vertexMap.map(_.swap)
    val newToOldEdges = data.edgeMap.map(_.swap)

    var vertexList = Vector[VType]()
    while (vertices.hasNext) {
      val v = vertices.next()
      val vertexToInsert = newToOldVertices.get(v).getOrElse( vertexFactory.getOrElse {
        throw new Error("The submitted JBPT graph contains a vertex that isn't specified in the vertex map, but no vertex factory is supplied")
      }.produceVertex(v))
      newToOldVertices += (v -> vertexToInsert)
      vertexList = vertexToInsert +: vertexList
    }

    var edgeList = Vector[EType[VType]]()
    while (edges.hasNext) {
      val e: Edge = edges.next()
      val from = newToOldVertices(e.getV1)
      val to = newToOldVertices(e.getV2)
      val edgeToInsert: EType[VType] = newToOldEdges.get(e).getOrElse(edgeFactory.getOrElse{
        throw new Error("The submitted JBPT graph contains an edge that isn't specified in the edge map, but no edge factory is supplied")
      }.produce(from, to).asInstanceOf[EType[VType]])
      edgeList = edgeToInsert +: edgeList
    }

    Graph.from(vertexList, edgeList)
  }
}
