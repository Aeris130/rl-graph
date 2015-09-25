package net.cyndeline.rlgraph.util.graphConverters.jgraphed

import java.util

import graphStructure.{Edge, Graph => JgraphEdGraph, Location, Node}
import net.cyndeline.rlgraph.util.graphConverters.{ConverterData, GraphConverter}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Converts graph structures between JGraphEd and scalax graphs. Edge adjacency order is not preserved.
 */
//TODO Stop storing the original data in node/edge extenders, and move it to the maps in the converter data.
class JGraphEdConverter[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l] 
  extends GraphConverter[VType, EType, JgraphEdGraph, Node, Edge] {

  /**
   * Converts from a scala graph to another graph structure.
   * @param graph A scala graph.
   * @return Graph structure type to convert to, as well as edges and vertices of the new graph mapped to the ones
   *         they're based off of in the input graph.
   */
  def convertTo(graph: Graph[VType, EType]): ConverterData[VType, EType, JgraphEdGraph, Node, Edge] = {
    var verticesToNodes = Map[VType, Node]()
    var nodesToVertices = Map[Node, VType]()
    var originalToNewEdges = Map[EType[VType], Edge]()
    val jgraphedGraph = new JgraphEdGraph()

    /* Since JGraphEds graph structure is made for drawing, each node must be assigned its own coordinate. */
    var c = 0

    /* Add every edge and its vertices. */
    for (edge <- graph.edges) {
      val outer = edge.toOuter
      val from = verticesToNodes.getOrElse(outer._1, {
        val n = jgraphedGraph.createNode(new Location(c, c))
        c += 1
        verticesToNodes += (outer._1 -> n)
        nodesToVertices += (n -> outer._1)
        n
      })
      val to = verticesToNodes.getOrElse(outer._2, {
        val n = jgraphedGraph.createNode(new Location(c, c))
        c += 1
        verticesToNodes += (outer._2 -> n)
        nodesToVertices += (n -> outer._2)
        n
      })
      jgraphedGraph.addEdge(from, to)
    }

    /* Add every vertex that doesn't have any edges connected to it. */
    for (v <- graph.nodes.filter(n => n.degree == 0)) {
      val n = jgraphedGraph.createNode(new Location(c, c))
      val vertex: VType = v
      c += 1
      verticesToNodes += (vertex -> n)
      nodesToVertices += (n -> vertex)
      n
    }

    /* Create extenders for every node and edge in JGraphEd. The extenders will store the original vertex/edges that
     * the graph was built from.
     */
    jgraphedGraph.createNodeExtenders(classOf[NodeWrapper[VType]])
    jgraphedGraph.createEdgeExtenders(classOf[EdgeWrapper[EType[VType]]])

    // Store the original vertex in every jgraphed node
    for (nodeAndVertex <- nodesToVertices) {
      nodeAndVertex._1.getExtender.asInstanceOf[NodeWrapper[VType]].setVertex(nodeAndVertex._2)
    }

    // Store the original edge in every jgraphed edge
    val jGraphEdEdges: util.Iterator[_] = jgraphedGraph.getEdges.iterator()
    while (jGraphEdEdges.hasNext) {
      val edge = jGraphEdEdges.next().asInstanceOf[Edge]
      val edgeExtender = edge.getExtender.asInstanceOf[EdgeWrapper[EType[VType]]]

      // Apparently JGraphEd returns the NodeExtender instead of the Node if one exists when getNode() is called
      val from = nodesToVertices(edgeExtender.getStartNode.asInstanceOf[NodeWrapper[VType]].getNode)
      val to = nodesToVertices(edgeExtender.getEndNode.asInstanceOf[NodeWrapper[VType]].getNode)

      val originalEdge = graph.get(from).edges.find(e => e.contains(from) && e.contains(to)).get.toOuter
      edgeExtender.setEdge(originalEdge)
      originalToNewEdges += (originalEdge -> edge)
    }

    ConverterData[VType, EType, JgraphEdGraph, Node, Edge](jgraphedGraph, verticesToNodes, originalToNewEdges)
  }

  /**
   * Converts another graph structure to scala graphs.
   * @param data Other graph type to convert to scala graph. Maps vertices in the input graph to the vertices used
   *             in scala-graph.
   * @return A scala graph structure based on the input graph.
   */
  def convertFrom(data: ConverterData[VType, EType, JgraphEdGraph, Node, Edge]): Graph[VType, EType] = {
    val graph = data.convertedGraph
    val vertices = new ArrayBuffer[VType]()
    val nodeWrappers = graph.getNodeExtenders.iterator()
    while (nodeWrappers.hasNext) {
      val wrapper = nodeWrappers.next().asInstanceOf[NodeWrapper[VType]]
      vertices += wrapper.vertex
    }

    val edges = new ArrayBuffer[EType[VType]]()
    val edgeWrappers = graph.getEdgeExtenders.iterator()
    while (edgeWrappers.hasNext) {
      val wrapper = edgeWrappers.next().asInstanceOf[EdgeWrapper[EType[VType]]]
      edges += wrapper.edge
    }

    Graph.from[VType, EType](vertices.toVector, edges.toVector)
  }
}
