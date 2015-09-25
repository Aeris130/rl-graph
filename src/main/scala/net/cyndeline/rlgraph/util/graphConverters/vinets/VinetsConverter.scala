package net.cyndeline.rlgraph.util.graphConverters.vinets

import de.fhstralsund.vinets.structure.{Link, Node}
import net.cyndeline.rlgraph.util.graphConverters.{ConverterData, GraphConverter}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Converts scala graphs to/from a graph structure that extends the interfaces needed by the Vinets library.
 * @tparam VType Vertex type used in the scala graph.
 * @tparam EType Edge type used in the scala graph.
 */
class VinetsConverter[VType : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
  extends GraphConverter[VType, EType, HashGraph, Node, Link] {

  /**
   * Converts from a scala graph to another graph structure.
   * @param graph A scala graph.
   * @return Graph structure type to convert to, as well as edges and vertices of the new graph mapped to the ones
   *         they're based off of in the input graph.
   */
  def convertTo(graph: Graph[VType, EType]): ConverterData[VType, EType, HashGraph, Node, Link] = {
    val vinetsGraph = new HashGraph()
    val nodeMap = (for (n <- graph.nodes) yield {
      val outer: VType = n
      outer -> vinetsGraph.createNode(n.toString())
    }).toMap

    val edgeMap = (for (e <- graph.edges) yield {
      val newEdge = vinetsGraph.createEdge(nodeMap(e._1), nodeMap(e._2), false) // Not directed
      e.toOuter -> newEdge
    }).toMap

    ConverterData[VType, EType, HashGraph, Node, Link](vinetsGraph, nodeMap, edgeMap)
  }

  /**
   * Converts another graph structure to scala graphs.
   * @param data Other graph type to convert to scala graph. Maps vertices in the input graph to the vertices used
   *             in scala-graph.
   * @return A scala graph structure based on the input graph.
   */
  def convertFrom(data: ConverterData[VType, EType, HashGraph, Node, Link]): Graph[VType, EType] = {
    val nodes = data.convertedGraph.nodes()
    val edges = data.convertedGraph.edges()
    val newNodes = new ArrayBuffer[VType]()
    val newEdges = new ArrayBuffer[EType[VType]]()
    val newToOldNodes = data.vertexMap.map(_.swap)
    val newToOldEdges = data.edgeMap.map(_.swap)

    while (nodes.hasNext) {
      val n = nodes.next().asInstanceOf[Node]
      newNodes += newToOldNodes(n)
    }

    while (edges.hasNext) {
      val e = edges.next().asInstanceOf[Link]
      newEdges += newToOldEdges(e)
    }

    Graph.from(newNodes.toVector, newEdges.toVector)
  }

}
