package net.cyndeline.rlgraph.planar.jgraphed

import graphStructure.{Edge, Node}
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.planar.PlanarEmbedOperation
import net.cyndeline.rlgraph.util.graphConverters.jgraphed.{EdgeWrapper, JGraphEdConverter, NodeWrapper}
import operation.{EmbedOperation, PlanarityOperation}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Wraps the JGraphEd library's planar embedding algorithm.
 *
 * NOTE: Do not use. JGraphEd cannot embed all planar graphs. Graph that triggers a bug:
 * Graph(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1~5, 1~7, 1~8, 1~9, 2~6, 2~7, 2~9, 2~10, 3~4, 3~7, 3~8, 3~10, 4~2, 5~3, 6~1, 7~8, 7~9, 7~10)
 */
class JGraphEdEmbedder[VType : TypeTag : ClassTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l] extends PlanarEmbedOperation[VType, EType] {
  private val converter = new JGraphEdConverter[VType, EType]()

  /**
   * Embeds a planar graph.
   * @param graph Graph to embed.
   * @return A planar embedding, or None if the graph isn't planar.
   */
  def embed(graph: Graph[VType, EType]): Option[Embedding[VType]] = {
    val jge = converter.convertTo(graph).convertedGraph

    /* Performing the embedding operation or planarity check causes the nodewrappers to be replaced with other ones
     * needed for the algorithm. Store Node <-> vertex mapping externally.
     */
    var nodeToVertexMap = Map[Node, VType]()
    for (node <- jge.getNodes.asScala) {
      val n = node.asInstanceOf[Node]
      val extender = n.getExtender.asInstanceOf[NodeWrapper[VType]]
      nodeToVertexMap += (n -> extender.vertex)
    }

    // Edges are deleted and replaced during planarization, but nodes are not. Original edges are mapped against
    // the node pair of an edge rather than the edge itself.
    var edgeToOriginalEdgeMap = Map[(Node, Node), EType[VType]]()
    for (edge <- jge.getEdges.asScala) {
      val e = edge.asInstanceOf[Edge]
      val extender = e.getExtender.asInstanceOf[EdgeWrapper[EType[VType]]]
      val start = e.getStartNode.asInstanceOf[Node]
      val end = e.getEndNode.asInstanceOf[Node]
      edgeToOriginalEdgeMap += ((start, end) -> extender.edge)
    }

    if (!PlanarityOperation.isPlanar(jge))
      return None

    // Edge adjacencies are modified inside the graph
    EmbedOperation.embed(jge, false) // Don't check if the graph isn't planar inside

    // Go over every edge list for each node and add it to the embedding
    var adjacencyMap = Map[VType, Vector[VType]]()
    val nodes = jge.getNodes.iterator
    while (nodes.hasNext) {
      val node = nodes.next().asInstanceOf[Node]
      val originalVertex = nodeToVertexMap(node)
      val adjacentVertices = node.incidentEdges().asScala.map((e: Any) => {
        val edge = e.asInstanceOf[Edge]
        val start = edge.getStartNode.asInstanceOf[Node]
        val end = edge.getEndNode.asInstanceOf[Node]
        val originalEdge = edgeToOriginalEdgeMap((start, end))
        val vertex = originalVertex
        val adjacentNode = if (originalEdge._1 == vertex) originalEdge._2 else originalEdge._1
        adjacentNode
      }).toVector

      adjacencyMap += (originalVertex -> adjacentVertices)
    }

    Some(UndirectedEmbedding[VType]().embed(adjacencyMap))
  }

  /**
   * Checks if a graph is planar.
   * @param graph Graph to evaluate planarity for.
   * @return true if the graph is planar, otherwise false.
   */
  def isPlanar(graph: Graph[VType, EType]): Boolean = {
    val jge = converter.convertTo(graph).convertedGraph
    PlanarityOperation.isPlanar(jge)
  }
}
