package net.cyndeline.rlgraph.planar.boyerMyrwold

import java.util

import boyer.BoyerPlanarEmbedder
import de.fhstralsund.vinets.algorithm.Parameter
import de.fhstralsund.vinets.structure.{Link, Node}
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.planar.PlanarEmbedOperation
import net.cyndeline.rlgraph.util.graphConverters.vinets.VinetsConverter

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Embeds a planar graph in O(n) time.
 *
 * This class wraps Arne-Michael Törsel's implementation of the Boyer-Myrvold embedder in a scala shell. As the original
 * vinets implementation rejects graphs without cycles, the implementation of gillean is used instead:
 * https://github.com/gilleain
 *
 * This class also makes the embedding deterministic, resulting in equal embeddings when the same input graph is used
 * multiple times.
  *
  * Note: Errors running on scala2.11.8 with java 8.
 */
//TODO fix java8 bug.
class BoyerMyrwoldEmbedder[VType : TypeTag : ClassTag] extends PlanarEmbedOperation[VType, UnDiEdge] {
  private val vinetsConverter = new VinetsConverter[VType, UnDiEdge]()

  /**
   * Embeds a planar graph.
   * @param graph Graph to embed.
   * @return A planar embedding, or None if the graph isn't planar.
   */
  def embed(graph: Graph[VType, UnDiEdge]): Option[Embedding[VType]] = {
    val graphConvertedToVinets = vinetsConverter.convertTo(graph)
    val embedder = new BoyerPlanarEmbedder()
    val parameter = new Parameter(graphConvertedToVinets.convertedGraph, null)
    val result: Parameter = embedder.execute(parameter)

    /* Ok, so this is the ugliest hack in the entire library. But for some reason, the author of the embedder forgot
     * to check what happens if a graph actually fails the planarity test by Eulers formula.
     *
     * Vertices remains set to null inside the embedder, causing errors when trying to retrieve adjacencies. And since
     * the embedder terminated without setting planar == false, it still claims the graph is planar. Luckily, the
     * returned parameter has a nice fail message...
     */
    if (result.getMessage == "Graph is NOT planar")
      None
    else {
      val cyclicalOrdering = embedder.getCyclicEdgeOrdering
      var adjacencyMap = Map[VType, Vector[VType]]()
      val newToOldNodes: Map[Node, VType] = graphConvertedToVinets.vertexMap.map(_.swap)
      val nodes: util.Iterator[_] = graphConvertedToVinets.convertedGraph.nodes()

      while (nodes.hasNext) {
        val node = nodes.next().asInstanceOf[Node]
        val originalVertex = newToOldNodes(node)
        val adjacentVertices: mutable.Buffer[Link] = cyclicalOrdering.get(node).asInstanceOf[util.ArrayList[Link]].asScala
        val adjacencyList = adjacentVertices.map(edge => newToOldNodes(findOppositeNode(node, edge))).toVector

        adjacencyMap += (originalVertex -> adjacencyList)
      }

      Some(UndirectedEmbedding[VType]().embed(adjacencyMap))
    }
  }

  /**
   * Checks if a graph is planar.
   * @param graph Graph to evaluate planarity for.
   * @return true if the graph is planar, otherwise false.
   */
  def isPlanar(graph: Graph[VType, UnDiEdge]): Boolean = {
    embed(graph).isDefined
  }

  private def findOppositeNode(n: Node, edge: Link): Node = {
    val nodes = edge.incidentNodes()
    val a = nodes.next().asInstanceOf[Node]
    val b = nodes.next().asInstanceOf[Node]

    if (a == n)
      b
    else if (b == n)
      a
    else
      throw new Error("No matching edge with node " + n + " found in " + edge)
  }

}

object BoyerMyrwoldEmbedder {
  def embed[VType : TypeTag : ClassTag](graph: Graph[VType, UnDiEdge]) = new BoyerMyrwoldEmbedder[VType]().embed(graph)
}
