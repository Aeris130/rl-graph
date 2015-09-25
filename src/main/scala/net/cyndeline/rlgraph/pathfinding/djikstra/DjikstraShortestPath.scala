package net.cyndeline.rlgraph.pathfinding.djikstra

import net.cyndeline.rlgraph.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter
import org.jgrapht.UndirectedGraph
import org.jgrapht.alg.DijkstraShortestPath
import org.jgrapht.graph.DefaultEdge

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Wraps JGraphTs path finding.
 *
 * @constructor Creates a new shortest path algorith wrapper.
 * @param graph Graph to perform the algorithm on.
 */
class DjikstraShortestPath[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]) {
  private val jgraphtConverter = new ScalaGraphToJGraphTConverter()
  private val jgraphtGraph: UndirectedGraph[VType, DefaultEdge] = jgraphtConverter.convert(graph)

  /**
   * Computes the shortest path between two vertices in a graph in terms of nodes to get there.
   * @param from A node in a graph to begin computing path from.
   * @param to Another node int the graph to travel to.
   * @return A list of all nodes between from and to. Includes start and stop node
   */
  def nodesOnShortestPath(from: VType, to: VType): Vector[VType] = {
    //TODO Move to the constructor so it doesn't get instantiated every call
    val djikstraPath = new DijkstraShortestPath(jgraphtGraph, from, to)
    val path = djikstraPath.getPath

    if (path == null)
      return Vector()

    val nodesOnPath = path.getEdgeList.iterator()
    val pathList = new ArrayBuffer[VType]()
    var currentNode = path.getStartVertex
    pathList += currentNode

    while (nodesOnPath.hasNext) {
      val pathEdge: DefaultEdge = nodesOnPath.next()
      currentNode = getOpposite(currentNode, pathEdge)
      pathList += currentNode
    }

    pathList.toVector
  }

  /**
   * Computes the distance between two nodes, as the minimum number of nodes to travel when going from one
   * node to the other.
   * @param from A node to travel from.
   * @param to A node to travel to.
   * @return
   */
  def distance(from: VType, to: VType): Int = {
    val path = nodesOnShortestPath(from, to)

    if (path.isEmpty)
      throw new Error("No path from " + from + " to " + to + " was found.")
    else
      path.size - 1 // -1 removes start node
  }

  private def getOpposite(node: VType, edge: DefaultEdge): VType = {
    val source = jgraphtGraph.getEdgeSource(edge)
    val target = jgraphtGraph.getEdgeTarget(edge)
    if (source == node)
      target
    else if (target == node)
      source
    else
      throw new Error("The node " + node + " did not belong to the edge " + edge + ".")

  }

}
