package net.cyndeline.rlgraph.biconnectivity.components

import net.cyndeline.rlgraph.biconnectivity.BiconnectedComponentsOperation

import scala.collection.mutable
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes all biconnected components from a graph.
 *
 * @constructor Creates a new dfs component search object.
 */
class DFSComponentSearch[VType, EType[X] <: UnDiEdge[X]] extends BiconnectedComponentsOperation[VType, EType] {

  /**
   * Computes a list of biconnected components from a connected graph by using a DFS search.
   * @param graph An undirected graph.
   * @return A list of every biconnected component in the graph. If the graph is empty or only
   *         has one vertex, an empty list is returned.
   */
  def components(graph: Graph[VType, EType]): Vector[Graph[VType, EType]] = {
    val vertexInfo = new mutable.HashMap[VType, VertexInfo]()
    componentSearch(graph, vertexInfo, false).map(v => v._1)
  }

  /**
   * Computes all articulation/cut points for a graph using a DFS search.
   * @param graph An undirected graph.
   * @return A list of every cutpoint in the graph.
   */
  def articulationPoints(graph: Graph[VType, EType]): Vector[VType] = {
    val vertexInfo = new mutable.HashMap[VType, VertexInfo]()
    componentSearch(graph, vertexInfo, false) // No need to store the result, it's the map that's interesting
    
    (for {
      info <- vertexInfo.values
      if info.isCutpoint
    } yield info.vertex).toVector
  }

  /**
   * Computes all biconnected components and identifies which cutpoints belong to which component.
   * @param graph An undirected graph.
   * @return A list of tuples where the first element is a biconnected component, and the second is the set of
   *         cutpoints belonging present on that component.
   */
  def componentsAndArticulationPoints(graph: Graph[VType, EType]): Vector[(Graph[VType, EType], Set[VType])] = {
    val vertexInfo = new mutable.HashMap[VType, VertexInfo]()
    componentSearch(graph, vertexInfo, false)
  }

  /**
   * Checks if a graph is biconnected or not.
   * @param graph Graph to check biconnectivity for.
   * @return True if the graph is biconnected, otherwise false if the graph has more than one biconnected component.
   */
  def isBiconnected(graph: Graph[VType, EType]): Boolean = {
    val vertexInfo = new mutable.HashMap[VType, VertexInfo]()
    componentSearch(graph, vertexInfo, true).map(v => v._1).size < 2
  }

  /**
   * Finds every biconnected component in the graph while also computing the cutpoints.
   * @param vertexInfo A mutable map, allowing the cutpoint method call to use the same code as the component search.
   * @param biconnectivityCheck Set this to true when only using this to check if a graph is biconnected. If it is
   *                            not, the method will now return as soon as 2 biconnected components are found,
   *                            skipping the rest.
   * @return A list containing all biconnected components.
   */
  private def componentSearch(graph: Graph[VType, EType],
                              vertexInfo: mutable.HashMap[VType, VertexInfo],
                              biconnectivityCheck: Boolean): Vector[(Graph[VType, EType], Set[VType])] = {
    require(!graph.edges.exists(_.isDirected), "Cannot compute biconnected components of a directed graph: " + graph.edges.find(_.isDirected).get)
    var result: Vector[Graph[VType, EType]] = Vector[Graph[VType, EType]]()
    val count = 0

    /* Add info to every vertex. */
    for (vertex <- graph.nodes) {
      val outer: VType = vertex
      vertexInfo += (outer -> new VertexInfo(outer))
    }

    for (vertex <- graph.nodes) {
      val info: VertexInfo = vertexInfo(vertex)

      if (!info.visited) {
        result = result ++ dfsVisit(vertex, graph, vertexInfo, count, biconnectivityCheck)
      }

      if (biconnectivityCheck && result.size > 1)
        return mapGraphsToCutpoints(result, vertexInfo)
    }

    mapGraphsToCutpoints(result, vertexInfo)
  }

  private def mapGraphsToCutpoints(graphs: Vector[Graph[VType, EType]],
                                   vertexInfo: mutable.HashMap[VType, VertexInfo]): Vector[(Graph[VType, EType], Set[VType])] = {
    graphs.map(component =>
      (component, component.nodes
        .filter(n => { vertexInfo(n).isCutpoint })
        .map(inner => { val outer: VType = inner; outer })
        .toSet
        ))
  }

  private def dfsVisit(vertex: VType,
                       graph: Graph[VType, EType],
                       vertexInfo: mutable.HashMap[VType, VertexInfo],
                       currentCount: Int,
                       biconnectivityCheck: Boolean,
                       stack: mutable.Stack[Graph[VType, EType]#EdgeT] = new mutable.Stack()): Set[Graph[VType, EType]] = {
    val info: VertexInfo = vertexInfo(vertex)
    val count = currentCount + 1
    var currentResult: Set[Graph[VType, EType]] = Set()
    info.visited = true
    info.num = count
    info.low = info.num

    val neighbors: Iterator[Graph[VType, EType]#EdgeT] = graph.get(vertex).incoming.iterator
    var children = 0
    while(neighbors.hasNext) {
      val neighbor: Graph[VType, EType]#EdgeT = neighbors.next()
      val nEdge: EType[VType] = neighbor.toOuter

      /* In these comparisons, == must be called on vertex with the edge value as parameter, or it will yield false. */
      val neighborVertex: VType =
        if (vertex == nEdge._1) graph.get(nEdge._2)
        else if (vertex == nEdge._2) graph.get(nEdge._1)
        else throw new Error("The edge belonged to neither vertex")
      val neighborInfo: VertexInfo = vertexInfo(neighborVertex)

      if (!neighborInfo.visited) {
        children += 1
        stack.push(neighbor)
        neighborInfo.parent = Option(vertex)

        currentResult = currentResult ++ dfsVisit(neighborVertex, graph, vertexInfo, count, biconnectivityCheck, stack)

        if (neighborInfo.low >= info.num) {
          currentResult = currentResult + constructComponent(nEdge, stack, graph)
        }

        info.low = Math.min(info.low, neighborInfo.low)

        /* If there's a child that cannot visit vertices before the current vertex, the current
         * vertex is a cutpoint.
         */
        if (info.parent.isDefined && neighborInfo.low >= info.num) {
          info.isCutpoint = true
        }

      } else if (info.parent.isDefined && info.parent.get != neighborVertex && neighborInfo.num < info.num) {
        stack.push(neighbor)
        info.low = Math.min(info.low, neighborInfo.num)
      }

    }

    /* Due to the depth first search, if the current vertex is the root, and visiting one child does not set the
     * others as visited (thus counting them as children as well), it means that the children of the root cannot
     * reach each other, making the root a cutpoint.
     */
    if (info.parent.isEmpty && children > 1) {
      info.isCutpoint = true
    }

    currentResult
  }

  private def constructComponent(stop: EType[VType],
                                 stack: mutable.Stack[Graph[VType, EType]#EdgeT],
                                 constructTemplate: Graph[VType, EType]): Graph[VType, EType] = {
    var graph = constructTemplate.empty
    var top: Option[Graph[VType, EType]#EdgeT] = None

    do {
      top = Some(stack.pop())
      graph = graph + top.get
    } while (top.get != stop)

    graph
  }

  private class VertexInfo(val vertex: VType) {
    var visited = false
    var num = 0
    var low = 0
    var parent: Option[VType] = None
    var isCutpoint = false

    override def equals(other: Any): Boolean = other match {
      case v: VertexInfo => v.vertex == vertex && v.visited == visited && v.num == num && v.low == low && v.parent == parent
      case _ => false
    }

    override def hashCode: Int = vertex.##
  }
}
