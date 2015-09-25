package net.cyndeline.rlgraph.sorting

import net.cyndeline.rlgraph.util.{GraphCommons, IndexGraph, IndexVertex}

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/** Factory object for ordering vertices in DFS order. */
object DFSOrder {

  /**
   * Sorts an entire connected graph in DFS order.
   * @param graph Graph whose vertices should be ordered. Must be connected.
   * @return Every vertex of the input graph sorted in DFS order.
   */
  def apply[VType : TypeTag](graph: Graph[VType, UnDiEdge]): Vector[VType] = new DFSOrder().visit(graph)

  /**
   * Sorts the subgraph reachable from a start vertex into DFS order.
   * @param graph Graph whose vertices should be ordered.
   * @param start Start vertex to begin the order from.
   * @return Every vertex of the input graph sorted in DFS order, if it was reachable from the start vertex.
   */
  def apply[VType : TypeTag](graph: Graph[VType, UnDiEdge], start: VType): Vector[VType] = new DFSOrder().visitFrom(graph, start)
}

/**
 * Sorts vertices in the order they're visited when traversing a graph depth-first. Recursive implementation.
 */
class DFSOrder {

  /**
   * Starts the DGS order in an arbitrary vertex and parses an entire graph.
   * @param graph Graph whose vertices should be ordered. Must be connected.
   * @return Every vertex of the input graph sorted in DFS order.
   */
  def visit[VType : TypeTag](graph: Graph[VType, UnDiEdge]): Vector[VType] = {
    require(graph.isConnected, "The input graph was not connected. A start vertex must be specified when this is the case.")
    verifyStart[VType](graph, graph.nodes.head)
    visitFrom[VType](graph, graph.nodes.head)
  }

  /**
   * @param graph Graph whose vertices should be ordered.
   * @param start Start vertex to begin the order from.
   * @return Every vertex of the input graph sorted in DFS order, if it was reachable from the start vertex.
   */
  def visitFrom[VType : TypeTag](graph: Graph[VType, UnDiEdge], start: VType): Vector[VType] = {
    verifyStart(graph, start)
    val indexGraph: Graph[IndexVertex[VType], UnDiEdge] = IndexGraph.undirected(graph)
    val visited = Array.fill(indexGraph.nodes.size)(false)
    val result = new ListBuffer[IndexVertex[VType]]()
    val indexStart = indexGraph.nodes.find(_.valueOfIndex == start).get
    dfs(indexStart, visited, result, indexGraph)
    result.toVector.map(_.valueOfIndex)
  }

  private def dfs[VType](v: IndexVertex[VType],
                         visited: Array[Boolean],
                         result: ListBuffer[IndexVertex[VType]],
                         g: Graph[IndexVertex[VType], UnDiEdge]): Unit = {
    visited(v.index) = true
    result += v
    val neighbors = GraphCommons.outerNeighbors(v, g).iterator
    while (neighbors.hasNext) {
      val n = neighbors.next()
      if (!visited(n.index))
        dfs(n, visited, result, g)
    }
  }

  private def verifyStart[VType](graph: Graph[VType, UnDiEdge], start: VType) {
    require(graph.contains(start), "Could not find the start vertex " + start + " when performing DFS search in the graph " + graph + ".")
  }
}
