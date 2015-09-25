package net.cyndeline.rlgraph.subgraph.stronglyConnectedComponents

import net.cyndeline.rlgraph.util.{GraphCommons, GraphConverter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Tarjan's algorithm to compute the strongly connected components of a graph: subgraphs where every vertex can be
 * reached from every other vertex, implying that for every vertex pair u, v there's a path from u to v and from v
 * to u.
 *
 * https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
 */
class TarjansAlgorithm {

  /**
   * @param graph A directed graph.
   * @tparam V Vertex type used in the graph.
   * @return A vector of vertex-collections, where every vertex sharing the same collection belongs to the same
   *         strongly connected component.
   */
  def sccComponents[V : TypeTag](graph: Graph[V, DiEdge]): Vector[Vector[V]] = {
    // The original algorithm uses mutable data structures, so let's stick to that
    val stackGraph = GraphConverter[V, StackNode[V], DiEdge, DiEdge]((v: V) => new StackNode[V](v), (e: DiEdge[V], v1: StackNode[V], v2: StackNode[V]) => v1~>v2).convert(graph)
    val data = new Data[V](stackGraph)
    for (v <- stackGraph.nodes if !v.indexDefined) {
      strongConnect(v, data)
    }

    data.result.toVector
  }

  private def strongConnect[V](v: StackNode[V], data: Data[V]): Unit = {
    v.index = data.index
    v.lowLink = data.index
    data.index += 1
    data.stack push v
    v.onStack = true

    for (neighbor <- GraphCommons.outgoingOuterNeighbors(v, data.graph)) {
      if (!neighbor.indexDefined) {
        strongConnect(neighbor, data)
        v.lowLink = Math.min(v.lowLink, neighbor.lowLink)
      } else if (neighbor.onStack) {
        v.lowLink = Math.min(v.lowLink, neighbor.index)
      }
    }

    if (v.lowLink == v.index) {
      var w = data.stack.pop()
      w.onStack = false
      val sccComponent = new ListBuffer[V]()
      while (v != w) {
        sccComponent += w.vertex
        w = data.stack.pop()
        w.onStack = false
      }

      sccComponent += w.vertex
      data.result += sccComponent.toVector
    }

  }

  private class Data[V](val graph: Graph[StackNode[V], DiEdge]) {
    val stack = new mutable.Stack[StackNode[V]]()
    val result = new ListBuffer[Vector[V]]()
    var index = 0
  }

}
