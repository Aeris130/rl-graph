package net.cyndeline.rlgraph.pathfinding

import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.immutable.Queue
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Computes a topological sorting order for vertices in a directed, acyclic graph.
 *
 * Algorithm:
 *
 *  1. Add every vertex with no incoming edges to the current vertex queue S
 *   1. If no such vertices exists, the graph has a cycle. Exit with error.
 *  1. For every vertex v in S, remove it from the graph and examine its neighbors n to see if removing v results
 *  in n not having any incoming edges. If so, add n to S.
 *  1. Repeat step 2 until the graph is empty.
 *    1. If at some point no vertex is added to S, but the graph still has vertices in it, the graph is cyclical.
 *    Exit with error.
 *
 * Example: Graph( 7->11, 7->8, 5->11, 3->8, 11->2, 11->9, 11->10, 8->9, 3->10 ) results in the order
 * [7, 5, 3] [8, 11] [2, 9, 10], where [...] indicates that the elements could come in any order.
 */
class TopologicalDAGSort[VType] {

  /**
   * @param graph An undirected acyclic graph.
   * @return Every vertex of the input graph, sorted by its topological order.
   */
  def order(graph: Graph[VType, DiEdge]): Vector[VType] = {
    if (graph.isEmpty) {
      Vector()
    } else {
      val initialNodes = graph.nodes
        .filter(_.inDegree == 0)
        .toVector
        .map(n => {val inner: VType = n; inner})

      if (initialNodes.isEmpty)
        throw new IllegalArgumentException("The supplied graph " + graph + " was not acyclic.")

      orderNext(graph, Queue[VType]() ++ initialNodes, graph)
    }
  }

  private def orderNext(graph: Graph[VType, DiEdge], S: Queue[VType], originalGraph: Graph[VType, DiEdge]): Vector[VType] = {
    if (!S.isEmpty) {
      val next: (VType, Queue[VType]) = S.dequeue
      val neighbors = GraphCommons.neighbors(next._1, graph).map(n => {val inner: VType = n; inner})
      val currentGraph = graph - next._1
      val candidates = for {
        n <- neighbors
        if currentGraph.get(n).inDegree == 0
      } yield n

      Vector(next._1) ++ orderNext(currentGraph, next._2 ++ candidates, originalGraph)
    } else if (graph.isEmpty) {
      Vector()
    } else {
      throw new IllegalArgumentException("The supplied graph " + originalGraph + " was not acyclic.")
    }
  }
}
