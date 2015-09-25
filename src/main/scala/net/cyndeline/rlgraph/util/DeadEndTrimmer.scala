package net.cyndeline.rlgraph.util

import scala.collection.mutable
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Removes all nodes n with degree < 2, and all nodes n' with degree 1 that result from removing n.
 */
object DeadEndTrimmer {

  /**
   * @param graph A graph.
   * @return The biconnected graph that results from removing all nodes of degree 1, or an empty graph if no cycles
   *         existed in the original graph.
   */
  def trim[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]): Graph[VType, EType] = {
    var currentGraph = graph
    val nextToCheck = new mutable.Queue[VType]()

    for (n <- graph.nodes.iterator) {
      if (n.degree == 1)
        nextToCheck += n
      else if (n.degree == 0)
        currentGraph -= n
    }

    while (!nextToCheck.isEmpty) {
      val nextToRemove = nextToCheck.dequeue()
      if (currentGraph.contains(nextToRemove) && hasDegree1[VType, EType](nextToRemove, currentGraph)) {
        var current = nextToRemove

        while (hasDegree1(current, currentGraph)) {
          val remove = current
          current = oppositeNode[VType, EType](current, currentGraph.get(current).edges.head.toOuter)
          currentGraph -= remove
        }

        if (currentGraph.get(current).degree > 0) {
          nextToCheck.enqueue(current)
        } else {
          currentGraph -= current
        }

      }

    }

    currentGraph
  }

  private def hasDegree1[VType, EType[X] <: UnDiEdge[X]](n: VType, g: Graph[VType, EType]): Boolean = {
    g.get(n).degree == 1
  }

  private def oppositeNode[VType, EType[X] <: UnDiEdge[X]](v: VType, edge: EType[VType]): VType = if (edge._1 == v) edge._2 else edge._1
}
