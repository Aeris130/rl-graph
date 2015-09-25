package net.cyndeline.rlgraph.planar.demoucron.operation.help

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Set, mutable}
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph

/**
 * Computes every fragment from a biconnected graph.
 *
 * @constructor Creates a new fragment computation.
 */
class FragmentComputation[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * Computes fragments from a subset of a partially embedded graph.
   *
   * @param graph The graph in its entirety.
   * @param embedded The part of the graph that has been embedded so far. Vertices in this graph will be considered
   *                 contact vertices for any potential fragments, and edge elements will be compared to elements in
   *                 'graph.
   * @return a list of all fragments found, containing all edges in 'graph but not in 'embedded.
   */
  def compute(graph: Graph[VType, EType], embedded: Graph[VType, EType]): Vector[Fragment[VType, EType]] = {
    val result = new ArrayBuffer[Fragment[VType, EType]]()
    var edgesLeft = difference(graph, embedded)

    while (!edgesLeft.edges.isEmpty) {
      val startEdge: EType[VType] = edgesLeft.edges.toOuter.toSet[EType[VType]].head  // Doesn't matter which edge is chosen.
      val commonNodes = edgesLeft.nodes.toOuter.toSet[VType] intersect embedded.nodes.toOuter.toSet[VType] //difference(edgesLeft, embedded).nodes.toNodeInSet.toSet[VType]
      val visited = new mutable.HashSet[EType[VType]]
      val notVisited = new mutable.ArrayStack[EType[VType]]()

      notVisited push startEdge
      while (!notVisited.isEmpty) {
        val current = notVisited.pop()
        visited += current
        if (!commonNodes.contains(current._1)) {
          pushNonVisited(current._1, notVisited, visited, edgesLeft)
        }
        if (!commonNodes.contains(current._2)) {
          pushNonVisited(current._2, notVisited, visited, edgesLeft)
        }
      }

      var fragmentComp = graph.empty
      for (edge <- visited) fragmentComp += edgesLeft.get(edge)
      val contactVertices = fragmentComp.nodes.toOuter.toSet[VType] intersect commonNodes
      result += new Fragment[VType, EType](fragmentComp, contactVertices)

      edgesLeft = difference(edgesLeft, fragmentComp)
    }

    result.toVector
  }

  /**
   * Pushes neighboring edges of a node that hasn't been visited yet onto a stack.
   */
  private def pushNonVisited(node: VType, stack: mutable.ArrayStack[EType[VType]], visited: mutable.HashSet[EType[VType]], graph: Graph[VType, EType]) {
    val innerVisited: Set[Graph[VType, EType]#EdgeT] = (for {v <- visited } yield graph.get(v)).toSet
    for (neighbor <- graph.get(node).incoming) {
      if (!innerVisited.contains(neighbor)) stack.push(neighbor.toOuter)
    }
  }

  private def difference(a: Graph[VType, EType], b: Graph[VType, EType]): Graph[VType, EType] = {
    val edges: Set[EType[VType]] = (a.edges.toOuter diff b.edges.toOuter).toSet[EType[VType]]
    var g = a.empty
    for (edge <- edges) g = g + a.get(edge)
    g
  }
}
