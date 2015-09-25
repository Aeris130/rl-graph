package net.cyndeline.rlgraph.pathfinding

import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable.ListBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Recursively finds the longest path in a tree, given a starting node.
 */
class LongestTreePath[V, E[X] <: UnDiEdge[X]] {

  def computeLongestPath(tree: Graph[V, E], start: V): Path[V, E] = {
    require(tree.contains(start), "The starting point was not found in the tree, cannot compute longest path.")
    require(tree.isAcyclic, "Cannot compute tree paths on a cyclic graph.")
    if (tree.nodes.size == 1)
      return Path[V, E](start)

    val initialNeighbors = GraphCommons.outerEdgeNeighbors(start, tree)
    val paths = for (n <- initialNeighbors) yield findLongest(tree)(start, n._1, Vector(n._1, start), Vector(n._2), 0)
    val longestPath = paths.maxBy(_._3)

    Path[V, E](start, longestPath._2.reverse)
  }

  private def findLongest(g: Graph[V, E])(previous: V, current: V, vPath: Vector[V], ePath: Vector[E[V]], length: Int): (Vector[V], Vector[E[V]], Int) = {
    if (g.get(current).degree < 2) {
      (vPath, ePath, length)
    } else {
      val neighbors = GraphCommons.outerEdgeNeighbors(current, g)
      val subLengthResults = new ListBuffer[(Vector[V], Vector[E[V]], Int)]()
      val nIt = neighbors.iterator
      while (nIt.hasNext) {
        val n = nIt.next()
        if (n._1 != previous) {
          subLengthResults += findLongest(g)(current, n._1, n._1 +: vPath, n._2 +: ePath, length + 1)
        }
      }

      subLengthResults.maxBy(_._3)
    }
  }

}
