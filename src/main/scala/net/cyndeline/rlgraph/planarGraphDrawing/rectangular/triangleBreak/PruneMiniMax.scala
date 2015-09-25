package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak

import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.help.{BubbleGraph, BubbleVertex, MiniMax, Pruning}

import scala.collection.mutable.ListBuffer

/**
 * A heuristic used to compute a (mostly) optimal solution to the triangle-breaking problem regarding drawings of
 * rectangular duals (given that some triangles may share an edge, how to split all triangles while splitting
 * the fewest amount of edges). This heuristic repeatedly runs two subroutines, Pruning and MiniMax, until every
 * separating triangle in a graph has been split through an edge (splitting a triangle implies that one of its edges
 * are split, with a new vertex joining the two resulting edges).
 */
class PruneMiniMax {
  private val pruning = new Pruning()
  private val miniMax = new MiniMax()

  /**
   * @param graph A non-empty bubble-graph.
   * @tparam V Vertex type in the graph.
   * @return A (mostly) optimal solution to splitting the triangles represented by the graph such that the amount of
   *         edges split is kept minimal, with every vertex in the list representing an edge to be split.
   */
  def applyHeuristic[V](graph: BubbleGraph[V]): Vector[BubbleVertex[V]] = {
    require(!graph.isEmpty, "Cannot reduce separating triangles in an empty bubble graph.")
    val optimalSolution = new ListBuffer[BubbleVertex[V]]()
    val currentGraphs = new ListBuffer[BubbleGraph[V]]()

    currentGraphs += graph
    while (!currentGraphs.isEmpty) {
      val prunedGraphs = new ListBuffer[BubbleGraph[V]]()

      for (g <- currentGraphs) {
        val solution = pruning.prune(g)
        optimalSolution ++= solution.vertices
        prunedGraphs ++= solution.graphsRemaining.filter(e => !e.isEmpty)
      }

      currentGraphs.clear()

      for (pruned <- prunedGraphs) {
        val solution = miniMax.miniMaxReduction(pruned)
        optimalSolution ++= solution.vertices
        currentGraphs ++= solution.graphsRemaining.filter(e => !e.isEmpty)
      }
    }

    optimalSolution.toVector
  }
}
