package net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help

/**
 * A sub-process for computing the optimal triangle split solution. Given a bubble-graph, this object selects the
 * bubble with the lowest weight (the combined weight of all its vertices) and pops it through its maximum-weighted
 * vertex.
 */
class MiniMax {

  /**
   * @param graph A non-empty bubble graph.
   * @tparam V Vertex type in the graph.
   * @return The vertex with the highest weight in the lowest weighted bubble, and the graphs that results from
   *         popping through it.
   */
  def miniMaxReduction[V](graph: BubbleGraph[V]): OptimalSolution[V] = {
    require(!graph.isEmpty, "Attempted MiniMax reduction on empty bubble graph.")
    val minimumWeightBubble = graph.bubbles.minBy(_.weight)
    val maxWeightVertex = minimumWeightBubble.vertices.maxBy(_.weight)
    val subgraphs = graph.popBubbles(maxWeightVertex)
    val graphs = if (subgraphs.size == 1 && subgraphs.head.isEmpty) Vector() else subgraphs
    OptimalSolution(Vector(maxWeightVertex), graphs)
  }

}
