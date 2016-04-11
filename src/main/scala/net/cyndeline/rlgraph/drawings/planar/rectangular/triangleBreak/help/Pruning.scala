package net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help

import scala.collection.mutable.ListBuffer

/**
 * Breaks triangles by popping 0- and 1-bubbles at their non-trivial vertex v (the one with weight > 1). 0-bubbles take
 * priority. The algorithm then recursively processes any subgraphs resulting from the previous pop. Every vertex v
 * selected this way is returned as the optimal solution to breaking the triangles represented by the bubbles.
 *
 * A k-bubble is defined as a bubble where k vertices has weight > 1.
 */
class Pruning {

  /**
   * @param graph A non-empty bubble graph.
   * @tparam V Vertex type.
   * @return The list of all vertices that was selected as a part in the optimal solution of breaking the 0-bubbles
   *         and 1-bubbles in the graph. Nil if only 2- and 3-bubbles exist.
   */
  def prune[V](graph: BubbleGraph[V]): OptimalSolution[V] = {
    require(!graph.isEmpty, "Cannot prune empty bubble graphs.")

    val w0Bubble = graph.bubbles.find(_.k == 0) // Minimum weight for a vertex is 1, so 3 == 0-bubble

    if (w0Bubble.isDefined) {

      /* Every vertex has the same weight, and it doesn't matter which is removed since the bubble is disconnected
       * from the rest of the graph.
       */
      val vertexToRemove = w0Bubble.get.v1

      /* No need to actually pop bubbles or check for subgraphs since this bubble is alone. If it weren't, one of
       * the vertices would have weight > 1.
       */
      OptimalSolution(Vector(vertexToRemove), Vector())

    } else {

      /* If no 0-bubbles exists, check if there's a 1-bubble. Two 1-weighted and one 2-weighted vertex is the only
       * combination that gives a 1-bubble.
       *
       * Since the graph isn't empty, if no 1-bubble (and by extension no 0-bubble) has been found, the graph still
       * has bubbles in it. If so, return it along with no selected vertices.
       */
      val w1Bubble = graph.bubbles.find(_.k == 1)

      if (w1Bubble.isEmpty) {
        OptimalSolution(Vector(), Vector(graph))

      } else {
        val nonSimplicial = w1Bubble.get.vertices.find(_.weight > 1).get // Must exist if we made it this far
        val subgraphs = graph.popBubbles(nonSimplicial)

        val vertexResult = new ListBuffer[BubbleVertex[V]]()
        val remainingSubgraphs = new ListBuffer[BubbleGraph[V]]()
        vertexResult += nonSimplicial

        for (sub <- subgraphs if !sub.isEmpty) {
          val optimalSolution = prune(sub) // Recursive!
          vertexResult ++= optimalSolution.vertices
          remainingSubgraphs ++= optimalSolution.graphsRemaining
        }

        OptimalSolution(vertexResult.toVector, remainingSubgraphs.toVector)
      }
    }
  }

}
