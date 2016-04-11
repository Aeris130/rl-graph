package net.cyndeline.rlgraph.drawings.planar.orthogonal.network

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.ResidualEdge

import scala.collection.Set
import scala.collection.mutable.ArrayBuffer
import scalax.collection.immutable.Graph

/**
 * Finds a negative cycle in a directed weighted network flow, if one exists.
 *
 * @constructor Creates a new cycle detection object.
 */
class BellmanFordCycleDetection {
  private val shortestPath = new BellmanFordShortestPath()

  /**
   * Computes a negative cycle. Note that this operation modifies mutable data in
   * the edges and vertices of the graph.
 *
   * @param graph A residual representation of a directed weighted network.
   * @param source The vertex from which every negative cycle can be reached. If the network lacks such a vertex
   *               (if it contains multiple sinks for example), it is up to the user to create a super-sink connecting
   *               to the remaining ones.
   * @return A cycle as a list of edges in the order they appear in it. The reason for returning edges
   *         instead of vertices is that multi-edges may be present in the network.
   *
   */
  def findCycle(graph: Graph[Int, ResidualEdge], source: Int): Option[Vector[ResidualEdge[Int]]] = {

    val result: (Map[Int, ResidualEdge[Int]], Map[Int, Int], Option[ResidualEdge[Int]]) = shortestPath.findParentsDistanceAndCycleMember(source, graph)
    val parents = result._1
    val start = result._3

    /* Step 4: Normally the algorithm stops here. But since the goal is not just to find the presence of
     * a cycle, but also the cycle itself, the assigned parents may be used for this.
     */

    /* If start is != null, a negative cycle exists. Follow its parents backwards until an already visited
     * edge is found (not necessarily the start edge). That is the start of the cycle along with the previous
     * visited edges beginning from the edge.
     */
    if (!start.isDefined) return None

    val edgesInCycle = new ArrayBuffer[ResidualEdge[Int]]()
    var addedEdges = Set[ResidualEdge[Int]]()
    var current: ResidualEdge[Int] = start.get
    var cycleFound = false
    do {
      val edgeToAd = current

      /* Every time toEdgeIn is called, a new instance of the edge is created. The set of original edges
       * is used to check equality based on instances-
       */
      if (addedEdges.contains(current)) {
        cycleFound = true

        /* one or more edges may be present in the beginning of the list that doesn't belong to the cycle. Remove
         * every element up until the cycle start is found (start = the first element traversed twice).
         */
        var done = false
        val compare = current
        while (!edgesInCycle.isEmpty && !done) {
          if (edgesInCycle.head != compare) edgesInCycle.remove(0)
          else done = true
        }

        /* Since parents are tracked backwards from .to to .from, the result must be reversed. */
        return Option(edgesInCycle.toVector.reverse)
      }

      edgesInCycle += edgeToAd
      addedEdges += current
      current = parents.get(graph.get(current.from)).get // Lots of converting back and forth, ugh...
    } while (!cycleFound)


    None
  }

}
