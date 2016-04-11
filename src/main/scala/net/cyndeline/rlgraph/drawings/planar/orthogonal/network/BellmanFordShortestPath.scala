package net.cyndeline.rlgraph.drawings.planar.orthogonal.network

import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.ResidualEdge

import scalax.collection.immutable.Graph

/**
 * Computes the shortest path data (parents and distance) in a residual network, taking edges with
 * negative cost into account.
 *
 * @constructor Creates a new Bellman Ford path computation object.
 */
class BellmanFordShortestPath {

  /**
   * Finds the parent of each vertex when traversing the shortest path in the network. Throws an exception if a negative
   * cycle exists.
   *
   * @param source Vertex to compute parents in shortest path to, from every other vertex .
   * @param network Network to compute shortest path in. Must contain the specified vertex.
   * @return every vertex id of the network, mapped against its parent when traversing the shortest paths from the
   *         given source. Every id is mapped against a network edge. The .To vertex is the id being mapped, and
   *         the .From vertex is the parent (meaning each edge is traversed backwards).
   */
  def findParentRelationWithNoCycle(source: Int, network: Graph[Int, ResidualEdge]): Map[Int, ResidualEdge[Int]] = {
    val result: (Map[Int, ResidualEdge[Int]], Map[Int, Int], Option[ResidualEdge[Int]]) = findParentsDistanceAndCycleMember(source, network)

    if (result._3.isDefined)
      throw new IllegalArgumentException("Negative cycle detected in " + network)
    else
      result._1
  }

  /**
   * Finds the distance from a source to every reachable vertex in the network. Throws an exception if
   * a negative cycle is found.
   *
   * @param source Vertex to compute distance from every other vertex to.
   * @param network Network to compute shortest path in. Must contain the specified vertex.
   * @return every vertex id of the network, mapped against its distance to the source.
   */
  def findDistanceWithNoCycle(source: Int, network: Graph[Int, ResidualEdge]): Map[Int, Int] = {
    val result: (Map[Int, ResidualEdge[Int]], Map[Int, Int], Option[ResidualEdge[Int]]) = findParentsDistanceAndCycleMember(source, network)

    if (result._3.isDefined)
      throw new IllegalArgumentException("Negative cycle detected in " + network)
    else
      result._2
  }

  /**
   * Finds the parent of each vertex when traversing the shortest path in the network, the shortest distance from
   * every vertex in a graph to a specific vertex. If a negative cycle is found, an edge where the distance to its
   * .To vertex is less than the distance to .From + edge weight is also returned.
   *
   * @param source Vertex to compute shortest path from every other vertex to.
   * @param network Network to compute shortest path in. Must contain the specified vertex.
   * @return every vertex id of the network, mapped against its parent when traversing shortest paths from the
   *         given source. Also returns the distance to every vertex in the network when traversing from the source
   *         (distance = edge costs) and an edge that is a part of a negative cycle if one exists.
   */
  def findParentsDistanceAndCycleMember(source: Int, network: Graph[Int, ResidualEdge]): (Map[Int, ResidualEdge[Int]], Map[Int, Int], Option[ResidualEdge[Int]]) = {

    /* The distance from the source to any given vertex in the network. None implies infinite distance. */
    var distance = Map[network.NodeT, Option[Int]]()

    /* Maps each vertex to its predecessor on the shortest path (the path with the lowest cost) from
     * the vertex to the source. The edge that is assigned has parent has the child as .to, and the parent
     * vertex as .from.
     */
    var parents = Map[network.NodeT, network.EdgeT]()

    /* Step 1: Setup distances and parents for all vertices. */
    for (node: network.NodeT <- network.nodes) {
      if (node == source) distance += (node -> Some(0))
      else distance += (node -> None)
    }

    /* Step 2: Relax all edges */
    for (i <- 1 to network.nodes.size) {
      for (edge: network.EdgeT <- network.edges) {
        val fromDistance = distance.get(edge.from).get
        val toDistance = distance.get(edge.to).get

        /* No point comparing two infinite distances. */
        if (fromDistance.isDefined && (!toDistance.isDefined || (fromDistance.get + edge.cost) < toDistance.get)) {
          distance += (edge.to -> Option(fromDistance.get + edge.cost))
          parents += (edge.to -> edge)
        }
      }

    }

    /* Step 3: Check for negative weight cycles. */
    var start: Option[network.EdgeT] = None
    var found = false
    for (edge: network.EdgeT <- network.edges if !found) {
      val fromDistance = distance.get(edge.from).get
      val toDistance = distance.get(edge.to).get

      // If there's no path from the start vertex A to some other vertex B, B will have no distance even after the relaxation.
      if (fromDistance.isDefined && toDistance.isDefined && (fromDistance.get + edge.cost) < toDistance.get) {
        start = Option(edge)
        found = true
      }
    }

    val parentMapping: Map[Int, ResidualEdge[Int]] = parents.map(kv => {
      val id: Int = kv._1
      (id, kv._2.toOuter)
    })

    val distanceMapping: Map[Int, Int] = distance.filter(kv => kv._2.isDefined).map(kv => {
      val id: Int = kv._1
      (id, kv._2.get)
    })

    if (!start.isDefined)
      (parentMapping, distanceMapping, None)
    else
      (parentMapping, distanceMapping, Option(start.get.toOuter))
  }
}
