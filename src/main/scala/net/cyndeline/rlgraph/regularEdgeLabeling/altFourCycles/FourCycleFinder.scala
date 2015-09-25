package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.regularEdgeLabeling.EdgeLabeling

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * An naive O(n2) algorithm that finds every alternating four-cycle in a REL as follows:
 *
 *  1. For every vertex pair (u, v) in the graph, compute the set of common neighbors for both vertices in the pair.
 *  1. Remove any neighbor pair that connects to both u and v using an edge of the same color.
 *  1. Sort the remaining neighbors into two separate sets A, B based on the edge color used when connecting to one of
 *  the vertices u or v (which one is arbitrary, but the same vertex must be used for all neighbors).
 *  1. For each neighbor pair (a, b) in the cross product of A and B, output the cycle (u, a, v, b).
 */
class FourCycleFinder[V : TypeTag] {

  /**
   * @param rel A regular embedding.
   * @return Every undirected cycle of length 4 in the embedding that the REL is based upon, where the edges alternate
   *         between the sets T1 and T2.
   */
  def findAlternatingFourCycles(rel: EdgeLabeling[V]): Vector[Cycle[V]] = {
    val allCycles = new ListBuffer[Cycle[V]]()
    val graph = asGraph(rel, Set(rel.north, rel.south, rel.west, rel.east))
    val vertexPairs = graph.nodes.toVector.combinations(2).map(vector => (vector(0), vector(1)))

    // Only one set is needed, since an edge not in t1 must be in t2.
    val t1Edges = rel.edgesOfT1.map(edge => UnorderedPair(edge.from, edge.to)).toSet

    for (pair <- vertexPairs) {
      val neighbors = commonNeighbors(graph, t1Edges)(pair._1, pair._2)
      val neighborSets = neighborTSets(graph, t1Edges)(pair._1, neighbors) // Arbitrary weather _1 or _2 is used.

      /* Arbitrary which set is t1 or t2, the point is to make sure a pair that connects to one of the pairs vertices
       * using the same T-set isn't considered.
       */
      val cycleNeighbors = for (a <- neighborSets._1; b <- neighborSets._2) yield (a, b)

      for (neighborPair <- cycleNeighbors) {
        allCycles += Cycle.undirected(pair._1, neighborPair._1, pair._2, neighborPair._2)
      }
    }

    allCycles.toVector.distinct
  }

  private def commonNeighbors(g: Graph[V, UnDiEdge],
                             t1Edges: Set[UnorderedPair[V]])
                            (v1: g.NodeT, v2: g.NodeT): Vector[g.NodeT] = {
    val commonNeighbors = (v1.neighbors intersect v2.neighbors)
      .filter(n => {
      val e1 = UnorderedPair[V](v1, n)
      val e2 = UnorderedPair[V](v2, n)

      // Both pair vertices cannot connect to the neighbor using edges of the same T-set membership
      t1Edges.contains(e1) != t1Edges.contains(e2)
    }).toVector
    commonNeighbors
  }

  /** Splits neighbors into two sets, based on weather they connect to a vertex using edges in T1 or T2. */
  private def neighborTSets(g: Graph[V, UnDiEdge],
                            t1Edges: Set[UnorderedPair[V]])
                           (v: g.NodeT, neighbors: Vector[g.NodeT]): (Vector[g.NodeT], Vector[g.NodeT]) = {
    val t1 = new ListBuffer[g.NodeT]()
    val t2 = new ListBuffer[g.NodeT]()

    for (n <- neighbors) {
      val edge = UnorderedPair[V](n, v)
      if (t1Edges.contains(edge))
        t1 += n
      else
        t2 += n
    }

    (t1.toVector, t2.toVector)
  }

  /** Removes the outer vertices(N, W, S, E). */
  private def asGraph(rel: EdgeLabeling[V], outer: Set[V]): Graph[V, UnDiEdge] = {
    val edges = rel.edges.filter(e => !outer.contains(e.from) && !outer.contains(e.to)).map(e => e.from~e.to)
    Graph.from(Nil, edges)
  }

}
