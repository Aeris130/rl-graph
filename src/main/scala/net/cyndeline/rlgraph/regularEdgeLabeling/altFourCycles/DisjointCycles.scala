package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * It's possible for essential alternating 4-cycles to share edges (the inner edge of one cycle is the outer edge
 * of another). Flipping one such cycle would result in causing the REL to enter an invalid state.
 *
 * This class applies a heuristic in order to find as many disjoint cycles as possible:
 *
 *  1. Create a graph where every cycle is a vertex.
 *  1. For every pair of cycles sharing an edge, where one (or both) of the cycles has said edge as an inner edge,
 *  add an edge between their vertices in the graph.
 *  1. While the graph has unmarked vertices left:
 *   1. If there's a disjoint cycle (degree 0), mark it to be returned.
 *   1. Else, if there are cycles with degree > 0, select the one with the lowest degree and delete all its neighbors
 *   from the graph, making its disjoint. Mark it to be returned.
 *
 * Any neighbor that is deleted will not be selected as a cycle. By deleting neighbors of vertices with the lowest
 * degree, the heuristic attempts to maximize the amount of cycles that are kept.
 *
 * @tparam V Vertex type used in the cycles.
 */
class DisjointCycles[V : TypeTag] {

  def disjointInnerEdges(cycles: Vector[EdgeData[V]]): Vector[EdgeData[V]] = {
    // Maps cycle edges to the cycles where they appear in the outer (resp. inner) edge set.
    val cyclesWithOuter = new mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]]()
    val cyclesWithInner = new mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]]()
    fillInnerAndOuterMap(cycles, cyclesWithInner, cyclesWithOuter)
    val graphWithIntersections = constructIntersectGraph(cycles, cyclesWithInner, cyclesWithOuter)
    trim(graphWithIntersections, new ListBuffer[EdgeData[V]]())
  }

  /** Recursively deletes the vertex having the lowest degree, and adds it to the result to be returned. Any neighbors
    * are deleted if necessary. */
  private def trim(g: Graph[EdgeData[V], UnDiEdge],
                   result: ListBuffer[EdgeData[V]]): Vector[EdgeData[V]] = {
    if (!g.isEmpty) {
      val lowestDegree = g.nodes.minBy(_.degree)
      result += lowestDegree
      val neighbors = GraphCommons.neighbors[EdgeData[V], UnDiEdge](lowestDegree, g)

      var nextG = g - lowestDegree
      for (n <- neighbors)
        nextG -= n

      trim(nextG, result)
    } else {
      result.toVector
    }
  }

  /** Constructs a graph where each cycle is a vertex, and two vertices are connected with an edge if the two shares
    * an edge that appears as an inner edge in one of them.
    */
  private def constructIntersectGraph(cycles: Vector[EdgeData[V]],
                                      cyclesWithInner: mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]],
                                      cyclesWithOuter: mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]]): Graph[EdgeData[V], UnDiEdge] = {
    var graph = Graph.from[EdgeData[V], UnDiEdge](cycles, Nil)
    for (cycle <- cycles; edge <- cycle.inner ++ cycle.outer) {
      val uop = UnorderedPair(edge.from, edge.to)

      if (cyclesWithInner.contains(uop)) {
        val otherCycles = cyclesWithInner(uop)

        // There will always be at least one cycle per edge (the current one).
        if (otherCycles.size > 1 || (!otherCycles.isEmpty && otherCycles.head != cycle)) {
          for (c <- otherCycles if c != cycle)
            graph += cycle~c
        }
      }
    }

    graph
  }

  /** Maps every inner and outer edge to the cycles it appears in. */
  private def fillInnerAndOuterMap(cycles: Vector[EdgeData[V]],
                                   cyclesWithInner: mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]],
                                   cyclesWithOuter: mutable.HashMap[UnorderedPair[V], mutable.HashSet[EdgeData[V]]]) {
    for (c <- cycles) {
      for (inner <- c.inner) {
        val edge = UnorderedPair(inner.from, inner.to)
        val set = cyclesWithInner.get(edge).getOrElse {
          val newSet = new mutable.HashSet[EdgeData[V]]()
          cyclesWithInner += edge -> newSet
          newSet
        }
        set += c
      }

      for (outer <- c.outer) {
        val edge = UnorderedPair(outer.from, outer.to)
        val set = cyclesWithOuter.get(edge).getOrElse {
          val newSet = new mutable.HashSet[EdgeData[V]]()
          cyclesWithOuter += edge -> newSet
          newSet
        }
        set += c
      }
    }
  }
}
