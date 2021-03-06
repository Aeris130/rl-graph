package net.cyndeline.rlgraph.cycles

import net.cyndeline.rlgraph.util.GraphCommons

import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph


/**
 * Finds an arbitrary cycle from a graph in O(V + E) time.
 *
 * @param vertexFilter Determines if a vertex is eligible to be a member of a cycle. Returns true if the vertex
 *                     can be in a cycle, otherwise false.
 * @tparam VType Vertex type used in graph.
 * @tparam EType Edge type used in graph.
 */
class UndirectedSingleCycleFinder[VType, EType[X] <: EdgeLikeIn[X]](vertexFilter: VType => Boolean)
  extends SingleCycleFinder[VType, EType] {

  /**
   * Constructs a default vertex finder.
   * @return A cycle finder with no vertex filter set.
   */
  def this() = this((v: VType) => true)

  /**
   * Computes a cycle from an arbitrary vertex. Only visits vertices approved by the vertex filter.
   * @param graph Graph to search for cycle in.
   * @return The vertices of the cycle, or None if no cycle exists.
   */
  def findCycle(graph: Graph[VType, EType]): Option[Cycle[VType]] = {
    val start = graph.nodes.find(vertexFilter(_)).getOrElse( return None )
    findCycleFromVertex(start, graph)
  }

  /**
   * Computes a cycle that start at a specified vertex. Only visits vertices approved by the vertex filter, except
   * for the start vertex.
   *
   * @param start Start vertex for the cycle.
   * @param graph Graph to search for cycle in.
   * @return The vertices of the cycle (with the start vertex as the head), or None if no cycle exists.
   */
  def findCycleFromVertex(start: VType, graph: Graph[VType, EType]): Option[Cycle[VType]] = {
    val result = computeCycleFromStart(start, start, start, graph, Vector[VType](), Set(start))
    if (result.isDefined) {
      val cycle = result.get
      Some(Cycle(start +: cycle.dropRight(1))) // The start vertex ends up last in the list, so remove it and prepend it again.
    } else {
      None
    }
  }

  private def computeCycleFromStart(start: VType,
                                    currentVertex: VType,
                                    parent: VType,
                                    graph: Graph[VType, EType],
                                    currentCycle: Vector[VType],
                                    visited: Set[VType]): Option[Vector[VType]] = {
    for (neighbor <- GraphCommons.neighbors(currentVertex, graph).filter(n => vertexFilter(n) || n == start) if neighbor != parent && (!visited.contains(neighbor) || neighbor == start)) { // Must have n == start rather than start == n
      if (neighbor == start) {
        return Some(currentVertex +: currentCycle)
      }
      computeCycleFromStart(start, neighbor, currentVertex, graph, currentVertex +: currentCycle, visited + currentVertex) match {
        case Some(cycle) => return Some(cycle)
        case None =>
      }
    }

    None
  }

}
