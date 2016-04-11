package net.cyndeline.rlgraph.pathfinding

import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

object BFSPathfinder {
  def apply() = new BFSPathfinder()
}

/**
 * Traverses vertices of a graph in BFS order to find a path between two vertices. The first instance of such a path
 * is returned.
 */
class BFSPathfinder {

  /**
   * @param from The start vertex on the path.
   * @param to The stop vertex on the path. May be the same as the start vertex.
   * @param graph The graph to search for a path in.
   * @tparam VType Vertex type in the graph.
   * @tparam EType Edge type in the graph.
   * @return A path from -> to if one exists, otherwise None.
   */
  def computePath[VType, EType[X] <: EdgeLikeIn[X]](from: VType, to: VType, graph: Graph[VType, EType]): Option[Path[VType, EType]] = {
    val edges = new ListBuffer[EType[VType]]()

    if (from == to)
      return Some(Path[VType, EType](from))

    val parents = pathBetween(from, to, graph).getOrElse{ return None }
    var current = to
    while (current != from) {
      val parent = parents(current)
      edges += parent.e
      current = parent.v
    }

    Some(Path(from, edges.toVector.reverse))
  }

  /**
   * @param from Vertex to start path from.
   * @param to Vertex to end path in.
   * @param graph Graph containing both vertices.
   * @return Every node mapped to its parent along the found path, or None if no path is available.
   */
  private def pathBetween[VType, EType[X] <: EdgeLikeIn[X]](from: VType, to: VType, graph: Graph[VType, EType]): Option[mutable.HashMap[VType, Parent[VType, EType]]] = {
    require(graph.contains(from), "Start vertex " + from + " was not present in the graph.")
    require(graph.contains(to), "Stop vertex " + to + " was not present in the graph.")

    val parent = new mutable.HashMap[VType, Parent[VType, EType]]()
    val visited = new mutable.HashSet[VType]()
    val vertexQueue = new mutable.Queue[VType]()
    vertexQueue.enqueue(from)

    var goalFound = false
    while(vertexQueue.nonEmpty && !goalFound) {
      val next = vertexQueue.dequeue()
      val out = graph.get(next).outgoing // Both directed and undirected edges
        .filter(e => !visited.contains(GraphCommons.oppositeVertex(next, e.toOuter)))

      for (neighborEdge <- out) {
        val neighbor = GraphCommons.oppositeVertex(next, neighborEdge.toOuter)
        parent += neighbor -> Parent(next, neighborEdge.toOuter)
        visited += neighbor

        if (neighbor == to)
          goalFound = true

        vertexQueue.enqueue(neighbor)
      }
    }

    if (!goalFound) {
      None
    } else {
      Some(parent)
    }
  }

  private case class Parent[VType, EType[X] <: EdgeLikeIn[X]](v: VType, e: EType[VType])
}
