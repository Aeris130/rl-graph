package net.cyndeline.rlgraph.traversal

import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
  * Result from traversing a graph in Breadth First Order.
  *
  * @param vertices All traversed vertices in the order they were traversed.
  * @param edges All traversed edges in the order they were traversed.
  */
class BFSTraversal[V, E[X] <: EdgeLikeIn[X]] private (val vertices: Vector[V], val edges: Vector[E[V]]) {

  override def equals(other: Any): Boolean = other match {
    case bfs: BFSTraversal[V, E] => bfs.vertices == vertices && bfs.edges == edges
    case _ => false
  }

  override def hashCode: Int = vertices.## ^ edges.##
}

/**
  * Traverses a graph in Breadth First Order.
  */
object BFSTraversal {

  /**
    * @param graph Graph to traverse.
    * @param start Starting point in the graph to begin traversal from. Must eb present in the graph.
    * @tparam V Vertex type in the graph.
    * @tparam E Edge type in the graph.
    * @return Every vertex and edge that was traversed, in BFS order.
    */
  def apply[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E], start: V): BFSTraversal[V, E] = {
    val defaultVisitor = (from: V, to: V, e: E[V]) => ()
    apply[V, E](graph, start, None, defaultVisitor)
  }

  /**
    * @param graph Graph to traverse.
    * @param start Starting point in the graph to begin traversal from. Must eb present in the graph.
    * @param maxDepth The number of neighbor levels that algorithm should traverse before terminating and returning
    *                 the result. Must be 0 or higher (0 == no max depth).
    * @tparam V Vertex type in the graph.
    * @tparam E Edge type in the graph.
    * @return Every vertex and edge that was traversed, in BFS order.
    */
  def apply[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E], start: V, maxDepth: Int): BFSTraversal[V, E] = {
    require(maxDepth >= 0, "Cannot set negative max depth.")
    val defaultVisitor = (from: V, to: V, e: E[V]) => ()
    apply[V, E](graph, start, Some(maxDepth), defaultVisitor)
  }

  /**
    *
    * @param graph Graph to traverse.
    * @param start Starting point in the graph to begin traversal from. Must eb present in the graph.
    * @param maxDepth The number of neighbor levels that algorithm should traverse before terminating and returning
    *                 the result. Must be 0 or higher (0 == no max depth).
    * @param visitor Called whenever an edge is traversed using (in order): The vertex that the edge traversal starts
    *                at, the vertex it ends at, and the edge that is traversed.
    * @tparam V Vertex type in the graph.
    * @tparam E Edge type in the graph.
    * @return Every vertex and edge that was traversed, in BFS order.
    */
  def apply[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E], start: V, maxDepth: Int, visitor: (V, V, E[V]) => Unit): BFSTraversal[V, E] = {
    require(maxDepth >= 0, "Cannot set negative max depth.")
    apply[V, E](graph, start, Some(maxDepth), visitor)
  }

  private def apply[V, E[X] <: EdgeLikeIn[X]](graph: Graph[V, E], start: V, maxDepth: Option[Int], visitor: (V, V, E[V]) => Unit): BFSTraversal[V, E] = {
    require(graph.contains(start), "Could not find start vertex " + start + " in the graph " + graph)
    val result = bfs[V, E](graph, Queue[V]().enqueue(start), Queue(), Vector[V](), Vector[E[V]](), 0, maxDepth.getOrElse(0), visitor)
    new BFSTraversal[V, E](result._1.reverse, result._2.reverse)
  }

  private def bfs[V, E[X] <: EdgeLikeIn[X]](remainingGraph: Graph[V, E],
                                            currentVertices: Queue[V],
                                            nextLevel: Queue[V],
                                            resultV: Vector[V],
                                            resultE: Vector[E[V]],
                                            currentDepth: Int,
                                            maxDepth: Int,
                                            visitor: (V, V, E[V]) => Unit): (Vector[V], Vector[E[V]]) = if (maxDepth == 0 || currentDepth < maxDepth) {

    if (currentVertices.isEmpty && nextLevel.isEmpty) {
      (resultV, resultE)
    } else if (currentVertices.isEmpty && nextLevel.nonEmpty) {
      bfs(remainingGraph, nextLevel, Queue(), resultV, resultE, currentDepth + 1, maxDepth, visitor)
    } else {
      val next = currentVertices.dequeue
      val nextVertex = next._1
      val remainingQueue = next._2
      val neighbors = GraphCommons.outerEdgeNeighbors(nextVertex, remainingGraph)
      val newResultV = nextVertex +: resultV

      /* Since edges are added based on the next level of vertices, only add them if this isn't the last level. */
      val newResultE = if (maxDepth == 0 || currentDepth < maxDepth - 1) {
        var updatedEdgeResult = resultE

        /* Although this could be solved using fewer LOC using maps an concatenating the result, adding the edges
         * one by one saves us the trouble of computing which order they were added in in order to ensure that the
         * visitor maintains the same ordering.
         */
        for (n <- neighbors) {
          updatedEdgeResult = n._2 +: updatedEdgeResult
          visitor(nextVertex, n._1, n._2) // Call the visitor since an edge is being added
        }

        updatedEdgeResult

      } else {
        resultE
      }

      val newNextLevel = nextLevel.enqueue(neighbors.map(_._1))
      val updatedGraph = remainingGraph - nextVertex

      bfs(updatedGraph, remainingQueue, newNextLevel, newResultV, newResultE, currentDepth, maxDepth, visitor)
    }

  } else {
    (resultV, resultE)
  }

}
