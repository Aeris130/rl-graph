package net.cyndeline.rlgraph.util.graphConverters

import net.cyndeline.rlgraph.util.GraphCommons

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import scala.reflect.runtime.universe._

/**
  * Maps every vertex of a graph onto a set of n integers from 0 to (n-1) in O(n log n) time.
  */
object IntConverter {

  /**
    * @param g A graph.
    * @tparam V Vertex type in the graph.
    * @return A graph with every vertex mapped to integers, and a vector of vertices where every integer-vertex in the
    *         converted graph stores its original vertex at its value-index in the vector.
    */
  def convert[V](g: Graph[V, UnDiEdge]): (Graph[Int, UnDiEdge], Vector[V]) = {
    val nodes = GraphCommons.outerVertices(g)
    val edges = GraphCommons.outerEdges(g)
    val intNodes: Vector[(V, Int)] = nodes.zipWithIndex
    val intMap = intNodes.toMap
    val intEdges = edges.map(e => intMap(e._1)~intMap(e._2))
    val nodeSet = intNodes.map(_._2)
    (Graph.from(nodeSet, intEdges), nodes)
  }

  def revert[V : TypeTag](g: Graph[Int, UnDiEdge], vs: Vector[V]): Graph[V, UnDiEdge] = {
    val nodes = GraphCommons.outerVertices(g).map(vs)
    val edges = GraphCommons.outerEdges(g).map(e => vs(e._1)~vs(e._2))
    Graph.from(nodes, edges)
  }

}
