package net.cyndeline.rlgraph.util.graphConverters.jGraphT

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge, SimpleGraph}
import org.jgrapht.{DirectedGraph, UndirectedGraph}

import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.immutable.{Graph => ScalaGraph}

/**
 * Converts a scala-graph to JGraphT.
 *
 * @constructor Creates a new converter.
 */
class ScalaGraphToJGraphTConverter {

  /**
   * Adds every vertex and edge of a scala-graph to a JGraphT graph.
   *
   * @param graph Scala graph to convert.
   * @return a JGraphT representation of the scala graph.
   */
  def convert[V, E[X] <: UnDiEdge[X]](graph: ScalaGraph[V, E]): UndirectedGraph[V, DefaultEdge] = {
    val jGraphTGraph = new SimpleGraph[V, DefaultEdge](classOf[DefaultEdge])

    val nodes = graph.nodes.iterator
    while (nodes.hasNext) {
      val n = nodes.next()
      jGraphTGraph.addVertex(n)
    }

    val edges = graph.edges.iterator
    while (edges.hasNext) {
      val e = edges.next()
      jGraphTGraph.addEdge(e._1, e._2)
    }

    jGraphTGraph
  }

  def convertToDirected[V, E[X] <: DiEdge[X]](graph: ScalaGraph[V, E]): DirectedGraph[V, DefaultEdge] = {
    val jGraphTGraph = new DefaultDirectedGraph[V, DefaultEdge](classOf[DefaultEdge])

    val nodes = graph.nodes.iterator
    while (nodes.hasNext) {
      val n = nodes.next()
      jGraphTGraph.addVertex(n)
    }

    val edges = graph.edges.iterator
    while (edges.hasNext) {
      val e = edges.next()
      jGraphTGraph.addEdge(e._1, e._2)
    }

    jGraphTGraph
  }

}
