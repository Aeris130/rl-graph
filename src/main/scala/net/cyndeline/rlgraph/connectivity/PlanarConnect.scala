package net.cyndeline.rlgraph.connectivity

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal
import scalax.collection.immutable.Graph

/**
 * Connects single vertices and biconnected components into a component path. The algorithm attempts to
 * minimize the increase in maximum degree by adding additional edges to the vertex with the lowest degree
 * in each component.
 */
class PlanarConnect[VType](g: Graph[VType, UnDiEdge]) {

  /** @return The graph after it has been connected. */
  def graph: Graph[VType, UnDiEdge] = computeGraphAndEdges._1

  /** @return Every edge that was added to achieve connectivity. */
  def extraEdges: Vector[(VType, VType)] = computeGraphAndEdges._2

  private val computeGraphAndEdges: (Graph[VType, UnDiEdge], Vector[(VType, VType)]) = {
    var allComponents = computeConnectedComponents(g)

    if (allComponents.isEmpty) {
      (g.empty, Vector[(VType, VType)]())
    } else {
      val allEdges = new ListBuffer[(VType, VType)]()
      var currentGraph = g
      var previousComponent = allComponents.head
      allComponents = allComponents.drop(1)

      while (allComponents.nonEmpty) {
        val nextComponent = allComponents.head
        val lowestDegreePrevious = lowestDegreeVertex(currentGraph, previousComponent)
        val lowestDegreeNext = lowestDegreeVertex(currentGraph, nextComponent)
        allEdges += ((lowestDegreePrevious, lowestDegreeNext))
        currentGraph += lowestDegreePrevious~lowestDegreeNext
        allComponents = allComponents.drop(1)
        previousComponent = nextComponent
      }

      (currentGraph, allEdges.toVector)
    }
  }

  private def lowestDegreeVertex(g: Graph[VType, UnDiEdge], vs: Set[VType]): VType = vs.minBy(g.get(_).degree)

  private def computeConnectedComponents(graph: Graph[VType, UnDiEdge]): Vector[Set[VType]] = {
    var current = graph
    val result = new ListBuffer[Set[VType]]()

    while (current.nonEmpty) {
      val arbitraryStart = current.nodes.head
      val reachedNodes = arbitraryStart.outerNodeTraverser.withKind(GraphTraversal.DepthFirst).toSet
      result += reachedNodes

      for (n <- reachedNodes)
        current -= n
    }

    result.toVector
  }
}

object PlanarConnect {
  def apply[VType : TypeTag](g: Graph[VType, UnDiEdge]) = new PlanarConnect(g)
}
