package net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.help

import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Keeps track of biconnected components that vertices in a graph belongs to, and merges them together.
 */
class BiconnectedComponentMerger[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType]) {
  private val componentSearch = new DFSComponentSearch[VType, EType]()
  private val componentsAndCutpoints: Vector[(Graph[VType, EType], Set[VType])] = componentSearch.componentsAndArticulationPoints(graph)
  private val cutpointsInGraph = componentsAndCutpoints.map(v => v._2).flatten.toSet

  /* Map every vertex in a biconnected component to the components it belongs to. Each component is represented
   * only by the vertices it contains.
   */
  private var componentsThatVerticesLieIn: Map[VType, Set[Set[VType]]] = computeComponentsForVertices({

    /* Map every biconnected component to its vertex set. */
    componentsAndCutpoints
      .map(_._1)
      .map(g => g -> g.nodes.map(n => {val outer: VType = n; outer}).toSet)
      .toMap
  })

  /**
   * Every cut vertx in the graph.
   */
  val cutPoints: Vector[VType] = cutpointsInGraph.toVector

  /**
   * @param a A vertex belonging to a biconnected component.
   * @param b A vertex belonging to a different biconnected component than a.
   * @return True if a and b belongs to the same biconnected component, otherwise false.
   */
  def sharesBiconnectedComponent(a: VType, b: VType): Boolean = componentsOf(a).exists(component => component.contains(b))

  /**
   * Merges the biconnected components of two vertices.
   * @param a A vertex belonging to a biconnected component.
   * @param b A vertex belonging to a different biconnected component than a.
   * @param cutPoint The cut point joinging the biconnected components of a and b.
   */
  def merge(a: VType, b: VType, cutPoint: VType): Unit = {

    // Find the components comtaining both a/cutpoint and b/cutpoint
    val aComponent = componentThatContains(a, cutPoint)
    val bComponent = componentThatContains(b, cutPoint)

    val mergedComponent = aComponent ++ bComponent

    // Remove a/b component from every vertex in it, and map it to the new component instead
    updateMergedComponent(aComponent, mergedComponent)
    updateMergedComponent(bComponent, mergedComponent)
  }

  private def componentThatContains(a: VType, b: VType): Set[VType] = componentsOf(a).find(c => c.contains(b)).getOrElse {
    throw new NoSuchElementException("No biconnected components contains vertices " + a + " and " + b)
  }

  private def updateMergedComponent(oldComponent: Set[VType], mergedComponent: Set[VType]) {
    for (vertex <- oldComponent) {
      val allComponentsForVertex = componentsOf(vertex)
      componentsThatVerticesLieIn += (vertex -> (allComponentsForVertex - oldComponent + mergedComponent))
    }
  }

  def componentsOf(a: VType): Set[Set[VType]] =
    componentsThatVerticesLieIn
    .get(a)
    .getOrElse(throw new NoSuchElementException("No biconnected components found for vertex " + a))

  private def computeComponentsForVertices(componentToVertexSet: Map[Graph[VType, EType], Set[VType]]): Map[VType, Set[Set[VType]]] = {
    var componentsThatVerticesLieIn = Map[VType, Set[Set[VType]]]()
    for (component <- componentsAndCutpoints.map(gc => gc._1)) { // Only need the graph
      for (node <- component.nodes) {
        val outer: VType = node
        val currentComponents = componentsThatVerticesLieIn.get(outer).getOrElse {
          Set[Set[VType]]()
        }

        componentsThatVerticesLieIn += (outer -> (currentComponents + componentToVertexSet(component)))
      }
    }

    componentsThatVerticesLieIn
  }

}
