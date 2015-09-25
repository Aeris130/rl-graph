package net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes the set of paths between two vertices that appears when the elementary cycle is removed from a graph.
 * If removing the elementary cycle results in a tree, the tree is decomposed into paths.
 *
 * Example: The tree 1-2, 2-3, 3-4, 4-5, 3-6, 6-7 will be decomposed into paths 5,4,3,2,1 + 7,6,3 or
 * 7,6,3,2,1 + 5,4,3 assuming vertex 1 lies on the elementary cycle.
 */
class DisjointPathParser[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @param elementaryCycle A closed tour, or an empty list if the input graph is a tree.
   * @param graph A connected graph whose vertex set is a super-set of the elementary cycle.
   * @return The list of disjoint paths that are created when removing the edges in the cycle.
   */
  def computePaths(elementaryCycle: Vector[VType], graph: Graph[VType, EType]): Vector[DisjointPath[VType]] = {
    var graphWithoutElementaryCycle = graph
    var resultingPaths = Vector[DisjointPath[VType]]()
    for (vertex <- elementaryCycle) {
      val n = graphWithoutElementaryCycle.get(vertex)

      /* If degree >= 3, it means there's a disjoint path that still needs the vertex.
       * Use the original graph when checking degree, the modified one will have its vertices
       * along the cycle get degree < 3 when neighbors are removed.
       */
      if (graph.get(vertex).degree == 2) {
        graphWithoutElementaryCycle -= n
      }
    }

    /* Start by computing paths from dead ends up until the first node with degree > 2 or == 1. Once done, only
     * paths from one part of the cycle to another remains. Then traverse the remaining paths from one node with
     * degree 1 to another. Remove visited nodes except the ones with degree > 2. Keep going until graph is empty.
     */
    var deadEnds = Set[VType]()
    var endsConnectedToGraph = Set[VType]()
    val fixedGraphType = graphWithoutElementaryCycle
    for (node <- fixedGraphType.nodes.filter(_.degree == 1)) {
      if (graph.get(node).degree == 1)
        deadEnds += node
      else
        endsConnectedToGraph += node
    }

    for (pathStart <- deadEnds.toVector ++ endsConnectedToGraph if graphWithoutElementaryCycle.contains(pathStart)) {
      val disjointPath = parseDisjointPath(pathStart, graphWithoutElementaryCycle, deadEnds)
      resultingPaths = disjointPath +: resultingPaths

      /* Filtering needs to be done before vertices are removed, otherwise those with degree > 2 will also have their
       * degree reduced by losing neighbors, and subsequently be removed.
       */
      val graphBeforeRemoval = graphWithoutElementaryCycle
      for (pathVertex <- disjointPath.path.filter((v: VType) => graphBeforeRemoval.get(v).degree < 3)) {
        graphWithoutElementaryCycle -= pathVertex
      }
    }

    if (!graphWithoutElementaryCycle.isEmpty)
      throw new Error("Not every path vertex was moved to a disjoint path. Topology still intact: " + graphWithoutElementaryCycle)

    resultingPaths
  }

  private def parseDisjointPath(start: VType, graph: Graph[VType, EType], deadEnds: Set[VType]): DisjointPath[VType] = {
    val disjointPath = graph.get(start)
      .pathUntil(n => n.degree == 1 || n.degree >= 3)
      .getOrElse(throw new Error("No ending node with degree < or > than 2 present, disjoint cycle found, starting at " + start + " in " + graph))
    val vertexPath = disjointPath.nodes.map(n => {
      val outer: VType = n
      outer
    }).toVector

    if (deadEnds.contains(start))
      DisjointPath(vertexPath, start)
    else
      DisjointPath(vertexPath)
  }

}
