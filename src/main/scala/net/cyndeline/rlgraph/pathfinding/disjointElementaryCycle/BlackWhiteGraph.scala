package net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.pathfinding.disjointElementaryCycle.help.{DisjointPath, DisjointPathParser, ElementaryCycleFinder}

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * A graph representing the edge relationships between subgraphs and vertices connecting to them. The graph
 * represents this using a bipartite graph with black and white nodes. Black nodes represents single vertices,
 * while white nodes represents subgraphs. All white vertices has degree 2, while black vertices has degree 1 or more.
 */
class BlackWhiteGraph[VType : TypeTag] {
  private var graph = Graph[BWNode[VType], UnDiEdge]()
  private val singleVertexMapping = new mutable.HashMap[VType, BWNode[VType]]()
  private val subgraphMapping = new mutable.HashMap[Embedding[VType], BWNode[VType]]()

  /* helper clases used when computing which vertex/subgraph pairs that should receive edges. */
  private val elemCycleFinder = new ElementaryCycleFinder[BWNode[VType], UnDiEdge]()
  private val disjointPathParsing = new DisjointPathParser[BWNode[VType], UnDiEdge]()

  /**
   * Adds a relationship between two vertices to a subgraph.
   * @param subgraph A subgraph that is not yet represented in the graph. This constraint is needed since subgraphs
   *                 all have degree 2.
   * @param singleVertex1 A single vertex in the graph, connected to some vertex in the subgraph. This vertex may
   *                      have been added previously as connected to another subgraph.
   * @param singleVertex2 Same as 1, but a different vertex.
   */
  def addNodes(subgraph: Embedding[VType], singleVertex1: VType, singleVertex2: VType): Unit = {
    if (singleVertex1 == singleVertex2)
      throw new IllegalArgumentException("The vertex " + singleVertex1 + " cannot connect to a subgraph with two edges, distinct vertices must be used.")
    else if (subgraphMapping.contains(subgraph))
      throw new IllegalArgumentException("The subgraph " + subgraph + " cannot connected to single vertices more than once, or its degree would exceed 2.")

    val subgraphNodeToAdd = BWNode(subgraph)
    subgraphMapping += (subgraph -> subgraphNodeToAdd)

    val singleVertex1ToAdd = singleVertexMapping.getOrElse(singleVertex1, BWNode(singleVertex1))
    val singleVertex2ToAdd = singleVertexMapping.getOrElse(singleVertex2, BWNode(singleVertex2))

    graph += singleVertex1ToAdd~subgraphNodeToAdd
    graph += singleVertex2ToAdd~subgraphNodeToAdd
  }

  /**
   * Selects alternate edges from a (possible) elementary cycle in the current graph, as well as the disjoint
   * paths that result when removing the cycle. This results in a list of edges that preserves the relationship
   * between every white vertex and at most one other black vertex. Black vertices may end up as members of multiple
   * edges.
   * @return A list of white/black node pairs that should have an edge between them.Every white vertex has at most
   *         one edge, guaranteeing no multi-edges in the graph.
   */
  def computeIncidentEdges: Vector[(BWNode[VType], BWNode[VType])] = {
    val elementaryCycle: Vector[BWNode[VType]] = elemCycleFinder.findCycle(graph).getOrElse(Vector()).toVector
    val disjointPaths: Vector[DisjointPath[BWNode[VType]]] = disjointPathParsing.computePaths(elementaryCycle, graph)

    val elemPairs: Vector[(BWNode[VType], BWNode[VType])] = elementaryCycle.grouped(2).toVector.dropRight(1).map(list => (list(0), list(1)))
    elemPairs ++ (for (path <- disjointPaths) yield path.alternateEdges).flatten
  }

}
