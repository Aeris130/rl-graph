package net.cyndeline.rlgraph.subgraph.isomorphism

import scala.language.{reflectiveCalls, higherKinds}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Maps vertices in an isomorphic relation between two graphs.
 *
 * @tparam N1 Node type of the pattern graph.
 * @tparam E1 Edge type in the pattern graph.
 * @tparam N2 Node type in the graph to match the pattern against.
 * @tparam E2 Edge type in the graph to match the pattern against.
 */
trait IsomorphicMapping[N1, E1[X] <: EdgeLikeIn[X], N2, E2[X] <: EdgeLikeIn[X]] {

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph.
   *
   * @param graph Graph to check if it contains the specified sub graph.
   * @param pattern Sub graph to check isomorphism for.
   * @param random Selects a random topology of the graph matching the sub graph, instead of
   *               the first one to be found.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  def randomIsomorphicMapping(graph: Graph[N1, E1],
                              pattern: Graph[N2, E2],
                              random: Random): Option[IsomorphicMatch[N1, N2]]

  /**
   * Checks if a graph contains a subset of vertices and edges that matches those of a sub graph, and returns
   * the first such topology that is found.
   *
   * @param graph Graph to check if it contains the specified sub graph.
   * @param pattern Sub graph to check isomorphism for.
   * @return a mapping between vertices in the sub graph and the graph it is a sub graph of, if such is the case.
   *         Otherwise None.
   */
  def firstIsomorphicMapping(graph: Graph[N1, E1], pattern: Graph[N2, E2]): Option[IsomorphicMatch[N1, N2]]

  /**
   * Computes all isomorphc mappings between a graph and a pattern.
   *
   * @param graph Graph to check if it contains the specified sub graph.
   * @param pattern Sub graph to check isomorphism for.
   * @return Every mapping between the pattern and the graph. Empty if no isomorphic match exists.
   */
  def allIsomorphicMappings(graph: Graph[N1, E1], pattern: Graph[N2, E2]): Vector[IsomorphicMatch[N1, N2]]

}
