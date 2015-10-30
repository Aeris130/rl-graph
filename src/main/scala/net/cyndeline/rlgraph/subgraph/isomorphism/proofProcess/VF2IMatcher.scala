package net.cyndeline.rlgraph.subgraph.isomorphism.proofProcess

import net.cyndeline.rlgraph.subgraph.isomorphism._

import scala.language.higherKinds
import scala.util.Random
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph
import scalax.collection.{ Graph => CGraph }

/**
 * Wraps the ProofProcess isomorphism matcher in the rlGraph isomorphism interface.
 */
class VF2IMatcher[V1, E1[X] <: EdgeLikeIn[X], V2, E2[X] <: EdgeLikeIn[X]](vMatch: Option[VertexCompare[V1, V2]],
                                                                          eMatch: Option[EdgeCompare[E1, E2]]) extends IsomorphicMapping[V1, E1, V2, E2] {

  /**
   * Constructs a matcher that only looks at the general topology between the graph and the pattern, without taking
   * vertex/edge-specific data into consideration.
   */
  def this() = this(None, None)

  /**
   * Constructs a matcher that only matches topology between a graph and a pattern if their vertices compares using
   * a user-supplied vertex comparison.
   * @param vertexMatch Compares if a vertex from the graph and pattern are equal.
   */
  def this(vertexMatch: VertexCompare[V1, V2]) = this(Some(vertexMatch), None)

  /**
   * Constructs a matcher that only matches topology between a graph and a pattern if their edges compares using
   * a user-supplied edge comparison.
   * @param edgeMatch Compares if an edge from the graph and pattern are equal.
   */
  def this(edgeMatch: EdgeCompare[E1, E2]) = this(None, Some(edgeMatch))

  /**
   * Constructs a matcher that compares both edges and vertices when matching topology between a graph and a pattern.
   *
   * @param vertexMatch Compares if a vertex from the graph and pattern are equal.
   * @param edgeMatch Compares if an edge from the graph and pattern are equal.
   */
  def this(vertexMatch: VertexCompare[V1, V2], edgeMatch: EdgeCompare[E1, E2]) = this(Some(vertexMatch), Some(edgeMatch))

  override def randomIsomorphicMapping(graph: Graph[V1, E1],
                                       pattern: Graph[V2, E2],
                                       random: Random): Option[IsomorphicMatch[V1, V2]] = matchPattern(Some(random), graph, pattern)

  override def firstIsomorphicMapping(graph: Graph[V1, E1],
                                      pattern: Graph[V2, E2]): Option[IsomorphicMatch[V1, V2]] = matchPattern(None, graph, pattern)

  override def allIsomorphicMappings(graph: Graph[V1, E1], pattern: Graph[V2, E2]): Vector[IsomorphicMatch[V1, V2]] = {
    val matcher = makeMatcher(graph, pattern)
    val result = matcher.default

    if (result.isIsomorphism) {
      result.isomorphisms.toVector.map(mapping => new IsomorphicMatch[V1, V2](innerToOuterMap(matcher.g1, matcher.g2)(mapping)))
    } else {
      Vector()
    }

  }

  private def matchPattern(random: Option[Random],
                           graph: Graph[V1, E1],
                           pattern: Graph[V2, E2]): Option[IsomorphicMatch[V1, V2]] = {

    val matcher = makeMatcher(graph, pattern)
    val result = matcher.default
    if (result.isIsomorphism) {

      // Check how many results were found and pick a random one
      if (random.isDefined) {
        val numberOfHits = result.isomorphisms.size
        val index = random.get.nextInt(numberOfHits)
        val nodeMap: Map[matcher.Node2, matcher.Node1] = result.isomorphisms(index)
        Some(new IsomorphicMatch[V1, V2](innerToOuterMap(matcher.g1, matcher.g2)(nodeMap)))

      } else {
        val nodeMap: Option[Map[matcher.Node2, matcher.Node1]] = result.isomorphism
        Some(new IsomorphicMatch[V1, V2](innerToOuterMap(matcher.g1, matcher.g2)(nodeMap.get)))
      }
    } else {
      None
    }
  }

  private def makeMatcher(graph: Graph[V1, E1], pattern: Graph[V2, E2]) = {
    val nodeCompairson = vMatch match {
      case Some(compairson) => Some((v1: V1, v2: V2) => compairson.compareNode(v1, v2))
      case None => None
    }
    val edgeCompairson = eMatch match {
      case Some(compairson) => Some((e1: E1[_], e2: E2[_]) => compairson.compareEdge(e1, e2))
      case None => None
    }

    VF2Isomorphism[V1, E1, V2, E2](graph, pattern, nodeCompairson, edgeCompairson)
  }

  private def innerToOuterMap(g1: CGraph[V1, E1], g2: CGraph[V2, E2])(m: Map[g2.NodeT, g1.NodeT]): Map[V2, V1] = m.map(kv => {
    val key: V2 = kv._1
    val value: V1 = kv._2
    key -> value
  })
}
