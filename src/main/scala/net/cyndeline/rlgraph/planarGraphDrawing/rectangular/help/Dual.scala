package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.help

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation, FaceMembershipManager}
import net.cyndeline.rlgraph.pathfinding.LongestDAGPath
import net.cyndeline.rlgraph.util.GraphCommons

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * Used to parse data from the dual graph of the edge set T1 and T2, along with external edges.
 *
 * @param tEdgeGraph graph constructed from T1 or T2.
 * @param dualStructure The dual of tEdgeGraph.
 * @param source Source vertex in 'dualStructure.
 * @param sink Sink vertex in 'dualStructure.
 * @param embedding Embedding of tEdgeGraph with adjacencies in the same order they appear in in the original graph.
 */
class Dual[V : TypeTag : ClassTag](tEdgeGraph: Graph[V, DiEdge],
                                  dualStructure: Graph[Face[V], DiEdge],
                                  source: Face[V],
                                  val sink: Face[V],
                                  embedding: Embedding[V],
                                  outerVertices: Set[V]) {
  private val allFaces = new FaceComputation[V]().computeFaces(embedding)
  private val edgemanager = new FaceMembershipManager[V](allFaces)
  private val leftFaceMap: Map[V, Face[V]] = computeLeftFaces
  private val rightFaceMap: Map[V, Face[V]] = computeRightFaces
  private val longestPaths = LongestDAGPath.withoutWeights(source, dualStructure)

  private val distanceMap: Map[Face[V], Int] = {
    val result = for {
      f <- dualStructure.nodes.toVector
      inner: Face[V] = f
      cost = longestPaths.distanceTo(inner)
    } yield inner -> cost

    result.toMap
  }

  def left(v: V): Face[V] = leftFaceMap(v)

  def right(v: V): Face[V] = rightFaceMap(v)

  def maxDistance(f: Face[V]): Int = distanceMap(f)

  private def computeLeftFaces: Map[V, Face[V]] = {
    val result = for {
      v <- embedding.embeddedVertices
      if !outerVertices.contains(v)
      outgoingEdge = firstOutgoingEdge(v)
      leftFace = edgemanager.leftFace(outgoingEdge)
    } yield v -> leftFace

    result.toMap
  }

  private def computeRightFaces: Map[V, Face[V]] = {
    val result = for {
      v <- embedding.embeddedVertices
      if !outerVertices.contains(v)
      incomingEdge = firstIncomingEdge(v)
      rightFace = edgemanager.rightFace(incomingEdge)
    } yield v -> rightFace

    result.toMap
  }

  /**
   * Traverses the adjacency list of a vertex in clockwise order and returns the first outgoing edge that comes after
   * an incoming edge. Since every outgoing and incoming edge lies on the same interval in the embedding, there's only
   * one such edge.
   */
  private def firstOutgoingEdge(v: V): (V, V) = {
    val outer = tEdgeGraph.get(v)
    val incoming = allIncoming(tEdgeGraph)(outer)
    val outgoing = allOutgoing(tEdgeGraph)(outer)
    val adjacencies = embedding.embeddingFor(v)

    val first = adjacencies.iterator.find(entry => incoming.contains(entry.adjacentVertex) && outgoing.contains(entry.next.adjacentVertex))
      .getOrElse(throw new Error("Could not find an incoming edge with an outgoing edge after it around " + v))
      .next
    (v, first.adjacentVertex)
  }

  private def firstIncomingEdge(v: V): (V, V) = {
    val outer = tEdgeGraph.get(v)
    val incoming = allIncoming(tEdgeGraph)(outer)
    val outgoing = allOutgoing(tEdgeGraph)(outer)
    val adjacencies = embedding.embeddingFor(v)

    val first = adjacencies.iterator.find(entry => outgoing.contains(entry.adjacentVertex) && incoming.contains(entry.next.adjacentVertex))
      .getOrElse(throw new Error("Could not find an outgoing edge with an incoming edge after it around " + v))
      .next
    (first.adjacentVertex, v)
  }

  private def allIncoming(g: Graph[V, DiEdge])(v: g.NodeT): Set[V] = v.incoming.toSet.map((edge: g.EdgeT) => GraphCommons.oppositeVertex[V, DiEdge](v, edge.toOuter))
  private def allOutgoing(g: Graph[V, DiEdge])(v: g.NodeT): Set[V] = v.outgoing.toSet.map((edge: g.EdgeT) => GraphCommons.oppositeVertex[V, DiEdge](v, edge.toOuter))
}
