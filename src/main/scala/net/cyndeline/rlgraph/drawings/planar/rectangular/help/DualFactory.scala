package net.cyndeline.rlgraph.drawings.planar.rectangular.help

import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation, FaceMembershipManager}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Computes the dual graphs based on embeddings for the sets T1 and T2 in a REL.
 *
 * @param vWest The vertex on the outer face assigned as West.
 * @param vSouth The vertex on the outer face assigned as South.
 * @param vNorth The vertex on the outer face assigned as North.
 * @param vEast The vertex on the outer face assigned as East.
 */
class DualFactory[V : TypeTag : ClassTag](vWest: V, vSouth: V, vNorth: V, vEast: V) {
  private val faceComp = new FaceComputation[V]()

  /**
   * @param embedding The embedding of G1, stores its adjacencies in the same order they appear in the graph that T1 was
   *                  derived from.
   * @param g1 The graph constructed from the REL set T1. Includes the additional edges between the four outer vertices
   *           (v(S)->v(W)->v(N), v(S)->v(E)->v(N)) as well as an additional edge from v(S) to v(N).
   * @return The dual of G1, where every node represents a face in G1. Two nodes a,b are connected with an edge if
   *         a and b shares an edge in G1. If a is the left face of the shared edge, the edge (a,b) will be directed
   *         from a to b (unless a,b == v(S), v(N), in which case the edge is directed from b to a).
   */
  def constructDualG1(embedding: Embedding[V], g1: Graph[V, DiEdge]): Dual[V] = {
    val faces = faceComp.computeFaces(embedding)
    val edgeMembership = new FaceMembershipManager(faces)
    val source = edgeMembership.rightFace(vSouth, vNorth)
    val sink = edgeMembership.leftFace(vSouth, vNorth)
    new Dual(
      g1,
      constructDual(faces, edgeMembership, g1, vSouth, vNorth),
      source, sink,
      embedding,
      Set(vWest, vNorth, vEast, vSouth)
    )
  }


  /**
   * @param g2 The graph constructed from the REL set T1. Includes the additional edges between the four outer vertices
   *           (v(W)->v(N)->v(E), v(W)->v(S)->v(E)) as well as an additional edge from v(W) to v(E).
   * @param embedding The embedding of G2, stores its adjacencies in the same order they appear in the graph that T2 was
   *                  derived from.
   * @return The dual of G2, where every node represents a face in G2. Two nodes a,b are connected with an edge if
   *         a and b shares an edge in G2. If a is the left face of the shared edge, the edge (a,b) will be directed
   *         from a to b (unless a,b == v(W), v(E), in which case the edge is directed from b to a).
   */
  def constructDualG2(embedding: Embedding[V], g2: Graph[V, DiEdge]): Dual[V] = {
    val faces = faceComp.computeFaces(embedding)
    val edgeMembership = new FaceMembershipManager(faces)
    val source = edgeMembership.rightFace(vWest, vEast)
    val sink = edgeMembership.leftFace(vWest, vEast)
    constructDual(faces, edgeMembership, g2, vWest, vEast)

    new Dual(
      g2,
      constructDual(faces, edgeMembership, g2, vWest, vEast),
      source, sink,
      embedding,
      Set(vWest, vNorth, vEast, vSouth)
    )
  }

  /**
   * @param a south or west, depending on wether G1's or G2's dual is being computed.
   * @param b North or east.
   * @return A directed graph with every edge weighted at -1, in order to compute the longest path through the
   *         graph using shortest-path algorithms.
   */
  private def constructDual(faces: Vector[Face[V]], faceManager: FaceMembershipManager[V], graph: Graph[V, DiEdge], a: V, b: V): Graph[Face[V], DiEdge] = {
    var dual = Graph[Face[V], DiEdge]()

    // Construct left/right face from g1 edges
    for (e <- graph.edges) {
      val outer: (V, V) = (e._1, e._2)
      val leftFace = faceManager.leftFace(outer)
      val rightFace = faceManager.rightFace(outer)

      if (outer._1 == a && outer._2 == b)
        dual += rightFace~>leftFace
      else
        dual += leftFace~>rightFace
    }

    dual
  }
}
