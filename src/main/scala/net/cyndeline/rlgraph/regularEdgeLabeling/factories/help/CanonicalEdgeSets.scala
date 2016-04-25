package net.cyndeline.rlgraph.regularEdgeLabeling.factories.help

import net.cyndeline.rlgraph.canonicalOrder.planar4ConnectedTriangular.FCanonicalOrder
import net.cyndeline.rlgraph.embedding.{AdjacencyEntry, Embedding}
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Takes an undirected embedding and sorts it into two sets of directed edges, T1 and T2, such that for each interior
 * vertex v the edges incident to v appear in counterclockwise order around v as follows: A set of edges in T1 leaving
 * v, a set of edges in T2 entering v, a set of edges in T1 entering v and a set of edges in T2 leaving v.
 *
 * @param embedding An embedded PTP graph. A PTP graph is defined as a graph where every interior face is a triangle,
*                   the outer face is a quadrangle, and every internal vertex has degree >= 4. This embedding first
*                   needs to (possibly) be reversed before being usable.
 * @param vS The southern vertex on the outer face.
 * @param vN The Northern vertex on the outer face.
 * @param vW The western vertex on the outer face.
 * @param vE The eastern vertex on the outer face.
 */
class CanonicalEdgeSets[V : TypeTag : ClassTag](embedding: Embedding[V], vS: V, vN: V, vW: V, vE: V) {
  private val outerEdgeAppender = new OuterEdgeAppender[V](vN, vS, vW, vE)
  private val faceComp = new FaceComputation[V]()
  private val emb = reverseEmbedding(embedding)
  private val t1t2 = computeEdgeSets // Fills temp sets

  val T1: Vector[(V, V)] = t1t2._1
  val T2: Vector[(V, V)] = t1t2._2

  /** An embedding containing only the edges in T1, in the same adjacency order that the original
    * graph stored them in.
    */
  val embeddingT1: Embedding[V] = outerEdgeAppender.addExtraG1Edge(outerEdgeAppender.trimEmbedding(T1, emb))

  /** An embedding containing only the edges in T2, in the same adjacency order that the original
    * graph stored them in.
    */
  val embeddingT2: Embedding[V] = outerEdgeAppender.addExtraG2Edge(outerEdgeAppender.trimEmbedding(T2, emb))

  private def computeEdgeSets: (Vector[(V, V)], Vector[(V, V)]) = {
    val tempT1 = new ListBuffer[(V, V)]()
    val tempT2 = new ListBuffer[(V, V)]()
    val canonicalOrder = canonicalOrdering
    val highestOrderIndex = canonicalOrder.size

    /* Some edges will be found both as base-edges and left/right edges, as well as connected to the external
     * vertices. This set keeps edges from being added twice.
     */
    val addedEdges = new mutable.HashSet[(V, V)]()

    /* Every interior edge connected to vN are in T1, entering vN.
     * Every interior edge connected to vW are in T2, leaving vW.
     * Every interior edge connected to vS are in T1, leaving vS.
     * Every interior edge connected to vE are in T2, entering vE.
     */
    tempT1 ++= vNVSInT1
    tempT2 ++= vWVEInT2
    addedEdges ++= tempT1
    addedEdges ++= tempT2

    // Skip the two lowest (1, 2) and the two highest.
    for (entry <- canonicalOrder if entry._2 > 2 && entry._2 < highestOrderIndex - 1) {
      val baseEdge = findbaseEdge(entry._1, canonicalOrder, highestOrderIndex)

      if (!addedEdges.contains(baseEdge.tuple)) {
        if (baseEdge.toT1)
          tempT1 += baseEdge.tuple
        else if (baseEdge.toT2)
          tempT2 += baseEdge.tuple
        else
          tempT1 += baseEdge.tuple // arbitrary, so T1 gets all the edges

        addedEdges += baseEdge.tuple
      }

      /* v(k1) to v(kn). Graph is four-connected, so there will always be 2 or more */
      val higherNeighbors = higherNumberedNeighbors(entry._1, canonicalOrder, highestOrderIndex)

      /* These becomes opposite since our embeddings goes clockwise. */
      val rightEdge = (entry._1, higherNeighbors.head)
      val leftEdge = (entry._1, higherNeighbors.last)

      if (!addedEdges.contains(leftEdge)) {
        tempT1 += leftEdge
        addedEdges += leftEdge
      }

      if (!addedEdges.contains(rightEdge)) {
        tempT2 += rightEdge
        addedEdges += rightEdge
      }

    }

    (tempT1.toVector, tempT2.toVector)
  }

  /**
   * Finds the neighbors of a vertex v with a higher canonical order index than v, and returns in the order they appear
   * around v. The order begins at the first vertex with higher index that is encountered when traversing the embedding
   * clockwise.
   *
   * Vertices with index 1 and 2 (i.e the starting edge of the canonical order) does not qualify for higher ordered
   * neighbors, as the edges on the outer face (between the four vertices in the quadrangle) won't be added to the
   * label sets T1 and T2.
   */
  private def higherNumberedNeighbors(v: V, order: Map[V, Int], highestIndex: Int): ListBuffer[V] = {
    val vOrderIndex: Int = order(v)
    require(vOrderIndex > 2, "No edges for vertex indices < 3 should be computed as leftedges or rightedges.")
    require(vOrderIndex < highestIndex - 1, "The two vertices on the outer face with the highest indices only has each other as possible high-order neighbors, and should not be added to T1/T2.")

    val startEntry: AdjacencyEntry[V] = emb.embeddingFor(v)
      .toVector
      .find(entry => order(entry.adjacentVertex) < vOrderIndex && order(entry.next.adjacentVertex) > vOrderIndex)
      .get.next

    ajdacencySubset(startEntry, (a: Int) => a > vOrderIndex, order)
  }

  /**
   * Finds the neighbor v(n) of vertex v(k), for which index n < k is maximal.
   *
   * Every neighbor v(n) with a lower order index is ordered clockwise around v(k) such that i <= n <= j.
   * Depending on which of these vertices the base edge is outgoing from, it gets added to different sets.
   * If n = i then the edge gets added to T2, = j gets it added to T1. Otherwise it's arbitrary.
   */
  private def findbaseEdge(v: V, order: Map[V, Int], highestIndex: Int): BaseEdgeEntry = {
    val vOrderIndex: Int = order(v)
    require(vOrderIndex > 2, "No edges for vertex indices < 3 should be added to T1/T2 as base edges.")
    require(vOrderIndex < highestIndex - 1, "The two vertices on the outer face with the highest indices always has the initial edge as their lowest neighbor. Those edges should not be added to T1/T2.")

    val startEntry: AdjacencyEntry[V] = emb.embeddingFor(v)
      .toVector
      .find(entry => order(entry.adjacentVertex) > vOrderIndex && order(entry.next.adjacentVertex) < vOrderIndex)
      .get.next

    // Vertices ci (first) -> cj (last)
    val neighbors = ajdacencySubset(startEntry, (a: Int) => a < vOrderIndex, order)
    val lowestVertex = neighbors
      .map(v => v -> order(v))
      .reduceLeft((a, b) => if (a._2 < b._2) a else b)
      ._1 // Only need the vertex

    /* Definition of c(i) and c(j) can be found in section 6 of Algorithms for drawing planar graphs (figure
     * with leftupper and rightupper). c(j) is the right-vertex, the first incoming vertex when traversing an
     * adjacency counter clockwise.
     *
     * But since scalarlib uses clockwise traversing, it'll be the opposite instead.
     */
    val setAddition = if (lowestVertex == neighbors.head) 2 else if (lowestVertex == neighbors.last) 1 else 0
    new BaseEdgeEntry(lowestVertex, v, setAddition)
  }

  private def ajdacencySubset(start: AdjacencyEntry[V],
                              isValid: Int => Boolean,
                              order: Map[V, Int]): ListBuffer[V] = {
    var current = start
    val result = new ListBuffer[V]()
    while (isValid(order(current.adjacentVertex))) {
      result += current.adjacentVertex
      current = current.next
    }

    result
  }

  private def canonicalOrdering: Map[V, Int] = {
    val newEmbedding = emb
    val faces = new FaceComputation[V]().computeFaces(newEmbedding)
    val outerFaceCandidates = faces.filter(_.vertexSize == 4)
    require(outerFaceCandidates.size == 1, "A regular edge embedding can only be constructed if exactly one face of size 4 is present, currently: " + outerFaceCandidates)
    val outerFace: Face[V] = outerFaceCandidates.head

    val v1 = vW
    val v2 = vS
    val vnM1 = vE
    val vn = vN

    // Any two adjacent vertices on the outer face will do as a start edge. Add +1 to the final ordering, to make it
    // go from 1 to n rather than 0 to n - 1.
    new FCanonicalOrder(v1, v2, vnM1, vn).order(makeGraph4Connected(newEmbedding, outerFace)).zipWithIndex.map(entry => entry._1 -> (entry._2 + 1)).toMap
  }

  /**
   * Copies the embedding and adds a new edge to the outer face, between two non-adjacent vertices.
   * Since the outer face augmentation specifies that vE is to get only three edges if the outer face contains
   * only 3 vertices, vE must receive the edge in order to guarantee 4-connectivity.
   */
  private def makeGraph4Connected(emb: Embedding[V], outerFace: Face[V]): Embedding[V] = {
    GraphCommons.embedEdgeInFace(emb, outerFace, vS, vN)
  }

  private def vNVSInT1: Vector[(V, V)] = {
    val outer = Set(vN, vS, vW, vE)
    val vNInterior = emb.embeddingFor(vN).iterator.toVector.map(_.adjacentVertex).filter(!outer.contains(_))
    val vSInterior = emb.embeddingFor(vS).iterator.toVector.map(_.adjacentVertex).filter(!outer.contains(_))

    (for (v <- vNInterior) yield (v, vN)) ++ (for (v <- vSInterior) yield (vS, v))
  }

  private def vWVEInT2: Vector[(V, V)] = {
    val outer = Set(vN, vS, vW, vE)
    val vWInterior = emb.embeddingFor(vW).iterator.toVector.map(_.adjacentVertex).filter(!outer.contains(_))
    val vEInterior = emb.embeddingFor(vE).iterator.toVector.map(_.adjacentVertex).filter(!outer.contains(_))

    (for (v <- vEInterior) yield (v, vE)) ++ (for (v <- vWInterior) yield (vW, v))
  }

  /**
   * The outer vertices must come in the order N, E, S, W in order for the REL to compute T1/T2 properly.
   * If this is not the case, reverse the embedding.
   */
  private def reverseEmbedding(embedding: Embedding[V]): Embedding[V] = {
    val outerFace = faceComp.computeFaces(embedding).find(_.vertexSize > 3).get
    val nFirst = GraphCommons.placeElementFirst(vN, outerFace.vertices)
    if (nFirst != Vector(vN, vE, vS, vW)) {
      embedding.reverse
    } else {
      embedding
    }
  }

  /**
   * @param addToSet 1 = add to T1, 2 = add to T2, 0 = Arbitrary
   */
  private class BaseEdgeEntry(val from: V, val to: V, addToSet: Int) {
    require(addToSet > -1 && addToSet < 3)
    def tuple = (from, to)
    def toT1 = addToSet == 1
    def toT2 = addToSet == 2

    override def toString: String = {
      "From: " + from + " to " + to + ", add to set: " + (if (addToSet == 1) "1" else if (addToSet == 2) "2" else "Any")
    }
  }
}
