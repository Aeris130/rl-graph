package net.cyndeline.rlgraph.canonicalOrder.planar4ConnectedTriangular

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.canonicalOrder.planar4ConnectedTriangular.help.VertexLabels
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable

/**
 * Computes the canonical order for a planar, 4-connected triangular graph. The ordering is built from top to bottom,
 * meaning that given a graph with n vertices, v(n) is computed first, then v(n - 1) and so on all the way down to 1.
 *
 * Ordering definition:
 *
 *  1. The subgraph G(k-1) of G induced by v1, v2, ... , v(k-1) is biconnected and the boundary of its exterior
 *  face is a cycle C(k-1) containing the edge (u, v).
 *  1. vk is in the exterior face of G(k-1), and its neighbours in G(k-1) form a subinterval of the path
 *  C(k-1) - {(u,v)} consisting of at least two vertices. If k <= n - 2, vk has at least two neighbours in G - G(k-1).
 *
 * Algorithm description:
 *
 *  1. Select a vertex vk that has been visited twice from a parent and doesn't have any adjacent edges that form a
 *  chord* with another vertex.
 *  1. Visit every neighbor v(l), i <= l <= j in that order, of vk and increase visit(l) by 1.
 *   1. If v(k) has more than two neighbors: For every neighbor visited this way, check if the graph contains edges
 *   to a vertex z with visit(z) > 0. If so, (i, z) form a chord. Increase chord(i) and chord(z) by 1.
 *   1. If v(k) has only two neighbors, they constitute a chord computed during a previous processing. Decrease
 *   chord(i) and chord(j) by 1.
 *
 * To start off the algorithm, v(n) is given the order n, and v(n-1) is given (n-1). Every neighbor of v(n) is then
 * visited, same with v(n-1).
 *
 * * A chord is an edge in a cycle that connects two vertices that are in the cycle, but aren't adjacent to each other
 * in the cycle itself. In this case the cycle is C(k).
 *
 * @param v1 One of the vertices in the start edge. Receives canonical order 1.
 * @param v2 The vertex opposite to v1 in the start edge. Receives canonical order 2.
 * @param vnM1 v(n - 1).
 * @param vn Forms a triangular face on the graph together with v1 and v2. Given a graph with n vertices, this vertex
 *           will receive the order n.
 */
class CanonicalOrder[V](v1: V, v2: V, vnM1: V, vn: V, emb: Embedding[V]) {
  require(!emb.iterator.exists(kv => kv._2.size < 4), "The submitted embedding was not 4-connected, vertex " + emb.iterator.find(_._2.size < 4).get + " has degree < 4")

  /* Every vertex mapped to its canonical ordering value. */
  val ordering: Map[V, Int] = computeOrdering

  def vertexOrder(v: V): Int = ordering(v)

  private def computeOrdering: Map[V, Int] = {
    var embedding = emb
    val labels = new VertexLabels(embedding.embeddedVertices)
    val result = new mutable.HashMap[V, Int]()
    result += vn -> embedding.embeddedVertices.size
    result += vnM1 -> (result(vn) - 1)

    /* Start by manually processing v(n), causing at least one other vertex to receive 2 visits after v(n-1)
     * has been processed.
     */
    processVertex(vn, labels, embedding)
    embedding = embedding.deleteVertex(vn)

    var verticesLeft = result(vnM1) - 1
    var vk = vnM1
    while (verticesLeft > 2) {
      processVertex(vk, labels, embedding)
      labels.deRegisterVertex(vk)
      embedding = embedding.deleteVertex(vk)
      val next = findNextVertex(labels)
      result += next -> verticesLeft
      vk = next
      verticesLeft -= 1
    }

    result += v1 -> 1
    result += v2 -> 2

    result.toMap
  }

  private def processVertex(v: V, labels: VertexLabels[V], e: Embedding[V]) {
    val ns = neighbors(v, e)
    visitVertices(ns, labels)
    markChords(v, ns, labels, e)
  }

  private def visitVertices(vs: Vector[V], labels: VertexLabels[V]) {
    for (v <- vs)
      labels.visit(v)
  }

  private def findNextVertex(labels: VertexLabels[V]): V = {
    labels.vertices.find(v => v != v1 && v != v2 && labels.visits(v) >= 2 && labels.chords(v) == 0).getOrElse {
      throw new Error("No additional canonical candidates found.")
    }
  }

  private def markChords(parent: V, vs: Vector[V], labels: VertexLabels[V], e: Embedding[V]) {

    /* If a marked vertex v(k) only has two neighbors on C(k-1), then their common edge was a chord in C(k).
     * Since they're now a part of C(k-1), both vertices has one less chord associated with them.
     */
    if (vs.size == 2) {
      labels.removeChord(vs.head)
      labels.removeChord(vs(1))
    } else {
      val outerfaceNeighbors = outerFaceNeighbors(vs)
      val processedVertices = new mutable.HashSet[UnorderedPair[V]]()

      /* If more than two neighbors of v(k) exists, process them in order left to right (according to the embedding).
       * For every neighbor c(L), i < L < j (i being the first neighbor, j the last), check if it has any neighbors v(z)
       * in the graph with visited > 0, and v(z) not being equal to the neighbors c(L+1) and c(L-1). If so, the edge
       * from c(L) to v(z) is a chord on the current outer face.
       */
      for (v <- vs.drop(1).dropRight(1)) {
        val adjacentOnOuterFace = outerfaceNeighbors(v)
        val visitedNeighbors = e.embeddingFor(v)
          .toVector
          .filter(entry => entry.adjacentVertex != parent && entry.adjacentVertex != adjacentOnOuterFace._1 && entry.adjacentVertex != adjacentOnOuterFace._2 && labels.isVisited(entry.adjacentVertex))
          .map(_.adjacentVertex)

        for (chordNeighbor <- visitedNeighbors if !processedVertices.contains(UnorderedPair(v, chordNeighbor))) {
          processedVertices += UnorderedPair(v, chordNeighbor)
          labels.addChord(v)
          labels.addChord(chordNeighbor)
        }
      }
    }
  }

  /**
   * Retrieves the neighbors of a vertex v in the graph in the order they appear in the embedding.
   * This order is based on the original embedding, such that the first neighbor in the list will be the first neighbor
   * in the original embedding that appears after a vertex that has already been removed (and thus assigned its index).
   */
  private def neighbors(v: V, e: Embedding[V]): Vector[V] = {
    val trimmedAdjacency = e.embeddingFor(v)
    val startNeighbor = if (v == vn) vnM1
      else emb.embeddingFor(v).iterator
            .find(e => !trimmedAdjacency.containsEntryFor(e.adjacentVertex) && trimmedAdjacency.containsEntryFor(e.next.adjacentVertex))
            .get.next.adjacentVertex

    GraphCommons.placeElementFirst(startNeighbor, trimmedAdjacency.toVector.map(_.adjacentVertex))
  }

  /**
   * Returns each vertex in a list > 2 mapped against the vertices that comes before and after it in the list. The first
   * and last element is not mapped.
   */
  private def outerFaceNeighbors(neighbors: Vector[V]): Map[V, (V, V)] = {

    /*
     * Example: 1,2,3,4,5,6,7
     * Group1 = (1,2,3)(4,5,6)
     * Group2 = (2,3,4)(5,6,7)
     * Group3 = (3,4,5)
     */

    val group1 = neighbors.grouped(3).filter(_.size == 3)
    val group2 = neighbors.drop(1).grouped(3).filter(_.size == 3)
    val group3 = neighbors.drop(2).grouped(3).filter(_.size == 3)

    (group1 ++ group2 ++ group3).map(l => (l(1), (l(0), l(2)))).toMap
  }

}
