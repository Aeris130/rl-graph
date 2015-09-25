package net.cyndeline.rlgraph.regularEdgeLabeling

import java.util.UUID

import net.cyndeline.rlgraph.embedding.{EdgeInsert, Embedding, Vertex}

import scala.collection.mutable.ListBuffer

/**
 * Divides the edges of an undirected planar graph into two sets T1 and T2, such that for each interior vertex v the
 * edges incident to v appear in counterclockwise order around v as follows: A set of edges in T1 leaving v, a set of
 * edges in T2 entering v, a set of edges in T1 entering v and a set of edges in T2 leaving v.
 * @tparam V Vertex type of the labeling.
 */
sealed trait EdgeLabeling[V] {

  def edges: Vector[LabelEdge[V]]

  /** @return The PTP embedding that both T1 and T2 were based on. */
  def originalEmbedding: Embedding[V]

  /** @return The vertices and edges belonging to T1 as well as S~W, S~E, N~W, N~E, S~N. */
  def embeddingOfT1: Embedding[V]

  /** @return The vertices and edges belonging to T2 as well as S~W, S~E, N~W, N~E, W~E. */
  def embeddingOfT2: Embedding[V]

  /**
   * Switches membership between T1 and T2 for a single edge.
   * @param edge The edge to flip. Must belong to this labeling.
   * @param reverse True if the order of the vertices in the edge should be reversed, otherwise false.
   * @return A new labeling with the specified edges now belonging to T2 if its previous set was T1, or vice versa.
   */
  def flipEdge(edge: LabelEdge[V], reverse: Boolean): EdgeLabeling[V]

  /**
   * Switches membership between T1 and T2 for multiple edges.
   * @param edges The edges to flip. Must belong to this labeling.
   * @param reverse A vector of equal size as the edge-vector. True if the edge stored at the same index in the
   *                edge-vector should have its vertices switch place, otherwise false.
   * @return A new labeling with the specified edges now belonging to T2 if its previous set was T1, or vice versa.
   */
  def flipEdges(edges: Vector[LabelEdge[V]], reverse: Vector[Boolean]): EdgeLabeling[V]

  /**
   * Switches membership between T1 and T2 for multiple edges.
   * @param edges Indices of edges to flip.
   * @param reverse A vector of equal size as the edge-vector. True if the edge stored at the same index in the
   *                edge-vector should have its vertices switch place, otherwise false.
   * @return A new labeling with the specified edges now belonging to T2 if its previous set was T1, or vice versa.
   */
  def flipIndexEdges(edges: Vector[Int], reverse: Vector[Boolean]): EdgeLabeling[V]

  /** @return Every edge member of T1, forming paths from south to north. */
  def edgesOfT1: Vector[LabelEdge[V]]

  /** @return Every edge member of T2, forming paths from west to east. */
  def edgesOfT2: Vector[LabelEdge[V]]

  /**
   * @param edge Edge to check membership for.
   * @return True if the edge is a member of the set T1, otherwise false.
   */
  def isMemberOfT1(edge: LabelEdge[V]): Boolean

  /**
   * @param edge Edge to check membership for.
   * @return True if the edge is a member of the set T2, otherwise false.
   */
  def isMemberOfT2(edge: LabelEdge[V]): Boolean

  /** The northern vertex on the outer face. */
  val north: V

  /** The western vertex on the outer face. */
  val west: V

  /** The southern vertex on the outer face. */
  val south: V

  /** The eastern vertex on the outer face. */
  val east: V

}

/**
 * Stores the edges of T1 and T2 using indices to update the labelings data structures in constant time. Flipping an
 * edge takes at most O(n) time, where n is the number of edges adjacent to the vertices of the edge.
 *
 * Use factory object to instantiate an initial labeling.
 *
 * @param edges Every edge of the labeling (both T1 and T2).
 * @param TMembership Each index in this vector corresponds to an index in the edge vector. If the value is true, the
 *                    edge belongs to T1. Otherwise T2.
 * @param originalEmbedding The PTP embedding that both T1 and T2 were based on.
 * @param embeddingOfT1 The vertices and edges belonging to T1 as well as S~W, S~E, N~W, N~E, S~N.
 * @param embeddingOfT2 The vertices and edges belonging to T2 as well as S~W, S~E, N~W, N~E, W~E.
 * @param id A unique id that confirms that two labelings stem from the same object instance, despite having flipped
 *           one or more edges. This id is not used when comparing equality.
 * @param size Number of edges in the labeling. Only used to validate indices of input parameters.
 */
class RegularEdgeLabeling[V] private (val edges: Vector[LabelEdge[V]],
                                     TMembership: Vector[Boolean],
                                     val originalEmbedding: Embedding[V],
                                     val embeddingOfT1: Embedding[V],
                                     val embeddingOfT2: Embedding[V],
                                     val id: UUID,
                                     size: Int,
                                     val north: V, val west: V, val south: V, val east: V) extends EdgeLabeling[V] {

  override def flipEdge(edge: LabelEdge[V], reverse: Boolean): EdgeLabeling[V] = {
    flipEdges(Vector(edge), Vector(reverse))
  }

  override def flipEdges(edges: Vector[LabelEdge[V]], reverse: Vector[Boolean]): EdgeLabeling[V] = {
    var t1Emb = embeddingOfT1
    var t2Emb = embeddingOfT2
    var membership = TMembership
    var edgeList = this.edges
    var i = 0
    val allEdges = edges.iterator

    while (allEdges.hasNext) {
      val e = allEdges.next()
      validateEdge(e)
      if (isMemberOfT1(e)) {
        t1Emb = removeFromEmbedding(e, t1Emb)
        t2Emb = addToEmbedding(e, t2Emb)
      } else {
        t2Emb = removeFromEmbedding(e, t2Emb)
        t1Emb = addToEmbedding(e, t1Emb)
      }

      membership = membership.updated(e.index, !TMembership(e.index))
      val swapVertices = if (reverse(i)) e.swap else e
      edgeList = edgeList.updated(e.index, swapVertices)
      i += 1
    }

    new RegularEdgeLabeling(edgeList, membership, originalEmbedding, t1Emb, t2Emb, id, size, north, west, south, east)
  }

  def flipIndexEdges(edges: Vector[Int], reverse: Vector[Boolean]): EdgeLabeling[V] = {
    flipEdges(edges.map(e => this.edges(e)), reverse)
  }

  override val edgesOfT1: Vector[LabelEdge[V]] = edges.filter(isMemberOfT1)
  override val edgesOfT2: Vector[LabelEdge[V]] = edges.filter(isMemberOfT2)

  override def isMemberOfT1(edge: LabelEdge[V]): Boolean = {
    validateEdge(edge: LabelEdge[V])
    TMembership(edge.index)
  }
  override def isMemberOfT2(edge: LabelEdge[V]): Boolean = {
    validateEdge(edge)
    !TMembership(edge.index)
  }

  override def equals(other: Any): Boolean = other match {
    case re: RegularEdgeLabeling[V] => edges == re.edges && edgesOfT1 == re.edgesOfT1 && edgesOfT2 == re.edgesOfT2
    case _ => false
  }

  // Don't use the id here, as it might break deterministic properties
  override def hashCode: Int = edges.## ^ TMembership.## ^ originalEmbedding.## ^ embeddingOfT1.## ^ embeddingOfT1.##

  private def removeFromEmbedding(edge: LabelEdge[V], embedding: Embedding[V]): Embedding[V] = {
    require(embedding.embeddingFor(edge.from).containsEntryFor(edge.to))
    embedding.deleteEdge(edge.from, edge.to)
  }

  private def addToEmbedding(edge: LabelEdge[V], embedding: Embedding[V]): Embedding[V] = {
    require(!embedding.embeddingFor(edge.from).containsEntryFor(edge.to))
    val previousFrom = previousEntry(edge.from, edge.to, embedding)
    val previousTo = previousEntry(edge.to, edge.from, embedding)

    // If one of the two vertices temporarily doesn't have edges connected to it, no insert positions may be found.
    val step0 = Vertex(edge.from)
    val step1 = if (previousTo.isDefined) step0 withInsertPosition previousTo.get inVertex edge.to else step0 withDefaultPositionInVertex edge.to
    val step2: EdgeInsert[V] = if (previousFrom.isDefined) step1.withInsertPosition(previousFrom.get) else step1.withDefaultInsertPosition
    embedding.embedEdge(step2)
  }

  private def previousEntry(from: V, to: V, embedding: Embedding[V]): Option[V] = {
    val fromEntry = embedding.embeddingFor(from)
    var currentEntry = originalEmbedding.embeddingFor(from).entryFor(to).previous

    while (currentEntry.adjacentVertex != to) {
      if (fromEntry.containsEntryFor(currentEntry.adjacentVertex))
        return Some(currentEntry.adjacentVertex)
      else
        currentEntry = currentEntry.previous
    }

    None
  }

  private def validateEdge(edge: LabelEdge[V]): Unit = {
    val str = "The edge " + edge + " did not belong to the REL."
    require(edge.index >= 0 && edge.index < size, str)
    val currentEdge = edges(edge.index)
    require(currentEdge.to == edge.to && currentEdge.from == edge.from, str)
  }

}

/**
 * Factory object used to creates new regular edge labels.
 */
object RegularEdgeLabeling {

  /**
   * @param insertIntoT1 All directed edges to insert into T1, starting at the first value and ending at the second.
   * @param insertIntoT2 All directed edges to insert into T2, starting at the first value and ending at the second.
   * @param originalEmbedding The embedding that both T1 and T2 were based on.
   * @param embeddingOfT1 The vertices and edges belonging to T1.
   * @param embeddingOfT2 The vertices and edges belonging to T2.
   * @return An edge labeling with the sets T1 and T2.
   */
  def apply[V](insertIntoT1: Vector[(V, V)],
               insertIntoT2: Vector[(V, V)],
               originalEmbedding: Embedding[V],
               embeddingOfT1: Embedding[V],
               embeddingOfT2: Embedding[V],
               north: V, west: V, south: V, east: V): EdgeLabeling[V] = {
    var index = 0
    val memberships = Array.fill(insertIntoT1.size + insertIntoT2.size)(false)
    val edges = new ListBuffer[LabelEdge[V]]()
    for (t1 <- insertIntoT1) {
      val edge = LabelEdge(t1._1, t1._2, index)
      memberships(index) = true
      edges += edge
      index += 1
    }
    for (t2 <- insertIntoT2) {
      val edge = LabelEdge(t2._1, t2._2, index)
      memberships(index) = false
      edges += edge
      index += 1
    }

    val edgeVector = edges.toVector
    new RegularEdgeLabeling(edgeVector, memberships.toVector, originalEmbedding, embeddingOfT1, embeddingOfT2, UUID.randomUUID(), edgeVector.size, north, west, south, east)
  }
}
