package net.cyndeline.rlgraph.canonicalOrder.planarBiconnected

import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}

/**
  * Represents the outer contour of G(k), C(k). The first element in the list is the initial vertex v1, and the last
  * element is the initial vertex v2, forming the initial edge v1~v2 of the canonical ordering.
  *
  * If a vertex vk added only connects to C(k-1) through a single neighbor n on C(k-1), it will be added to the contour
  * as if the two edges (n-1)~vk, n~vk exists if the vertex has left support. If it has right support, (n+1) is used
  * instead. Thus a contour 1-3-4-5-2 will turn into 1-3-6-4-5-2 if a vertex 6 with left support is connected to
  * vertex 4.
  *
  * @param originalEmbedding The initial embedding that this contour was created from. Should be used when looking for
  *                          left and right neighbors.
  * @param modifiedEmbedding An embedding that has been modified with edges that are added to provide legal left/right
  *                          support.
  */
class Contour[V] private (val vertices: Vector[V], onCk: Set[V], inGk: Set[V], originalEmbedding: Embedding[V], modifiedEmbedding: Embedding[V]) {
  require(vertices.nonEmpty, "No initial vertices in contour.")

  def this(v1: V, v2: V, embedding: Embedding[V]) = this(Vector(v1, v2), Set(v1, v2), Set(v1, v2), embedding, embedding)

  /** The starting vertex of the first edge. */
  val v1 = vertices.head

  /** The ending vertex of the first edge. */
  val v2 = vertices.last

  /**
    * Adds a vertex to this contour that is either a cutpoint, or connected to C(k - 1) using an interval of edges.
    *
    * @param vk Neighbor of one or more vertices on this contour.
    */
  def addVertex(vk: V): CKMod[V] = {
    require(!onContour(vk), "Attempted to add vertex " + vk + " twice to C(k-1).")
    require(!inGk.contains(vk), "Attempted to add vertex " + vk + " twice to G(k-1).")
    val lar = findLeftAndRightNeighbor(vk)
    val leftmostNeighbor = lar._1
    val rightmostNeighbor = lar._2

    if (leftmostNeighbor == rightmostNeighbor) {
      addCutpoint(vk, leftmostNeighbor)
    } else {
      // This will only occur when there are no cut-vertices in C(k-1)
      val leftIndex = vertices.indexOf(leftmostNeighbor)
      val rightIndex = vertices.indexOf(rightmostNeighbor)

      // +/- 1 to keep the left/right vertex in the left/right split
      val leftAndRemaining = vertices.splitAt(leftIndex + 1)
      val removedAndRight = leftAndRemaining._2.splitAt(rightIndex - leftIndex - 1)
      val right = removedAndRight._2
      val removed = removedAndRight._1

      val newC = new Contour(leftAndRemaining._1 ++ (vk +: right), (onCk + vk) -- removed, (inGk ++ removed) + vk, originalEmbedding, modifiedEmbedding)
      CKMod(newC, None, removed, leftmostNeighbor, rightmostNeighbor)
    }
  }

  /**
    * @param v A vertex to check the existence for.
    * @return True if vertex v is on this contour, otherwise false.
    */
  def onContour(v: V): Boolean = onCk.contains(v)

  /**
    * @param v A vertex to check cover for.
    * @return True if the vertex v is not on C(k), but has been at some point.
    */
  def contains(v: V): Boolean = inGk.contains(v) || onContour(v)

  /**
    * Given a vertex v(k) that has neighbors on C(k-1) (a list starting from the left at v1 and going to the right to
    * v(p)), this method finds the leftmost and rightmost neighbor. These may be one and the same if only one such
    * neighbor exists.
    */
  def findLeftAndRightNeighbor(v: V): (V, V) = {
    val vNeighbors = originalEmbedding.neighborsOf(v).toSet
    val neighbors = this.vertices.filter(vNeighbors.contains)
    require(neighbors.nonEmpty, "Attempted to find neighbors of vertex " + v + " on C(k-1): " + this.vertices.mkString(", ") + ", but no neighbor existed.")
    val leftNeighbor = neighbors.head
    val rightNeighbor = neighbors.last

    (leftNeighbor, rightNeighbor)
  }

  /**
    * @param v A vertex to check support for that only has a single neighbor on C(k-1).
    * @return true if a vertex v(k), connected to a neighbor n(i) on C(k-1), immediately follows n(i-1) when going
    *         clockwise around n. Or if v's only neighbor is v2.
    */
  def hasLeftSupport(v: V): Boolean = leftSupport(v).isDefined

  /**
    * @param v A vertex to check support for that only has a single neighbor on C(k-1).
    * @return true if a vertex v(k), connected to a neighbor n(i) on C(k-1), immediately follows n(i+1) when going
    *         counter clockwise around n. Or if v's only neighbor is v1.
    */
  def hasRightSupport(v: V): Boolean = rightSupport(v).isDefined

  private def findSingleNeighbor(v: V): V = {
    val neighborsOnCk = originalEmbedding.neighborsOf(v).intersect(vertices)
    require(neighborsOnCk.nonEmpty, "Attempted to find neighbor of a vertex not connected to C(k-1).")
    require(neighborsOnCk.length == 1, "Attempted to find a single neighbor of a vertex on C(k-1), but multiple neighbors exists.")
    neighborsOnCk.head
  }

  /**
    * Adds a vertex v(k) to C(k-1) that only has a single neighbor on it. vk is added before its only cutpoint-neighbor
    * if it has left support, otherwise after it.
    */
  private def addCutpoint(vk: V, singleNeighbor: V): CKMod[V] = {
    var support: Option[LegalSupport] = None
    var lv: Option[V] = None
    var rv: Option[V] = None

    val newVertexC = if (hasLeftSupport(vk)) {
      support = Some(Left)
      lv = leftSupport(vk)
      rv = Some(singleNeighbor)
      vertices.patch(vertices.indexOf(singleNeighbor), Seq(vk, singleNeighbor), 1)
    } else if (hasRightSupport(vk)) {
      support = Some(Right)
      lv = Some(singleNeighbor)
      rv = rightSupport(vk)
      vertices.patch(vertices.indexOf(singleNeighbor), Seq(singleNeighbor, vk), 1)
    } else {
      throw new Error("Attempted to add a single vertex to a cutpoint on the contour C(k-1), but the vertex lacked both left and right support.")
    }

    // If the vertex was added using support, the internal embedding must be updated to reflect this

    val newC = new Contour(newVertexC, onCk + vk, inGk, originalEmbedding, embedSupportEdge(vk, singleNeighbor, support.get))
    CKMod[V](newC, support, Vector(), lv.get, rv.get)
  }

  def leftSupport(v: V): Option[V] = {
    val n = findSingleNeighbor(v)
    if (n == v1) {
      None

    } else {
      val nIndex = vertices.indexOf(n)
      val previousOnCk = vertices(nIndex - 1)

      if (modifiedEmbedding.embeddingFor(n).entryFor(v).previous.adjacentVertex == previousOnCk) {
        Some(previousOnCk)
      } else {
        None
      }
    }
  }

  def rightSupport(v: V): Option[V] = {
    val n = findSingleNeighbor(v)
    if (n == v2) {
      None

    } else {
      val nIndex = vertices.indexOf(n)
      val nextOnCk = vertices(nIndex + 1)

      if (modifiedEmbedding.embeddingFor(n).entryFor(v).next.adjacentVertex == nextOnCk) {
        Some(nextOnCk)
      } else {
        None
      }
    }
  }

  private def embedSupportEdge(vk: V, neighbor: V, support: LegalSupport): Embedding[V] = support match {
    case Left =>
      val newCkConnection = modifiedEmbedding.embeddingFor(neighbor).entryFor(vk).previous.adjacentVertex
      val connectionInsertPoint = modifiedEmbedding.embeddingFor(newCkConnection).entryFor(neighbor).previous.adjacentVertex
      modifiedEmbedding.embedEdge(Vertex(newCkConnection) withInsertPosition neighbor inVertex vk withInsertPosition connectionInsertPoint)
    case Right =>
      val newCkConnection = modifiedEmbedding.embeddingFor(neighbor).entryFor(vk).next.adjacentVertex
      val connectionInsertPoint = modifiedEmbedding.embeddingFor(vk).entryFor(neighbor).previous.adjacentVertex
      modifiedEmbedding.embedEdge(Vertex(newCkConnection) withInsertPosition connectionInsertPoint inVertex vk withInsertPosition neighbor)
  }

}
