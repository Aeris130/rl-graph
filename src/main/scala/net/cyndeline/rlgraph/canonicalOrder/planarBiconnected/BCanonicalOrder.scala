package net.cyndeline.rlgraph.canonicalOrder.planarBiconnected

import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.canonicalOrder.CanonicalOrder
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation, FaceMembershipManager}
import net.cyndeline.rlgraph.util.{GraphCommons, IndexGraph}

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scalax.collection.GraphEdge.UnDiEdge

/**
  * Computes the canonical ordering of vertices in a planar biconnected graph, using the algorithm outlined in
  * "An Incremental Drawing algorithm for Planar Graphs" by David Harel and Meir Sardas.
  *
  * @param startingEdge Selects an edge to serve as the starting edge of the algorithm (the first element will receive
  *                     order 1, while the second receives order 2). This edge is also used to select the outer face, as
  *                     the left face will be the first inner face, and the right face will be the external face.
  */
class BCanonicalOrder[V : TypeTag : ClassTag](startingEdge: Embedding[V] => (V, V)) extends CanonicalOrder[V] {

  /**
    * @return A canonical order algorithm that selects an arbitrary starting edge.
    */
  def this() = this((e: Embedding[V]) => {
    require(!e.isEmpty, "Cannot retrieve starting edge from an empty embedding.")
    e.edges.head
  })

  /**
    * @param embedding A planar undirected embedding.
    * @return The vertices in the graph, in the order they appear in the canonical ordering.
    */
  override def order(embedding: Embedding[V]): Vector[V] = {
    require(!embedding.isEmpty, "Cannot compute canonical orderings on an empty embedding.")
    require(new DFSComponentSearch[V, UnDiEdge]().isBiconnected(GraphCommons.embeddingAsGraph(embedding)), "Attempted to compute canonical order for a non-biconnected graph.")

    val indexData = buildIntData(embedding)
    val indexToOriginal = indexData._2
    val indexEmbedding = indexData._1
    val originalToIndex = indexToOriginal.map(_.swap)
    val faces = new FaceComputation[Int]()
      .computeFaces(indexEmbedding)
      .zipWithIndex
      .map(f => new IndexFace(f._2, f._1))
      .sortBy(_.index) // Every face is stored at the position of its index
    val faceManager = new FaceMembershipManager[Int, IndexFace](faces)
    val vertexNr = indexEmbedding.vertexSize
    var addedVertices = 0

    /* Stores how many edges from a face (having the same index as the position in the vector) that is currently
     * in the graph G(k-1).
     */
    val af = Array.fill[Int](faces.length)(0)

    /* Stores at each index n how many neighbors the vertex n has in G(k-1). */
    val nv = Array.fill[Int](indexEmbedding.vertexSize)(0)

    /* Stores at each index n (where n is a vertex) the number of ready faces that have vertex n as the only vertex outside G(k-1). */
    val fv = Array.fill[Int](indexEmbedding.vertexSize)(0)

    /* Start edge with vertices v1 and v2. */
    val startEdge = startingEdge(embedding)
    require(embedding.embeddingFor(startEdge._1).containsEntryFor(startEdge._2), "The vertex pair selected as starting edge are not neighbors in the original embedding.")
    val v1 = originalToIndex(startEdge._1)
    val v2 = originalToIndex(startEdge._2)
    val outerFace = faceManager.rightFace(v1, v2)

    /* Increment the number of visits for all neighbors of v1 and v2 by 1. */
    for (n <- indexEmbedding.neighborsOf(v1) ++ indexEmbedding.neighborsOf(v2)) {
      nv(n) = nv(n) + 1
    }

    /* Set A(f) to 1 for the left face of (v1, v2). */
    val leftFace = faceManager.leftFace(v1, v2)
    af(leftFace.index) = 1

    /* If the left face is a triangle, set F(v) to 1 since the face is no ready. */
    if (leftFace.vertexSize == 3)
      fv(leftFace.vertices.find(v => v != v1 && v != v2).get) = 1

    // Main algorithm starts here
    val ordering = new ListBuffer[V]() ++ List(indexToOriginal(v1), indexToOriginal(v2))
    var ck = new Contour[Int](v1, v2, indexEmbedding)
    addedVertices += 2

    for (k <- 2 until indexEmbedding.vertexSize) { // 0 and 1 has already been processed
      val neighbor = indexEmbedding.embeddedVertices.find(v => nv(v) >= 2
        && !ck.contains(v)
        && (nv(v) == fv(v) + 1))

      val vk = neighbor.getOrElse {

        // If no neighbor to add as vk was found, find a vertex with legal support and N(v) == 1 and add it to C(k)
        val newVk = indexEmbedding.embeddedVertices.find(v => nv(v) == 1 && !ck.contains(v) && v != v1 && v != v2 && {
          val neighborOnCk = indexEmbedding.neighborsOf(v).find(ck.onContour)

          if (neighborOnCk.isEmpty)
            false
          else {
            val n = neighborOnCk.get

            /* There are three cases of legal support (see article in class description): 1: The neighbor is v1 and vk
             * has right support, 2: the neighbor is v2 and vk has left support, 3: vk has either left and/or right
             * support.
             */
            (n == ck.v1 && ck.hasRightSupport(v)) ||
              (n == ck.v2 && ck.hasLeftSupport(v)) ||
              (ck.hasLeftSupport(v) || ck.hasRightSupport(v))
          }


        }).getOrElse(throw new Error("No canonical candidate was found to be inserted as v(k) onto C(k-1)."))

        newVk
      }

      ck = ck.addVertex(vk).newContour
      ordering += indexToOriginal(vk)
      addedVertices += 1

      if (addedVertices < vertexNr)
        updateArrays(vk, af, nv, fv, ck, indexEmbedding, faceManager, outerFace)
    }

    ordering.toVector
  }

  /**
    * Updates the array data structures after a vertex v(k) has been added to G(k-1).
    */
  private def updateArrays(v: Int,
                           af: Array[Int], nv: Array[Int], fv: Array[Int],
                           ck: Contour[Int], emb: Embedding[Int],
                           faces: FaceMembershipManager[Int, IndexFace],
                           outerFace: IndexFace): Unit = {

    // Every neighbor of v that isn't already in C(k) now has one more neighbor in G(k) since v has been added to it.
    for (n <- emb.neighborsOf(v).filterNot(ck.onContour)) {
      nv(n) += 1
    }

    // The leftmost and rightmost face now has one more edge in C(k), add +1 to af(f), and mark any single
    // remaining vertices as ready if that is the case.
    val leftAndRightNeighbor = ck.findLeftAndRightNeighbor(v)
    val leftFace = faces.leftFace(leftAndRightNeighbor._1, v)
    val rightFace = faces.rightFace(leftAndRightNeighbor._2, v)

//    if (leftFace != outerFace)
      updateFace(leftFace, af, fv, ck)

//    if (rightFace != outerFace)
      updateFace(rightFace, af, fv, ck)
  }

  /**
    * Updates a face that has a vertex from it added to C(k). If the face becomes ready as a result, increments F(v)
    * by 1. A face is ready when every edge except two is in C(k).
    *
    * @param ck C(k), contains the vertex v(k) that was added.
    */
  private def updateFace(f: IndexFace, af: Array[Int], fv: Array[Int], ck: Contour[Int]): Unit = {
    af(f.index) += 1
    if (af(f.index) == f.edgeSize - 2) {
      val remainingVertex = f.vertices.filterNot(ck.contains)
      assert(remainingVertex.length == 1, "No single vertex (" + remainingVertex.mkString(", ") + ") was found in the face " + f + ", despite f being ready.")
      fv(remainingVertex.head) += 1
    }
  }

  private def buildIntData(embedding: Embedding[V]): (Embedding[Int], Map[Int, V]) = {
    val indexGraph = IndexGraph.undirected[V](GraphCommons.embeddingAsGraph(embedding))
    val vertices = GraphCommons.outerVertices(indexGraph)
    val im = (for (n <- vertices) yield n.valueOfIndex -> n.index).toMap
    (embedding.map((v: V) => im(v)), im.map(_.swap))
  }

  /**
    * A face with an associated index value, used for vector storage and lookup in constant time.
    */
  private class IndexFace(val index: Int, f: Face[Int]) extends Face[Int](f.vertices) {
    override def equals(other: Any): Boolean = f.equals(other)
    override def hashCode: Int = f.##
    override def toString: String = f.toString
  }

}
