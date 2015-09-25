package net.cyndeline.rlgraph.planar

import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.face.Face

/**
 * Embeds a set of edges inside a face such that the face remains planar, assuming the edges allows it to be.
 */
class PlanarFaceAugmentation {

  /**
   * @param edges Every edge to add to a face.
   * @param face The face to add edges to.
   * @param embedding The embedding used to compute the face.
   * @tparam V Vertex type in the embedding.
   */
  def embedEdges[V](edges: Vector[(V, V)], face: Face[V], embedding: Embedding[V]): Embedding[V] = {
    var currentEmbedding = embedding
    val verticesWithIndex = face.vertices.zipWithIndex
    val indexMap: Map[V, Int] = verticesWithIndex.map(vi => vi._1 -> vi._2).toMap
    val edgesWithIndex: Vector[((V, Int), (V, Int))] = edges.map(e => ((e._1, indexMap(e._1)), (e._2, indexMap(e._2))))
    val adjacencyMap: Map[V, Adjacency[V]] = verticesWithIndex.map(vi => vi._1 -> {
      val next = face.vertexPresentAfter(vi._1)
      val nextEntry = (next, indexMap(next))
      new Adjacency(vi._1, vi._2, nextEntry)
    }).toMap

    for (e <- edgesWithIndex) {
      adjacencyMap(e._1._1).addEdge(e)
      adjacencyMap(e._2._1).addEdge(e)
    }

    for (adj <- adjacencyMap.values) {
      adj.sort()
    }

    for (e <- edgesWithIndex) {
      val a: (V, Int) = e._1
      val b: (V, Int) = e._2
      val aAdj = adjacencyMap(a._1)
      val bAdj = adjacencyMap(b._1)
      val aPos = aAdj.getInsertPoint(b)
      val bPos = bAdj.getInsertPoint(a)

      currentEmbedding = currentEmbedding.embedEdge(Vertex(a._1) withInsertPosition bPos inVertex b._1 withInsertPosition aPos)

      aAdj.markAsEmbedded(b)
      bAdj.markAsEmbedded(a)
    }

    currentEmbedding
  }

  /**
   * Holds the vertex adjacencies of a single vertex, and marks weather or not they've been embedded.
   * @param v The vertex.
   * @param index The index of v on the face.
   * @param next The next vertex and index on the original face.
   */
  private class Adjacency[V](v: V, index: Int, next: (V, Int)) {
    private var edges = Vector[EdgeNeighbor]() // Every edge going to vertices with lower index than v

    def addEdge(e: ((V, Int), (V, Int))) {
      val oppositeEntry = opposite(e)
      edges = new EdgeNeighbor(oppositeEntry._1, oppositeEntry._2) +: edges
    }

    def markAsEmbedded(entry: (V, Int)): Unit = {
        edges.find(_.v == entry._1).get.embedded = true
    }

    def sort(): Unit = {
      edges = edges.sortWith(_.index > _.index)
    }

    /**
     * Retrieves the insert position for another vertex around the owner of this adjacency, based on its index.
     * If no edge has been embedded here yet, the next vertex on the original face will be used.
     *
     * When inserting an edge from A to B:
     *
     * If A > B, then B's position in A is the highest entry with lower index than B, or the lowest entry with index
     * higher than A.
     *
     * If A < B then B's position is the highest entry whose index is higher than A and lower than B.
     *
     * @param other Another vertex with its index, representing an edge from here to 'other.
     * @return One of the vertices currently embedded around this vertex, or its previous vertex on the original
     *         face if no such vertex exists, or if the other vertex should be embedded first.
     */
    def getInsertPoint(other: (V, Int)): V = {

      if (index > other._2) {
        val candidates = edges.filter(_.embedded)
        val lowerThanOther = candidates.filter(_.index < other._2)

        if (lowerThanOther.isEmpty) {
          val higherThanThis = candidates.filter(_.index > index)

          if (higherThanThis.isEmpty) {
            next._1
          } else {
            higherThanThis.maxBy(_.index).v
          }

        } else {
          lowerThanOther.maxBy(_.index).v
        }

      } else {
        val candidates = edges.filter(e => e.embedded && e.index > index && e.index < other._2)
        if (candidates.isEmpty) {
          next._1

        } else {
          candidates.maxBy(_.index).v
        }
      }
    }

    private def opposite(e: ((V, Int), (V, Int))): (V, Int) = if (v == e._1._1) e._2 else e._1

    private class EdgeNeighbor(val v: V, val index: Int) {
      var embedded = false
      override def toString: String = "(" + v + "," + index + ")" + (if (embedded) "/Embedded" else "")
    }

    override def toString: String = "Adjacency -> " + v + "(" + index + ") :: edges [" + edges.mkString(", ") + "]"
  }



}
