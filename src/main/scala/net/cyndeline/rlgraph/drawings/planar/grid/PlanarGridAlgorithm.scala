package net.cyndeline.rlgraph.drawings.planar.grid

import java.util.concurrent.TimeUnit

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.canonicalOrder.planarBiconnected.{BCanonicalOrder, Contour}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.grid.binaryTree.LeftRightTree
import net.cyndeline.rlgraph.embedding.Embedding
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding
import net.cyndeline.rlgraph.util.GraphCommons
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Computes a planar straight-line drawing of a biconnected graph based on the algorithm based on the algorithm by
  * M. Chrobak and T.H. Payne as outlined in "A Linear-time Algorithm for Drawing a Planar Graph on a Grid". As the
  * original algorithm required the input graph to be maximally planar (easiest achieved by triangulating the graph if
  * this was not the case), the canonical ordering used to produce the drawing is computed by the generalized algorithm
  * by D. Harel and M. Sardas described in "An Incremental Drawing Algorithm for Planar Graphs" which only requires
  * the input graph to be biconnected instead. Doing so prevents the final drawing from becoming needlessly convex when
  * the (would-be) triangulating dummy-edges are removed.
  */
class PlanarGridAlgorithm {

  /**
    * Computes a planar grid drawing from an arbitrary planar graph.
    * @param g Graph to draw. Must contain at least two vertices and be planar. If the graph is not biconnected, the
    *          final drawing will be based on a graph having dummy-edges to achieve biconnectivity.
    * @return A planar grid drawing of the graph using an arbitrary start edge (as long as the original graph contains
    *         at least one edge, the starting edge is guaranteed to not be a dummy).
    */
  def computeDrawing(g: Graph[Int, UnDiEdge]): StraightLineDrawing[Int] = {
    if (g.isEmpty)
      return StraightLineDrawing.empty[Int]
    else if (g.nodes.size == 1)
      return StraightLineDrawing.singleVertex(g.nodes.head)

    val embedder = new DemoucronEmbedding[Int, UnDiEdge]()
    val biconnectData = new BiconnectivityOperation[Int]().biconnect(g)
    val biconnectedGraph = biconnectData.graph // Solves connectivity and lack of edges

    val tBeforeEmbedding = java.lang.System.currentTimeMillis()

    val embedding = embedder.embed(biconnectedGraph).getOrElse(throw new Error("The submitted graph was not planar."))

    val tAfterEmbedding = java.lang.System.currentTimeMillis()

    val startEdge = if (g.edges.nonEmpty) GraphCommons.outerEdges(g).head else GraphCommons.outerEdges(biconnectedGraph).head
    val finalDrawing = computeDrawing(embedding, (startEdge._1, startEdge._2))

    val tAfterDrawing = java.lang.System.currentTimeMillis()

    //TODO remove after linear embedding algorithm has been measured.
    println("Total time: " + TimeUnit.MILLISECONDS.toSeconds(tAfterDrawing - tBeforeEmbedding))
    println("Time to embed: " + TimeUnit.MILLISECONDS.toSeconds(tAfterEmbedding - tBeforeEmbedding))
    println("Time to draw: " +  TimeUnit.MILLISECONDS.toSeconds(tAfterDrawing - tAfterEmbedding))
    println("----------------------------------")

    // Remove any dummy-edges
    val dummySet = biconnectData.extraEdges.toSet
    finalDrawing.updateEdges(finalDrawing.edges.filterNot(e => dummySet.contains(e) || dummySet.contains((e._2, e._1))))
  }

  /**
    * Computes a planar grid drawing with every vertex occupying a single coordinate.
    * @param e A planar biconnected embedding.
    * @param startEdge An edge in the embedding that should be used as the bottom edge in the drawing (v1 to v2).
    *                  This edge may be used to affect the final drawing, as its left face (when traversing the faces of
    *                  the embedding counter clockwise in each vertex edge set) will be the first internal face of the
    *                  drawing.
    * @return A grid drawing of the embedding.
    */
  def computeDrawing(e: Embedding[Int], startEdge: (Int, Int)): StraightLineDrawing[Int] = {
    require(e.embeddingFor(startEdge._1).containsEntryFor(startEdge._2), "The start edge " + startEdge + " was not found in the embedding.")
    val canonicalOrder: Vector[Int] = new BCanonicalOrder((e: Embedding[Int]) => startEdge).order(e)

    // Associate each vertex with an index, for use in the binary tree
    val indexVertexOrder = canonicalOrder.zipWithIndex.map(_._2)
    val originalToIndex: Map[Int, Int] = canonicalOrder.zip(indexVertexOrder).toMap
    val indexEmbedding = e.map(originalToIndex)
    val nrOfVertices = originalToIndex.size

    // Data for the final drawing
    val y = Array.fill(nrOfVertices)(0)
    val deltaX = Array.fill(nrOfVertices)(0)
    var lrTree = new LeftRightTree(nrOfVertices)

    /* Begin the algorithm by setting default values for the first three vertices in the canonical order(v1, v2, v3) */
    val v1 = originalToIndex(canonicalOrder(0))
    val v2 = originalToIndex(canonicalOrder(1))

    deltaX(v1) = 0
    deltaX(v2) = 1

    y(v1) = 0
    y(v2) = 0

    var contour = new Contour(v1, v2, indexEmbedding)

    /* Special case: Data for vertex v3 is set up if more than 2 vertices exists. */
    if (canonicalOrder.length > 2) {
      val v3 = originalToIndex(canonicalOrder(2))
      deltaX(v3) = 1
      y(v3) = 1

      /* v3 is the right child of v1, v2 is the right child of v3 as the current outer contour C(k) (k = 3) is 1, 3, 2. */
      lrTree = lrTree.addRight(v1, v3).addRight(v3, v2)
      contour = contour.addVertex(v3).newContour
    }

    /* Add every vertex to the drawing and compute their y-coordinates and x-offsets. */
    for (vk <- 3 until nrOfVertices) {
      val modification = contour.addVertex(vk)

      /* Let w(p) ... w(q) be the neighbors of v on C(k-1) from left to right.
       * Adjust the x-offset of w(p+1) and w(q) by 1.
       */
      val neighborsPastLeftChild = modification.vertexCover :+ modification.rightNeighbor
      deltaX(modification.rightNeighbor) += 1
      deltaX(neighborsPastLeftChild.head) += 1

      // deltaX(wp, wq) = deltaX(wp+1) + ... + deltaX(wq)
      val deltaPQ = neighborsPastLeftChild.map(n => deltaX(n)).sum

      // deltaX(vk) = 1/2 * [-y(wp) + deltaX(wp, wq) + y(wq)]
      val xDelta = -y(modification.leftNeighbor) + deltaPQ + y(modification.rightNeighbor)
      assert(xDelta % 2 == 0, "Odd x-delta found.")
      deltaX(vk) = (0.5 * xDelta).toInt

      // y(vk) = 1/2 * [y(wp) + deltaX(wp, wq) + y(wq)]
      val yValue = y(modification.leftNeighbor) + deltaPQ + y(modification.rightNeighbor)
      assert(yValue % 2 == 0, "Odd y value found.")
      y(vk) = (0.5 * yValue).toInt

      // deltaX(wq) = deltaX(wp, wq) - deltaX(vk)
      deltaX(modification.rightNeighbor) = deltaPQ - deltaX(vk)

      // if (p+1) != q then deltaX(wp+1) = deltaX(wp+1) - deltaX(vk)
      if (neighborsPastLeftChild.head != modification.rightNeighbor)
        deltaX(neighborsPastLeftChild.head) = deltaX(neighborsPastLeftChild.head) - deltaX(vk)

      /* Install vk in the binary tree. */
      lrTree = if (lrTree.hasRight(modification.leftNeighbor)) lrTree.removeRight(modification.leftNeighbor) else lrTree
      lrTree = if (lrTree.hasRight(vk)) lrTree.removeRight(vk) else lrTree
      lrTree = lrTree.addRight(modification.leftNeighbor, vk)
        .addRight(vk, modification.rightNeighbor)

      if (neighborsPastLeftChild.head != modification.rightNeighbor) {
        lrTree = lrTree.addLeft(vk, neighborsPastLeftChild.head)
            .removeRight(modification.vertexCover.last) // Right child of (q-1) = Nil

      } else {
        lrTree = if (lrTree.hasLeft(vk)) lrTree.removeLeft(vk) else lrTree
      }

      contour = modification.newContour
    }

    /* Finally, traverse the binary tree and accumulate the x-offsets to use as x-values for the vertex coordinates. */
    accumulateXOffsets(v1, lrTree, deltaX, 0)
    val coordinates = canonicalOrder.map(v => v -> {
      val indexV = originalToIndex(v)
      Point(deltaX(indexV), y(indexV))
    }).toMap

    new StraightLineDrawing(e.embeddedVertices, e.edges, coordinates, coordinates.values.maxBy(_.x).x + 1, coordinates.values.maxBy(_.y).y + 1)
  }

  /* Traverses the tree and sets x-offsets for each node according to the acummulated offset values for the nodes
   * before it.
   */
  private def accumulateXOffsets(v: Int, tree: LeftRightTree, deltaX: Array[Int], value: Int = 0): Unit = {
    deltaX(v) = deltaX(v) + value

    if (tree.hasLeft(v))
      accumulateXOffsets(tree.leftChild(v), tree, deltaX, deltaX(v))
    if (tree.hasRight(v))
      accumulateXOffsets(tree.rightChild(v), tree, deltaX, deltaX(v))
  }

}
