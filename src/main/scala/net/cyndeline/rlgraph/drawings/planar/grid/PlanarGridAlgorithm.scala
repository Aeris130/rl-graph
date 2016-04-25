package net.cyndeline.rlgraph.drawings.planar.grid

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point}
import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.canonicalOrder.planarBiconnected.{BCanonicalOrder, Contour}
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
  * by D. Harel and M. Sardas described in "An Incremental Drawing Algorithm for Planar Graphs". Doing so prevents
  * the final drawing from becoming needlessly convex when the triangulating dummy-edges are removed.
  */
//TODO Test building a drawing from a graph
//TODO test exception throwing when not every rectangle dimension is specified.
class PlanarGridAlgorithm {

  /**
    * Computes a planar grid drawing from an arbitrary planar graph.
    * @param g Graph to draw. Must contain at least two vertices and be planar. If the graph is not biconnected, the
    *          final drawing will be based on a graph having dummy-edges to achieve biconnectivity.
    * @return A planar grid drawing of the graph using an arbitrary start edge (as long as the original graph contains
    *         at least one edge, the starting edge is guaranteed to not be a dummy).
    */
  def computeDrawing(g: Graph[Int, UnDiEdge]): GridDrawing = {
    computeRectangleDrawing(g, GraphCommons.outerVertices(g).map(v => v -> Dimensions(1, 1)).toMap)
  }

  /**
    * Specifies rectangle dimensions for each vertex, causing the final drawing to space vertex coordinates apart in
    * order to avoid rectangle overlap.
    * @param g Graph to draw. Must contain at least two vertices and be planar. If the graph is not biconnected, the
    *          final drawing will be based on a graph having dummy-edges to achieve biconnectivity.
    * @param rectangles Maps each vertex in the embedding to the dimensions of the rectangle that it should be drawn as.
    * @return A grid drawing of the embedding.
    */
  def computeRectangleDrawing(g: Graph[Int, UnDiEdge], rectangles: Map[Int, Dimensions]): GridDrawing = {
    require(g.nodes.size > 1, "The submitted graph must contain at least two vertices.")
    val embedder = new DemoucronEmbedding[Int, UnDiEdge]()
    val biconnectivity = new BiconnectivityOperation[Int]()
    val biconnectedGraph = biconnectivity.biconnect(g).graph
    val embedding = embedder.embed(biconnectedGraph).getOrElse(throw new Error("The submitted graph was not planar."))
    val startEdge = if (g.edges.nonEmpty) GraphCommons.outerEdges(g).head else GraphCommons.outerEdges(biconnectedGraph).head
    computeDrawing(embedding, (startEdge._1, startEdge._2))
  }

  /**
    * Computes a planar grid drawing with every vertex occupying a single coordinate.
    * @param e A planar biconnectd embedding.
    * @param startEdge An edge in the embedding that should be used as the bottom edge in the drawing (v1 to v2).
    *                  This edge may be used to affect the final drawing, as its left face (when traversing the faces of
    *                  the embedding counter clockwise in each vertex edge set) will be the first internal face of the
    *                  drawing.
    * @return A grid drawing of the embedding.
    */
  def computeDrawing(e: Embedding[Int], startEdge: (Int, Int)): GridDrawing = {
    computeRectangleDrawing(e, startEdge, e.embeddedVertices.map(v => v -> Dimensions(1, 1)).toMap)
  }

  /**
    * Specifies rectangle dimensions for each vertex, causing the final drawing to space vertex coordinates apart in
    * order to avoid rectangle overlap.
    * @param e A planar biconnectd embedding.
    * @param startEdge An edge in the embedding that should be used as the bottom edge in the drawing (v1 to v2).
    *                  This edge may be used to affect the final drawing, as its left face (when traversing the faces of
    *                  the embedding counter clockwise in each vertex edge set) will be the first internal face of the
    *                  drawing.
    * @param rectangles Maps each vertex in the embedding to the dimensions of the rectangle that it should be drawn as.
    * @return A grid drawing of the embedding.
    */
  def computeRectangleDrawing(e: Embedding[Int], startEdge: (Int, Int), rectangles: Map[Int, Dimensions]): GridDrawing = {
    require(rectangles.keySet == e.embeddedVertices.toSet, "Every vertex in the embedding did not have a rectangle dimension specified.")
    require(e.embeddingFor(startEdge._1).containsEntryFor(startEdge._2), "The start edge " + startEdge + " was not found in the embedding.")
    val canonicalOrder: Vector[Int] = new BCanonicalOrder((e: Embedding[Int]) => startEdge).order(e)

    // Associate each vertex with an index, for use in the binary tree
    val indexVertexOrder = canonicalOrder.zipWithIndex.map(_._2)
    val originalToIndex: Map[Int, Int] = canonicalOrder.zip(indexVertexOrder).toMap
    val indexToOriginal = originalToIndex.map(_.swap)
    val indexEmbedding = e.map(originalToIndex)
    val nrOfVertices = originalToIndex.size

    // Data for the final drawing
    val y = Array.fill(nrOfVertices)(0)
    val deltaX = Array.fill(nrOfVertices)(0)
    var lrTree = new LeftRightTree(nrOfVertices)
    var children = Map[Int, (Int, Int)]()
    var cover = Map[Int, Vector[Int]]()

    /* Begin the algorithm by setting default values for the first three vertices in the canonical order(v1, v2, v3) */
    val v1 = originalToIndex(canonicalOrder(0))
    val v2 = originalToIndex(canonicalOrder(1))

    deltaX(v1) = 0
    deltaX(v2) = 1

    y(v1) = 0
    y(v2) = 0

    var contour = new Contour(v1, v2, indexEmbedding)

    // Special case: Data for vertex v3 is set up if more than 2 vertices exists.
    if (canonicalOrder.length > 2) {
      val v3 = originalToIndex(canonicalOrder(2))
      deltaX(v3) = 1
      y(v3) = 1

      /* v3 is the right child of v1, v2 is the right child of v3 as the current outer contour C(k) (k = 3) is 1, 3, 2. */
      lrTree = lrTree.addRight(v1, v3).addRight(v3, v2)
      children += ((indexToOriginal(v3), (indexToOriginal(v1), indexToOriginal(v2))))
      contour = contour.addVertex(v3).newContour
    }

    /* Add every vertex to the drawing and compute their y-coordinates and x-offsets. */
    for (vk <- 3 until nrOfVertices) {
      val modification = contour.addVertex(vk)
      children += ((indexToOriginal(vk), (indexToOriginal(modification.leftNeighbor), indexToOriginal(modification.rightNeighbor))))
      cover += ((indexToOriginal(vk), modification.vertexCover.map(indexToOriginal)))

      /* Let w(p) ... w(q) be the neighbors of v on C(k-1) from left to right.
       * Adjust the x-offset of w(p+1) and w(q) by 1.
       */
      //TODO Increment w(q) by 2 if w(q) == w(p+1) ? Doing it for now. Delete this comment if no errors are found.
      val neighborsPastLeftChild = modification.vertexCover :+ modification.rightNeighbor
      deltaX(modification.rightNeighbor) += 1
      deltaX(neighborsPastLeftChild.head) += 1

      // deltaX(wp, wq) = deltaX(wp+1) + ... + deltaX(wq)
      val deltaPQ = neighborsPastLeftChild.map(n => deltaX(n)).sum

      // deltaX(vk) = 1/2 * [-y(wp) + deltaX(wp, wq) + y(wq)]
      val xDelta = -y(modification.leftNeighbor) + deltaPQ + y(modification.rightNeighbor)
      assert(xDelta % 2 == 0, "Odd delta detected, cannot be divided by 2 and still be a valid grid coordinate.")
      deltaX(vk) = (0.5 * xDelta).toInt

      // y(vk) = 1/2 * [y(wp) + deltaX(wp, wq) + y(wq)]
      val yValue = y(modification.leftNeighbor) + deltaPQ + y(modification.rightNeighbor)
      assert(xDelta % 2 == 0, "Odd y-value detected, cannot be divided by 2 and still be a valid grid coordinate.")
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

    new GridDrawing(canonicalOrder, e.edges, coordinates, Map())
  }

  private def accumulateXOffsets(v: Int, tree: LeftRightTree, deltaX: Array[Int], value: Int = 0): Unit = {
    deltaX(v) = deltaX(v) + value

    if (tree.hasLeft(v))
      accumulateXOffsets(tree.leftChild(v), tree, deltaX, deltaX(v))
    if (tree.hasRight(v))
      accumulateXOffsets(tree.rightChild(v), tree, deltaX, deltaX(v))
  }

}
