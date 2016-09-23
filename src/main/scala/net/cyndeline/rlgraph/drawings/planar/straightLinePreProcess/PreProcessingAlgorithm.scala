package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.grid.PlanarGridAlgorithm
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.centerField.CenterPreProcessing
import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding
import spire.math.Rational

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Computes a randomized planar straight-line drawing by using a straight-line grid drawing as base and then
  * applying a series of processing steps to it to remove the triangular properties of the drawing without
  * introducing additional edge crossings. A more detailed description of the algorithm can be found in the
  * article "Randomized graph drawing with heavy-duty preprocessing" by D.Harel and M.Sardas.
  *
  * The steps of the algorithm are as follows:
  *
  *  1. A planar grid drawing is computed from the initial graph
  *  1. Every vertex in the initial drawing has its position adjusted by moving it towards the centroid of
  * its neighbors. This is not enough to create a pleasing graph layout, but will even out edge lengths, letting
  * the next algorithm perform the finer adjustments.
  *  1. A simulated annealing algorithm is used, which scores produced graphs based on edge length, distance to
  * the border of the drawing and euclidean distance between the vertices (ensuring even spread across the drawing).
  *
  * Regarding temperature: An initial temperature of 0.8 times the total area available will be used.
  *
  * Regarding performance: The largest user-influenced factor in performance is how many iterations the annealing
  * process should run at each temperature. 10 temperatures will be ran in total (after each, the temperature will be
  * cooled to produce the next), and each temperature will run the iteration once. Each iteration processes all n
  * vertices in the drawing. Thus, a drawing with 100 vertices and 30 iterations will result in (10 * 100 * 30)
  * vertices being processed.
  */
class PreProcessingAlgorithm {

  /**
    * @param g        A planar graph.
    * @param random   Random object used to determine the positions to move vertices to.
    * @param settings Algorithm parameters.
    * @return A straight line drawing of the graph, having been processed by the algorithm.
    */
  def computeDrawing(g: Graph[Int, UnDiEdge], random: Random, settings: Settings): StraightLineDrawing[Int] = {
    if (g.isEmpty)
      return StraightLineDrawing.empty[Int]
    else if (g.size == 1)
      return StraightLineDrawing.singleVertex[Int](g.nodes.head)

    /* Two minimize edge length, biconnecting the graph is a task we won't leave to the grid drawing algorithm.
     * Instead the drawing will be biconnected here, and the starting edge of the drawing will be selected
     * from the longest face, ensuring a maximum number of vertices on the outer contour of the layout.
     */
    val biconnectivity = new BiconnectivityOperation[Int]().biconnect(g)
    val graphWithExtraEdges = biconnectivity.graph
    val extraEdges = biconnectivity.extraEdges

    val embedding = new DemoucronEmbedding[Int, UnDiEdge].embed(graphWithExtraEdges)
      .getOrElse(throw new IllegalArgumentException("Input graph was not planar"))
    val faces = new FaceComputation[Int].computeFaces(embedding)
    val longestFace = faces.maxBy(_.vertexSize)
    val startEdge = longestFace.edges.head

    /* Step 1: Compute initial drawing. */
    val planarGridDrawing = new PlanarGridAlgorithm().computeDrawing(embedding, startEdge)

    /* Step 2: Centralize vertices based on their neighbors. */
    var centralizer = CenterPreProcessing(planarGridDrawing)
    var i = 0
    while (i < 2) {
      centralizer = centralizer.adjustAll.getOrElse(centralizer)
      i += 1
    }

    /* Step 3: Before modifying the vertices using the annealing algorithm, the drawing must be scaled up such that
     * every vertex rectangle is guaranteed to fit within its borders. The drawings bound is also increased by a
     * user-specified constant, to give the algorithm more space to adjust rectangles around.
     */
    val scaledDrawing = StraightLineDrawing
      .scale(planarGridDrawing.updateCoordinates(centralizer.allCoordinates), settings.rectangles)

    val finalSize = (settings.boundMultiplier * Math.max(scaledDrawing.width, scaledDrawing.height)).toInt
    val adjustCoordinatesBy = finalSize / 2

    // Final drawing before modification
    val adjustedDrawing = StraightLineDrawing.adjust(adjustCoordinatesBy, adjustCoordinatesBy, scaledDrawing)

    /* Step 4: Run annealing algorithm. */
    val annealingSettings = AnnealingProcess.Settings(
      temperature = Rational(finalSize) * 0.8,
      cooling = Rational(settings.cooling),
      targetEdgeLength = Rational(settings.targetEdgeLength),
      edgeWeight = Rational(settings.edgeWeight),
      borderWeight = Rational(settings.borderWeight),
      distributionWeight = Rational(settings.distributionWeight)
    )
    var annealingAlgorithm = AnnealingProcess(adjustedDrawing, graphWithExtraEdges, finalSize, settings.rectangles, annealingSettings)
    var conseqFailedAttempts = 0
    var temperaturesLeft = 10

    /* According to Harel and Sardas, if this stage of the algorithm fails to find a valid adjustment 2-3 stages in
     * a row, it is unlikely to exist, and the algorithm can be terminated early to save time.
     */
    while (temperaturesLeft > 0 && conseqFailedAttempts < 3) {
      val result = annealingAlgorithm.run(settings.iterations, random)

      if (result.isDefined) {
        annealingAlgorithm = result.get.cool
        conseqFailedAttempts = 0
      } else {
        annealingAlgorithm = annealingAlgorithm.cool
        conseqFailedAttempts += 1
      }
      temperaturesLeft -= 1
    }

    val finalDrawing = buildDrawing(adjustedDrawing.vertices, adjustedDrawing.edges, annealingAlgorithm)
    val extraEdgeSet = extraEdges.toSet
    finalDrawing.updateEdges(finalDrawing.edges.filterNot(e => extraEdgeSet.contains(e) || extraEdgeSet.contains((e._2, e._1))))
  }

  private def buildDrawing(vertices: Vector[Int], edges: Vector[(Int, Int)], annealing: AnnealingProcess) = {
    require(!annealing.coordinates.exists(kv => kv._2.x < 0 || kv._2.y < 0), "Cannot build drawing from negative coordinates.")
    require(vertices.nonEmpty && annealing.coordinates.nonEmpty, "Cannot build drawing from empty vertex/coordinate set.")
    val rectangles = vertices.map(annealing.rectangle)
    val maxX = rectangles.maxBy(_.stop.x).stop.x
    val maxY = rectangles.maxBy(_.stop.y).stop.y

    StraightLineDrawing.adjustToZero(new StraightLineDrawing(vertices, edges, annealing.coordinates, maxX.toInt, maxY.toInt))
  }

}
