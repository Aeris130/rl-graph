package net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess

import net.cyndeline.rlcommon.math.geom.{Dimensions, Rectangle}
import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.BiconnectivityOperation
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.grid.PlanarGridAlgorithm
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.annealing.AnnealingProcess
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.centerField.CenterPreProcessing
import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.planar.demoucron.operation.DemoucronEmbedding
import net.cyndeline.rlgraph.util.graphConverters.IntConverter
import scalax.collection.GraphPredef._

import scala.collection.mutable
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
  def computeDrawing[S <: LayoutState[S]](g: Graph[Int, UnDiEdge], random: Random, settings: Settings[S]): StraightLineDrawing[Int] = {
    if (g.isEmpty)
      return StraightLineDrawing.empty[Int]
    else if (g.size == 1)
      return StraightLineDrawing.singleVertex[Int](g.nodes.head)

    val graphAt0Conversion = IntConverter.convert(g)
    val intToOriginal = graphAt0Conversion._2.zipWithIndex.map(_.swap).toMap
    val graph0 = graphAt0Conversion._1

    /* Two minimize edge length, biconnecting the graph is a task we won't leave to the grid drawing algorithm.
     * Instead the drawing will be biconnected here, and the starting edge of the drawing will be selected
     * from the longest face, ensuring a maximum number of vertices on the outer contour of the layout.
     */
    val biconnectivity = new BiconnectivityOperation[Int]().biconnect(graph0)
    val graphWithExtraEdges = biconnectivity.graph
    val extraEdges = biconnectivity.extraEdges
    val extraEdgeSet = extraEdges.map(UnorderedPair(_))

    val embedding = new DemoucronEmbedding[Int, UnDiEdge].embed(graphWithExtraEdges)
      .getOrElse(throw new IllegalArgumentException("Input graph was not planar"))
    val faces = new FaceComputation[Int].computeFaces(embedding)
    val longestFace = faces.maxBy(f => f.vertexSize - f.edges.count(e => extraEdgeSet.contains(UnorderedPair(e))))
    val startEdge = (longestFace.edges.head._2, longestFace.edges.head._1)

    /* Step 1: Compute initial drawing. */
    val planarGridDrawing = new PlanarGridAlgorithm().computeDrawing(embedding, startEdge)

    /* Biconnecting the graph may have introduced some extra edges. However, all such edges can't be removed
     * until after the algorithm terminates, as edge intersections are how illegal moves are detected.
     * Instead, every vertex with no neighbors in the original graph only keeps a single extra edge added to it.
     */
    val trimmedGraphAndDrawing = trimEdges(graphWithExtraEdges, planarGridDrawing)
    val trimmedGraph = trimmedGraphAndDrawing._1
    val trimmedDrawing = trimmedGraphAndDrawing._2

    /* Step 2: Centralize vertices based on their neighbors. */
    var centralizer = CenterPreProcessing(trimmedDrawing)
    var i = 0
    while (i < settings.centerIterations) {
      centralizer = centralizer.adjustAll.getOrElse(centralizer)
      i += 1
    }

    val centralizedDrawing = trimmedDrawing.updateCoordinates(centralizer.allCoordinates)

    val finalSize = (settings.boundMultiplier * Math.max(centralizedDrawing.width, centralizedDrawing.height)).toInt
    val adjustCoordinatesBy = finalSize / 2

    // Final drawing before modification
    val adjustedDrawing = centralizedDrawing.adjust(adjustCoordinatesBy, adjustCoordinatesBy)

    /* Step 3: Run annealing algorithm. */
    val annealingSettings = AnnealingProcess.Settings(
      temperature = finalSize * 0.8,
      cooling = settings.cooling,
      costFunctions = settings.costFunctions
    )

    var annealingAlgorithm = AnnealingProcess(adjustedDrawing, trimmedGraph, finalSize, annealingSettings, settings.initialState)
    var conseqFailedAttempts = 0
    var temperaturesLeft = 10

    /* According to Harel and Sardas, if this stage of the algorithm fails to find a valid adjustment 2-3 stages in
     * a row, it is unlikely to exist, and the algorithm can be terminated early to save time.
     */
    while (temperaturesLeft > 0 && conseqFailedAttempts < 3) {
      val result = annealingAlgorithm.run(settings.annealingIterations, random)

      if (result.isDefined) {
        annealingAlgorithm = result.get.cool
        conseqFailedAttempts = 0
      } else {
        annealingAlgorithm = annealingAlgorithm.cool
        conseqFailedAttempts += 1
      }
      temperaturesLeft -= 1
    }

    val originalEdgeSet = adjustedDrawing.edges.map(UnorderedPair(_)) diff extraEdges.map(UnorderedPair(_))
    buildDrawing(adjustedDrawing.vertices, originalEdgeSet.map(_.asTuple), annealingAlgorithm)
      .map(intToOriginal)
      .cut
  }

  private def buildDrawing[S <: LayoutState[S]](vertices: Vector[Int], edges: Vector[(Int, Int)], annealing: AnnealingProcess[S]) = {
    require(!annealing.coordinates.exists(kv => kv._2.x < 0 || kv._2.y < 0), "Cannot build drawing from negative coordinates.")
    require(vertices.nonEmpty && annealing.coordinates.nonEmpty, "Cannot build drawing from empty vertex/coordinate set.")

    // Dimensions gets computed during cut.
    val d = new StraightLineDrawing(vertices, edges, annealing.coordinates, 1, 1)
    d.setSize(d.coordinateSize)
  }

  private def trimEdges(g: Graph[Int, UnDiEdge], drawing: StraightLineDrawing[Int]): (Graph[Int, UnDiEdge], StraightLineDrawing[Int]) = {
    val singles = g.nodes.filter(_.degree == 0)
    val isSingle = new mutable.HashSet[Int]()
    for (s <- singles)
      isSingle += s

    val connected = new mutable.HashSet[Int]()
    var trimmedGraph = g

    def connect(single: Int): Unit = connected += single
    def isConnected(v: Int) = !isSingle(v) || connected(v)

    val trimmedEdges = drawing.edges.flatMap(e => {
      val a = e._1
      val b = e._2

      if ((!isSingle(a) && !isSingle(b)) || (isSingle(a) || isSingle(b)) && (!isConnected(a) || !isConnected(b))) {
        connect(a)
        connect(b)
        Some(e)
      } else {
        trimmedGraph = trimmedGraph - a~b
        None
      }
    })

    (trimmedGraph, drawing.updateEdges(trimmedEdges))
  }

}
