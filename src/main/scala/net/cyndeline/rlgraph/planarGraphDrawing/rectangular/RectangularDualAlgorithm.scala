package net.cyndeline.rlgraph.planarGraphDrawing.rectangular

import java.util.UUID

import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.help._
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, RELFactory, RegularEdgeLabeling}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Constructs algoithms used to compute rectangular duals. For a description of the algorithm, see the
 * RectangularDualAlgorithm class documentation.
 */
object RectangularDualAlgorithm {

  /**
   * This factory method is recommended for users that doesn't want/need to input any data other than the graph itself.
   * @return An algorithm that only requires the user to input the graph data in order to produce a dual.
   */
  def regularAlgorithm[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]: RDualAlgorithm[VType, EType] =
    new RectangularDualAlgorithm[VType, EType](RAlgorithmSettings[VType, EType](), None)

  /**
   * @param settings User-supplied data for the algorithm.
   * @return An algorithm that only requires the user to input the graph data in order to produce a dual.
   */
  def regularAlgorithmWithSettings[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
    (settings: RAlgorithmSettings[VType, EType]): RDualAlgorithm[VType, EType] = new RectangularDualAlgorithm[VType, EType](settings, None)

  /**
   * Runs the dual algorithm once to produce an initial REL, then lets the user produce multiple drawings from the
   * same graph data by directly inserting regular edge labelings.
   *
   * @param graph The graph to construct the initial edge labeling from. This graph must contain at least 4 vertices,
   *              as layouts with less than 4 areas are handled using special cases instead of edge labelings. The
   *              regular algrorithm should be used for graphs with with 3 or less vertices.
   * @return An algorithm that produces rectangular layouts from a regular edge labeling, and any of its combinatorial
   *         versions. The algorithm also comes with an initial labeling that should be used when creating additional
   *         layouts. Note that any edge labelings that are not derived from this initial labeling will be invalid.
   */
  def edgeLabelAlgorithm[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
  (graph: Graph[VType, EType]): (RELDualAlgorithm[VType, EType], EdgeLabeling[RVertex[VType]]) =
    edgeLabelAlgorithmWithSettings(graph, RAlgorithmSettings[VType, EType]())


  /**
   * Runs the dual algorithm once to produce an initial REL, then lets the user produce multiple drawings from the
   * same graph data by directly inserting regular edge labelings.
   *
   * @param graph The graph to construct the initial edge labeling from. This graph must contain at least 4 vertices,
   *              as layouts with less than 4 areas are handled using special cases instead of edge labelings. The
   *              regular algrorithm should be used for graphs with with 3 or less vertices.
   * @param settings User-supplied data beside the graph for the algorithm.
   * @return An algorithm that produces rectangular layouts from a regular edge labeling, and any of its combinatorial
   *         versions. The algorithm also comes with an initial labeling that should be used when creating additional
   *         layouts. Note that any edge labelings that are not derived from this initial labeling will be invalid.
   */
  def edgeLabelAlgorithmWithSettings[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l]
    (graph: Graph[VType, EType], settings: RAlgorithmSettings[VType, EType]): (RELDualAlgorithm[VType, EType], EdgeLabeling[RVertex[VType]]) = {

    require(graph.nodes.size > 3, "Cannot create a REL algorithm using a graph with less than 4 vertices, as " +
      "such graphs are handled using special cases rather than a REL. Use the regular algorithm for this graph.")

    val relFactory = settings.regularEdgeLabelFactory
    val data = new DataSetup(graph, settings.outerFaceSelect)
    val regularEdgeLabeling = relFactory.produceRegularEdgeLabeling(data.embedding, data.vNorth, data.vSouth, data.vWest, data.vEast)
    val uniqueId = regularEdgeLabeling match {
      case rel: RegularEdgeLabeling[RVertex[VType]] => rel.id
    }

    (new RectangularDualAlgorithm(settings, Some((data, uniqueId, graph))), regularEdgeLabeling)
  }

}


/**
 * Computes a rectangular dual from a planar graph G. A rectangular dual is defined as a drawing where each vertex v
 * in a graph is represented as a rectangular area, with each area being adjacent to the rectangles corresponding to
 * the neighbors of v in G. Rectangles are positioned such that at most two corners intersect at any given coordinate
 * in the drawing.
 *
 * The algorithm proceeds as outlined in "Algorithms for drawing planar graphs" by G.Kant:
 *
 *  1. Connect and biconnect the graph, then select a face to be the outer face in the drawing.
 *  1. Augment the graph with four new vertices (North, South, West and East) that will serve as the boundary of the
 *  drawing (these will not be included in the final result).
 *  1. If the graph contains one or more separating triangles (a triangle that isn't a face), it means that no valid
 *  drawing can be found as the four-connectivity requirement for inner vertices in the triangle is broken. Every such
 *  triangle is removed by breaking one of its edges into two, and connecting both ends to a dummy vertex representing
 *  the split (every such split will be represented as a "gate" in the final drawing, with the previously neighboring
 *  vertices adjacent to it).
 *  1. For each biconnected face (except the outer), add additional edges to create a proper triangular planar graph
 *  (PTP graph) such that every inner face is a triangle, and every vertex not present in the outer face has degree > 3.
 *  The algorithm guarantees that no additional separating triangles will be introduced here.
 *  1. Construct the regular edge labeling from the PTP graph.
 *  1. Construct the South->North and West->East duals from the REL sets T1 and T2 (also referred to as red/blue or
 *  left/right depending on which REL article you read).
 *  1. Compute the distance from the source of each dual (South and West) to each vertex in it.
 *  1. Compute start and stop (x,y) coordinates using the dual distances.
 *
 * To improve the readability of the drawing, the algorithm also selects a valid outer face (if one exists) with the
 * highest amount of vertices possible unless the user specifies a custom face selection.
 *
 * Two different versions of the algorithm may be constructed:
 *
 *  - The regular algorithm: An algorithm that runs from start to finish anew every time a graph is inputted.
 *  - An REL-specific algorithm that only performs steps 1 to 5 once. After that, the user may input the constructed
 *  REL directly (and any variations derived from it by changing the color of its edges) to create a drawing directly.
 *
 * Use the factory object to create one of the available algorithms.
 */
class RectangularDualAlgorithm[VType : ClassTag : TypeTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l] private
  (settings: RAlgorithmSettings[VType, EType], staticData: Option[(DataSetup[VType, EType], UUID, Graph[VType, EType])]) extends RDualAlgorithm[VType, EType] with RELDualAlgorithm[VType, EType] {

  def computeLayout(graph: Graph[VType, EType]): RectangularLayout[VType, EType] = executeRegularAlgorithm(graph)

  def computeLayout(rel: EdgeLabeling[RVertex[VType]]): RectangularLayout[VType, EType] = executeRELAlgorithm(rel)

  /**
   * @param graph A planar graph.
   * @return A rectangular drawing of the graph.
   */
  private def executeRegularAlgorithm(graph: Graph[VType, EType]): RectangularLayout[VType, EType] = {
    require(!graph.isEmpty, "Cannot compute a rectangular dual from an empty graph.")

    /* Before the algorithm begins, three special cases are handled for graphs with 1, 2 and 3 vertices in them
     * since every drawing of those graphs look the same regardless how the edge topology is drawn.
     */
    val inputSize = graph.nodes.size
    if (inputSize == 1) {
      return singleDrawing(graph.nodes.head, graph)

    } else if (inputSize == 2) {
      val l = graph.nodes.toVector
      return dualDrawing(l(0), l(1), graph)

    } else if (inputSize == 3) {
      val l = graph.nodes.toVector
      return tripleDrawing(l(0), l(1), l(2), graph)
    }

    /* Setup the graph in step 1 to 5. */
    val dataSetup = new DataSetup(graph, settings.outerFaceSelect)

    /* Produce the regular edge labeling and derive the directed graphs from it. */
    val rel = produceREL(dataSetup, settings.regularEdgeLabelFactory)
    produceDual(rel, graph, dataSetup)
  }

  private def produceREL(data: DataSetup[VType, EType], rf: RELFactory): EdgeLabeling[RVertex[VType]] = {
    rf.produceRegularEdgeLabeling(data.embedding, data.vNorth, data.vSouth, data.vWest, data.vEast)
  }

  /**
   * Should only be called when constructing the dual from a REL inputted by the user.
   * @param rel A valid REL.
   * @return A rectangular dual based on the REL.
   */
  private def executeRELAlgorithm(rel: EdgeLabeling[RVertex[VType]]): RectangularLayout[VType, EType] = {
    assume(staticData.isDefined, "REL algorithm has not instantiated static data.")
    val dataSetup = staticData.get._1
    val staticId = staticData.get._2
    val relId = rel match {
      case rel: RegularEdgeLabeling[RVertex[VType]] => rel.id
    }
    require(relId == staticId, "The supplied regular edge labeling was not derived from the initial labeling obtained when constructing the algorithm.")
    val graph = staticData.get._3

    produceDual(rel, graph, dataSetup)
  }

  /**
   * Assigns coordinates to each vertex area based on the duals created from a REL.
   * @param rel The regular edge labeling that was produced or supplied by the user.
   * @param graph The original user graph.
   * @param data Data obtained from steps 1 to 4 in the algorithm.
   * @return A rectangular dual.
   */
  private def produceDual(rel: EdgeLabeling[RVertex[VType]], graph: Graph[VType, EType], data: DataSetup[VType, EType]): RectangularLayout[VType, EType] = {

    /* Finally, compute the duals needed to assign coordinates. */
    val factory = new DualFactory(data.vWest, data.vSouth, data.vNorth, data.vEast)

    val g1 = addT1OuterEdges(directedGraph(rel.edgesOfT1.map(e => e.from -> e.to)), data.vNorth, data.vWest, data.vSouth, data.vEast)
    val g2 = addT2OuterEdges(directedGraph(rel.edgesOfT2.map(e => e.from -> e.to)), data.vNorth, data.vWest, data.vSouth, data.vEast)

    val g1Dual: Dual[RVertex[VType]] = factory.constructDualG1(rel.embeddingOfT1, g1)
    val g2Dual = factory.constructDualG2(rel.embeddingOfT2, g2)

    val vertexList: Vector[VType] = graph.nodes.map(n => {val outer: VType = n; outer}).toVector
    val drawing = computeCoordinates(vertexList, g1Dual, g2Dual, graph, data.splitPairs, data.dummyGates)
    drawing.adjustToZero
  }

  /**
   * Produces a directed G1 or G2 from edge sets T1 or T2. Also adds the external vertices vN, vW, vS, vE.
   */
  private def directedGraph(dirEdges: Vector[(RVertex[VType], RVertex[VType])]): Graph[RVertex[VType], DiEdge] = {
    var current = Graph[RVertex[VType], DiEdge]()
    for (e <- dirEdges)
      current += e._1~>e._2

    current
  }

  /** Adds the directed edges going around the outer face to the graph constructed from T1, plus a new edge
    * South -> North.
    */
  private def addT1OuterEdges(g: Graph[RVertex[VType], DiEdge],
                             vN: RVertex[VType],
                             vW: RVertex[VType],
                             vS: RVertex[VType],
                             vE: RVertex[VType]): Graph[RVertex[VType], DiEdge] = {
    g + vS~>vN + vS~>vW + vW~>vN + vS~>vE + vE~>vN
  }

  /** Adds the directed edges going around the outer face to the graph constructed from T2, plus a new edge
    * West -> East.
    */
  private def addT2OuterEdges(g: Graph[RVertex[VType], DiEdge],
                             vN: RVertex[VType],
                             vW: RVertex[VType],
                             vS: RVertex[VType],
                             vE: RVertex[VType]): Graph[RVertex[VType], DiEdge] = {
    g + vW~>vE + vW~>vN + vN~>vE + vW~>vS + vS~>vE
  }

  /** The left/right x coordinates and the top/bottom y coordinates equals the distance from the source to the
    * left/right face of each vertex.
    */
  private def computeCoordinates(vs: Vector[VType],
                                 g1: Dual[RVertex[VType]],
                                 g2: Dual[RVertex[VType]],
                                 graph: Graph[VType, EType],
                                 splits: Vector[(RVertex[VType], Vector[RVertex[VType]])],
                                 dummySplits: Vector[RVertex[VType]]): RectangularLayout[VType, EType] = {
    val result = new ListBuffer[(VType, (Int, Int), (Int, Int))]()
    for (vertex <- vs) {
      val rv = RVertex(vertex)
      val xLeft = g1.maxDistance(g1.left(rv))
      val xRight = g1.maxDistance(g1.right(rv))
      val yLow = g2.maxDistance(g2.left(rv))
      val yHigh = g2.maxDistance(g2.right(rv))
      result += ((vertex, (xLeft, yLow), (xRight, yHigh)))
    }

    val gates = new ListBuffer[((VType, VType), (Int, Int), (Int, Int))]()
    for (s <- splits) {
      val split = s._1
      val neighbors = s._2
      require(neighbors.size == 2, "Error when splitting triangle, 3+ neighbors were produced: " + neighbors)
      val n1 = neighbors(0)
      val n2 = neighbors(1)
      val xLeft = g1.maxDistance(g1.left(split))
      val xRight = g1.maxDistance(g1.right(split))
      val yLow = g2.maxDistance(g2.left(split))
      val yHigh = g2.maxDistance(g2.right(split))
      gates += (((n1.vertex, n2.vertex), (xLeft, yLow), (xRight, yHigh)))
    }

    val dummyGates = new ListBuffer[((Int, Int), (Int, Int))]()
    for (d <- dummySplits) {
      val xLeft = g1.maxDistance(g1.left(d))
      val xRight = g1.maxDistance(g1.right(d))
      val yLow = g2.maxDistance(g2.left(d))
      val yHigh = g2.maxDistance(g2.right(d))
      dummyGates += (((xLeft, yLow), (xRight, yHigh)))
    }

    new RectangularLayout(result.toVector, gates.toVector, dummyGates.toVector, graph)
  }

  /** Special case when the input graph only has 1 vertex. */
  private def singleDrawing(v: VType, graph: Graph[VType, EType]): RectangularLayout[VType, EType] = {
    new RectangularLayout(Vector((v, (0, 0), (1, 1))), Vector(), Vector(), graph)
  }

  /** Special case when the input graph only has 2 vertices. */
  private def dualDrawing(v1: VType, v2: VType, graph: Graph[VType, EType]): RectangularLayout[VType, EType] = {
    val v1Entry = (v1, (0, 0), (1, 1))
    val v2Entry = (v2, (1, 0), (2, 1))
    new RectangularLayout(Vector(v1Entry, v2Entry), Vector(), Vector(), graph)
  }

  /** Special case when the input graph only has 3 vertices. */
  private def tripleDrawing(v1: VType, v2: VType, v3: VType, graph: Graph[VType, EType]): RectangularLayout[VType, EType] = {
    val v1Entry = (v1, (0, 0), (1, 1))
    val v2Entry = (v2, (1, 0), (2, 1))
    val v3Entry = (v3, (0, 1), (2, 2))
    new RectangularLayout(Vector(v1Entry, v2Entry, v3Entry), Vector(), Vector(), graph)
  }

}
