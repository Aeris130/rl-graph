package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic.VisibilityGraphFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.{SegmentFactory, ShapeGraphFactory}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.drawing.{DrawingFactory, OrthogonalLayout}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.DualMinimumCostFlow.DualMinimumCostFlow
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.operation.OrthoRepresentationComputation
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{OrthogonalDartRep, VertexWrapperFactory}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.WLkDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes an orthogonal drawing based on an undirected planar graph.
 *
 * The algorithm uses dual-minimum cost flows to compute a drawing with minimum edge length, rather than
 * minimum width/height of the drawing.
 *
 * @constructor Creates a new drawing computation.
 */
class OrthogonalLayoutAlgorithm[VType : TypeTag : ClassTag, EType[X] <: UnDiEdge[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l : ({type l[M[_]] = ClassTag[M[VType]]})#l] extends Orthogonalizer[VType, EType] {
  private val shapeFactory = new ShapeGraphFactory()
  private val dartDirections = new DirectionPointer()
  private val segmentFactory = new SegmentFactory()
  private val dualMinimumCostFlow = new DualMinimumCostFlow()
  private val drawFactory = new DrawingFactory()

  /* This visibility factory is instantiated using an ordering opposite to the default. The reason is that the
   * dual minimum cost flow uses the sink as starting position (distance 0) when computing coordinates. The sink
   * will then become the lowest neighbor the next time a visibility graph is computed during the final
   * improvement heuristic loop, and thus be given the highest coordinate. This then loops infinitely.
   *
   * By making arcs in the visibility graph point from highest to lowest, the sink will once again become the sink on
   * subsequent loops, and not have its coordinate modified unless the segments can be optimized.
   */
  private val visibilityFactory = new VisibilityGraphFactory[VType](Ordering.fromLessThan[Segment[VType]](_.coordinate > _.coordinate))

  /**
   * Computes an orthogonal drawing.
   * @param graph A connected planar graph with vertex degree 4 or lower.
   * @return an orthogonal drawing of the graph.
   */
  def orthogonalize(graph: Graph[VType, EType]): OrthogonalLayout[VType, EType] = {
    if (graph.isEmpty)
      return new OrthogonalLayout() // Empty

    if (graph.nodes.exists(_.degree > 4)) {
      throw new IllegalArgumentException("Vertex " + graph.nodes.find(_.degree > 4) + " has more than 4 connected edges.")
    }

    if (!graph.isConnected)
      throw new IllegalArgumentException("The supplied graph was not connected.")

    /* Special case: A graph with a single vertex and no edges doesn't need to run any algorithms, a drawing with
     * the single vertex mapped to the coordinate (0, 0) is enough.
     */
    if (graph.size == 1)
      return singleVertexDrawing(graph)

    /* These must be declared anew every time a graph is orthogonalized, otherwise the id's in the wrapper
     * factory won't reset.
     */
    val wrapperFactory = new VertexWrapperFactory[VType]()
    val orthoRepComp = new OrthoRepresentationComputation[VType, EType](wrapperFactory)
    val simplifier = new OrthogonalFaceSimplifier(wrapperFactory)
    val refiner = new RectangleRefiner(wrapperFactory)

    /* Compute orthogonal representation */
    val representation: OrthogonalDartRep[VType] = orthoRepComp.computeRepresentation(graph)

    /* Simplify representation */
    val simplifiedRep: OrthogonalDartRep[VType] = simplifier.simplifyFaces(representation)
    val directionDart = simplifiedRep.externalFace.darts.head // Used later on as basis for directions

    /* Make the representation rectangular */
    val rectangularFaces: OrthogonalDartRep[VType] = refiner.refineFaces(simplifiedRep)

    /* Compute shape graph.
     *
     * Since every dart in the simple representation may have been split, find a split dart with the same
     * original as the directionDart from the simple rep (unless the directionDart is still present that is).
     * This is so that the directions later computed using the simple rep are identical to the rectangular faces.
     */
    val rectStart = rectangularFaces.allDarts.find(d => !d.isBasedOnArtificialDart && {
      val original = d.findSimpleOriginal
      original.from == directionDart.from && original.to == directionDart.to
    }).get

    val directedDartRep = dartDirections.computeDirection[VType](rectangularFaces, rectStart, Right)
    val directedDarts = directedDartRep.allDarts.toSet
    val segments = segmentFactory.computeSegments(directedDarts)
    val shapeGraphs: (Graph[Segment[VType], WLkDiEdge], Graph[Segment[VType], WLkDiEdge]) = shapeFactory.makeShapeGraphs(directedDarts, segments)

    /* Compute coordinates for segments in shape graph */
    val horizontalAssignment: Set[Segment[VType]] = if (!shapeGraphs._1.isEmpty) {
      dualMinimumCostFlow.assignSegmentCoordinates(shapeGraphs._1)
    } else {
      Set()
    }
    val verticalAssignment: Set[Segment[VType]] = if (!shapeGraphs._2.isEmpty) {
      dualMinimumCostFlow.assignSegmentCoordinates(shapeGraphs._2)
    } else {
      Set()
    }

    /* Compute new segments based on the simplified dart representation (i.e using the faces as they were before
     * the refiner added artificial darts and vertices) and add coordinates to them based on coordinates from the
     * shape graph.
     */
    val directedOriginalRep = dartDirections.computeDirection[VType](simplifiedRep, directionDart, Right)
    val directedOriginalDarts = directedOriginalRep.allDarts.toSet
    val nonRectangularSegments = segmentFactory.computeSegments(directedOriginalDarts)
    val horizontalSegments = nonRectangularSegments.filter(s => s.orientation == Horizontal)
    val verticalSegments = nonRectangularSegments -- horizontalSegments

    var finalHorizontalSegments: Set[Segment[VType]] = assignCoordinates(horizontalSegments, horizontalAssignment)
    var finalVerticalSegments: Set[Segment[VType]] = assignCoordinates(verticalSegments, verticalAssignment)
    var previousHorizontalSegments = Set[Segment[VType]]()
    var previousVerticalSegments = Set[Segment[VType]]()

    /* Compute visibility graphs and run dual-minimum optimization on both graphs until no more improvement
     * can be made (that is, until the coordinates of the resulting segments matches the coordinates that
     * the visibility graph was originally made from).
     *
     * Since optimization in one orientation can allow or obstruct optimization in the other, the visibility
     * graph of the second orientation can only be computed after the opposing segments has had their coordinates
     * computed.
     */
    do {
      previousHorizontalSegments = finalHorizontalSegments
      previousVerticalSegments = finalVerticalSegments

      val horizontalVisibilityGraph = visibilityFactory.buildSingleVisibilityGraph(previousVerticalSegments ++ previousHorizontalSegments, Horizontal)
      finalHorizontalSegments = if (!horizontalVisibilityGraph.isEmpty) {
        dualMinimumCostFlow.assignSegmentCoordinates(horizontalVisibilityGraph)
      } else {
        finalHorizontalSegments // No change
      }

      // Uses finalHorizontalSegments
      val verticalVisibilityGraph = visibilityFactory.buildSingleVisibilityGraph(previousVerticalSegments ++ finalHorizontalSegments, Vertical)
      finalVerticalSegments = if (!verticalVisibilityGraph.isEmpty) {
        dualMinimumCostFlow.assignSegmentCoordinates(verticalVisibilityGraph)
      } else {
        finalHorizontalSegments // No change
      }

    } while (segmentCoordinatesDiffer(previousHorizontalSegments, finalHorizontalSegments)
              || segmentCoordinatesDiffer(previousVerticalSegments, finalVerticalSegments))

    /* Compute the drawing based on the visibility graphs segments. */
    val dartToDraw = directedOriginalRep.allDarts.toSet.filter(d => d.direction == Option(Up) || d.direction == Option(Right))
    val finalDrawing = drawFactory.draw[VType, EType](graph, finalHorizontalSegments ++ finalVerticalSegments, dartToDraw)

    finalDrawing
  }

  /**
   * Assigns coordinates to a set of segments using other segments with the same alignment and whose vertices
   * are a super-set of the other segment set.
   * @param to Segments to assign coordinates to.
   * @param from Super-set of 'to (every segment has an equivalent segment in 'to with a subset of its vertices)
   * @return the set 'to wot coordinates assigned.
   */
  private def assignCoordinates(to: Set[Segment[VType]], from: Set[Segment[VType]]): Set[Segment[VType]] = {
    if (from.exists(s => s.orientation != to.head.orientation))
      throw new IllegalArgumentException("Segment 'to and 'from must have the same alignment.")

    val result = new ArrayBuffer[Segment[VType]]()
    val segments = to.iterator
    while (segments.hasNext) {
      val s = segments.next()
      val vertices = s.vertices.toSet
      val sWithCoordinate = from.find(s => vertices subsetOf s.vertices.toSet)

      /* If no segment is found, it means the graph only had darts in one orientation. The other orientation
       * has coordinate 0 by default.
       */
      if (sWithCoordinate.isDefined)
        result += s.newCoordinate(sWithCoordinate.get.coordinate)
      else
        result += s.newCoordinate(0)

    }

    result.toSet
  }

  /**
   * Checks if two sets containing segments with the same vertices differ in their coordinates.
   * @param a A set of segment.
   * @param b The same segments as in a, but with (potentially) other coordinates.
   * @return True if at least one segment in one of the sets has a different coordinate than its counterpart.
   */
  private def segmentCoordinatesDiffer(a: Set[Segment[VType]], b: Set[Segment[VType]]): Boolean = {
    for (segment <- a) {
      val otherSegment = b.find(s => s.vertices == segment.vertices).get
      if (segment.coordinate != otherSegment.coordinate)
        return true
    }

    false
  }

  /**
   * Produces a drawing containing the first vertex in a graph, giving it coordinate 0. Used on graphs with 1 vertex.
   * @param graph Graph with a single vertex.
   * @return a drawing with the single vertex having coordinate 0, 0.
   */
  private def singleVertexDrawing(graph: Graph[VType, EType]): OrthogonalLayout[VType, EType] = {
    new OrthogonalLayout[VType, EType](Vector(), Vector((graph.nodes.head, 0, 0)))
  }
}
