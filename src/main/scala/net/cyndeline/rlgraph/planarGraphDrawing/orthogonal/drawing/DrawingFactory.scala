package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.drawing

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.SegmentOrientation._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.{DefaultDart, SimpleDart}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Parses a set of segments with assigned coordinates, and a set of darts, into an orthogonal drawing.
 *
 * @constructor Creates a new drawing factory.
 */
class DrawingFactory {

  /**
   * Computes a drawing based on a set of default or split darts, as well as a set of segments with
   * assigned coordinates.
   * @param segments Segments with coordinates. Every vertex must be present in one vertical and one horizontal segment.
   * @param darts One upward and one sidewards pointing dart for each edge in the simplified drawing. Dummy vertices
   *              are allowed, but not artificial edges and vertices.
   * @tparam VType Type of vertex in the original graph.
   * @return a drawing containing coordinated for every vertex, and bend-coordinated for all bends between them, if any.
   */
  def draw[VType, EType[X] <: UnDiEdge[X]](graph: Graph[VType, EType], segments: Set[Segment[VType]], darts: Set[Dart[VType]]): OrthogonalLayout[VType, EType] = {
    val mappedCoordinates: Map[VertexWrapper[VType], (Int, Int)] = mapCoordinates(segments)
    val drawnEdges = new ArrayBuffer[DrawnEdge[VType, EType]]()
    var remainingDarts = darts

    while (!remainingDarts.isEmpty) {
      val arbitraryDart = remainingDarts.head

      arbitraryDart match {

        /* Default edges can be drawn without checking for bends. */
        case d: DefaultDart[VType] => {
          val from = d.from
          val to = d.to
          val fromC: (Int, Int) = mappedCoordinates(from)
          val toC: (Int, Int) = mappedCoordinates(to)
          drawnEdges += new DrawnEdge[VType, EType](from.value.get, to.value.get, fromC, toC, findEdge(from.value.get, to.value.get, graph))

          remainingDarts -= d
        }

        /* If it's a simple edge, find all simple edges with the same original dart, and order them into the
         * vertex chain they'll be drawn as.
         */
        case s: SimpleDart[VType] => {

          /* Since each dart comes in a pair, a split dart with A->B as its original shares the same original as the
           * ones with B->A. And since only up/right-wards darts are used in the drawing, not every dart will have
           * its original pointing the same direction.
           */
          val allSimples: Set[Dart[VType]] = remainingDarts.filter(d => {
            val underLying: DefaultDart[VType] = findUnderlyingDefaultDart(d)
            val original = s.originalDart
            (underLying.from == original.from && underLying.to == original.to) || (underLying.to == original.from && underLying.from == original.to)
          })
          val vertexChain = makeDartChain(allSimples)
          val coordinates = new ArrayBuffer[(Int, Int)]()

          for (vertex <- vertexChain) {
            coordinates += mappedCoordinates(vertex)
          }

          val from = vertexChain.head.value
          val to = vertexChain.last.value
          val bendCoordinates = coordinates.take(coordinates.size - 1).drop(1)
          drawnEdges += new DrawnEdge[VType, EType](from.get, to.get, coordinates.head, coordinates.last, bendCoordinates.toVector, findEdge(from.get, to.get, graph))

          remainingDarts = remainingDarts -- allSimples
        }

        case _ => throw new IllegalArgumentException("Only default and simple dart can be used to construct drawing, not " + arbitraryDart.getClass)

      }
    }

    val vertexCoordinates: Vector[(VType, Int, Int)] = for {
      vertex <- mappedCoordinates.toVector
      if vertex._1.isDefined
    } yield (vertex._1.value.get, vertex._2._1, vertex._2._2)

    /* Make all coordinates positive if needed. */
    val lowestCoordinates = findLowestNegativeCoordinate(drawnEdges)
    val edgesWithAdjustedCoordinates = if (lowestCoordinates.isDefined)
      makeEdgeCoordinatesPositive(drawnEdges, graph, lowestCoordinates.get._1, lowestCoordinates.get._2)
    else
      drawnEdges

    val verticesWithAdjustedCoordinates = if (lowestCoordinates.isDefined)
      makeVertexCoordinatesPositive(vertexCoordinates, lowestCoordinates.get._1, lowestCoordinates.get._2)
    else
      vertexCoordinates

    new OrthogonalLayout[VType, EType](edgesWithAdjustedCoordinates.toVector, verticesWithAdjustedCoordinates)
  }

  /**
   * Computes coordinates for every vertex based on segments.
   */
  private def mapCoordinates[VType](segments: Set[Segment[VType]]) = {
    val horizontalSegments = segments.filter(s => s.orientation == Horizontal)
    val verticalSegments = segments -- horizontalSegments

    val vertices = (for {
      segment <- segments
      v <- segment.vertices
    } yield v).toSet

    val coordinates = for {
      vertex <- vertices
      dummySegment = new Segment(Horizontal).newCoordinate(0) // Used to return coordinate 0 if no segment present, orientation doesn't matter
      x = verticalSegments.find(s => s.contains(vertex)).getOrElse(dummySegment).coordinate
      y = horizontalSegments.find(s => s.contains(vertex)).getOrElse(dummySegment).coordinate
    } yield (vertex -> (x, y))

    coordinates.toMap
  }

  private def findUnderlyingDefaultDart[VType, EType[X] <: UnDiEdge[X]](dart: Dart[VType]): DefaultDart[VType] = dart match {
    case d: DefaultDart[VType] => d
    case s: SimpleDart[VType] => findUnderlyingDefaultDart(s.originalDart)
    case _ => throw new IllegalArgumentException("Artificial and split darts should have been removed before drawing graph.")
  }

  /**
   * Sorts a set of darts into a vertex chain based on their vertices.
   * @param darts Simple darts to sort. Must have vertices that allows a chain to be formed.
   */
  private def makeDartChain[VType, EType[X] <: UnDiEdge[X]](darts: Set[Dart[VType]]): Vector[VertexWrapper[VType]] = {

    // Simple darts should always come in sets of at least two
    if (darts.size < 2)
      throw new IllegalArgumentException("At least two simple darts needed to sort into a chain (currently " + darts.size + ": " + darts + ")")

    /* Even though a chain should exist in the supplied darts, there's no telling if a dart belongs to a particular
     * dart or its opposite. As such, the chain starts at an arbitrary defined vertex (there should only be two in
     * any given simple chain of darts.
     */
    val hasDefinedVertices = darts.filter(d => d.from.isDefined || d.to.isDefined)

    if (hasDefinedVertices.size > 2 || hasDefinedVertices.size < 2)
      throw new IllegalArgumentException("The number of darts with defined vertices must number exactly two, currently: " + hasDefinedVertices)

    var remainingDarts = darts
    var latestDart = hasDefinedVertices.head
    var current: VertexWrapper[VType] = if (latestDart.from.isDefined) latestDart.from else latestDart.to
    var chain = Vector(current)

    while (!remainingDarts.isEmpty) {
      current = if (chain.last == latestDart.to) latestDart.from else latestDart.to
      remainingDarts -= latestDart
      chain = chain :+ current

      if (!remainingDarts.isEmpty)
        latestDart = remainingDarts.find(d => d.to == current || d.from == current).get
    }

    chain
  }

  /**
   * Adds the minimum amount needed to each axis to make the lowest edge coordinate(s) 0.
   */
  private def makeEdgeCoordinatesPositive[VType, EType[X] <: UnDiEdge[X]](edges: ArrayBuffer[DrawnEdge[VType, EType]],
                                                                      graph: Graph[VType, EType],
                                                                      lowestX: Int,
                                                                      lowestY: Int): ArrayBuffer[DrawnEdge[VType, EType]] = {
    val lowestXCoordinate = Math.abs(lowestX)
    val lowestYCoordinate = Math.abs(lowestY)

    val edgesWithPositiveCoordinated = edges.map(edge => {
      val startPos = (edge.startPos._1 + lowestXCoordinate, edge.startPos._2 + lowestYCoordinate)
      val stopPos = (edge.stopPos._1 + lowestXCoordinate, edge.stopPos._2 + lowestYCoordinate)
      val bends = edge.bends.map(bend => (bend._1 + lowestXCoordinate, bend._2 + lowestYCoordinate))

      new DrawnEdge[VType, EType](edge.start, edge.stop, startPos, stopPos, bends, findEdge(edge.start, edge.stop, graph))
    })

    edgesWithPositiveCoordinated
  }

  private def makeVertexCoordinatesPositive[VType](vertices: Vector[(VType, Int, Int)], lowestX: Int, lowestY: Int): Vector[(VType, Int, Int)] = {
    for { vertex <- vertices } yield (vertex._1, vertex._2 + Math.abs(lowestX), vertex._3 + Math.abs(lowestY))
  }

  /**
   * Finds an edge between a and b in a graph.
   */
  private def findEdge[VType, EType[X] <: UnDiEdge[X]](a: VType, b: VType, graph: Graph[VType, EType]): EType[VType] = {
    val aInner = graph.get(a)
    val aOutgoing = aInner.outgoing
    val ab = aOutgoing.find(e => e._2 == b)

    if (!ab.isEmpty) {
      ab.get.toOuter
    } else {
      val bInner = graph.get(b)
      val bOutgoing = bInner.outgoing
      val ba = bOutgoing.find(e => e._2 == a).getOrElse {
        throw new Error("No edge found going between " + a + " and " + b + " in " + graph)
      }.toOuter

      ba
    }
  }

  /**
   * Finds the lowest coordinate in the drawing, including negative ones. Only edges are needed since they contain
   * all vertices. Returns None if no negative coordinates are found. If no negative coordinate is found on
   * only one axis, 0 is returned for that axis.
   */
  private def findLowestNegativeCoordinate[VType, EType[X] <: UnDiEdge[X]](edges: ArrayBuffer[DrawnEdge[VType, EType]]): Option[(Int, Int)] = {
    var lowestXCoordinate = 0
    var lowestYCoordinate = 0

    edges.foreach(edge => {
      lowestXCoordinate = Math.min(lowestXCoordinate, edge.startPos._1)
      lowestYCoordinate = Math.min(lowestYCoordinate, edge.startPos._2)
      lowestXCoordinate = Math.min(lowestXCoordinate, edge.stopPos._1)
      lowestYCoordinate = Math.min(lowestYCoordinate, edge.stopPos._2)

      for (bend <- edge.bends) {
        lowestXCoordinate = Math.min(lowestXCoordinate, bend._1)
        lowestYCoordinate = Math.min(lowestYCoordinate, bend._2)
      }
    })

    if (lowestXCoordinate >= 0 && lowestYCoordinate >= 0)
      None
    else
      Some((lowestXCoordinate, lowestYCoordinate))
  }
}
