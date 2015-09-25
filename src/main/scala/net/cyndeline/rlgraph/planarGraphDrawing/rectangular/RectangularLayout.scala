package net.cyndeline.rlgraph.planarGraphDrawing.rectangular

import net.cyndeline.rlcommon.util.Intersection
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.dataStructure.{Gate, Rectangle}
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Contains rectangular coordinates used to draw every vertex in a graph as a rectangular dual.
 *
 * @param vs Every vertex in the original graph, followed by the start and stop (x,y) coordinates of its rectangle.
 * @param gs Every vertex pair separated by a gate, followed by the start and stop (x,y) coordinates of the gate rectangle.
 * @param ds Dummy rectangles used to keep the layout rectangular, if gates between a regular and an outer vertex
 *           exists.
 * @param graph The original graph that the layout is based on.
 */
class RectangularLayout[VType, EType[X] <: UnDiEdge[X]](vs: Vector[(VType, (Int, Int), (Int, Int))],
                                                        gs: Vector[((VType, VType), (Int, Int), (Int, Int))],
                                                        ds: Vector[((Int, Int), (Int, Int))],
                                                        val graph: Graph[VType, EType]) {

  /**
   * Constructs a rectangular layout with regular areas and gates, but no dummies.
   * @param vs Every vertex in the original graph, followed by the start and stop (x,y) coordinates of its rectangle.
   * @param gs A pair of vertices split by a gate, followed by the coordinates of the gate.
   * @param graph The original graph that the layout is based on.
   */
  def this(vs: Vector[(VType, (Int, Int), (Int, Int))],
           gs: Vector[((VType, VType), (Int, Int), (Int, Int))],
           graph: Graph[VType, EType]) = this(vs, gs, Vector(), graph)

  /**
   * Constructs a rectangular layout without gates.
   * @param vs Every vertex in the original graph, followed by the start and stop (x,y) coordinates of its rectangle.
   * @param graph The original graph that the layout is based on.
   */
  def this(vs: Vector[(VType, (Int, Int), (Int, Int))], graph: Graph[VType, EType]) = this(vs, Vector(), Vector(), graph)

  /** Vertices mapped against rectangular coordinates used to draw them. */
  val rectangles: Map[VType, Rectangle] = vs.map(vAndCords => {
    val vertex = vAndCords._1
    val start = vAndCords._2
    val stop = vAndCords._3
    vertex -> Rectangle(start._1, start._2, stop._1, stop._2)
  }).toMap

  /** Gates separating rectangles for regular vertices. */
  val gates: Vector[Gate[VType]] = for {
    gate <- gs
    vertices = gate._1
    start = gate._2
    stop = gate._3
  } yield Gate(vertices._1, vertices._2, start._1, start._2, stop._1, stop._2)

  /**
   * Rectangles that used to be gates between an inner and outer vertex, but now only exists to fill the space, keeping
   * the layouts outer shape rectangular.
   */
  val dummies: Vector[Rectangle] = ds.map(cords => {
    val start = cords._1
    val stop = cords._2
    Rectangle.dummy(start._1, start._2, stop._1, stop._2)
  })

  /** Maps rectangular areas to adjacent neighbors, as defined in the original graph. In other words, this map does
    * not contain adjacencies based on edges that were added during the dualization algorithm (i.e due to biconnection
    * or triangulation). If a gate has been added, the neighbors it connects will not be listed as adjacent here.
    * Instead, both neighbors will be adjacent to the gate instead.
    */
  val adjacentAreas: Map[Rectangle, Vector[Rectangle]] = {
    val result = new mutable.HashMap[Rectangle, Vector[Rectangle]]()

    for (vertex <- graph.nodes) {

      // Every original neighbor that is now separated by a gate
      val gatesOfVertex = gates.filter(g => vertex == g.from || vertex == g.to)
      val gatedNeighbors = gatesOfVertex.map(g => if (vertex == g.from) g.to else g.from)
      val neighbors = GraphCommons.outerNeighbors[VType, EType](vertex, graph) diff gatedNeighbors

      result.put(rectangles(vertex), neighbors.map(rectangles(_)) ++ gatesOfVertex)
    }

    for (gate <- gates)
      result.put(gate, Vector(rectangles(gate.from), rectangles(gate.to)))

    result.toMap
  }

  /** Every rectangle and gate in the layout. */
  val allAreas: Vector[Rectangle] = rectangles.values.toVector ++ gates ++ dummies

  /** The highest x coordinate in the drawing. */
  val maxX = allAreas.maxBy(_.stopX).stopX

  /** The lowest x coordinate in the drawing. */
  val minX = allAreas.minBy(_.startX).startX

  /** The highest y coordinate in the drawing. */
  val maxY = allAreas.maxBy(_.stopY).stopY

  /** The lowest y coordinate in the drawing. */
  val minY = allAreas.minBy(_.startY).startY

  /**
   * @param x Amount to adjust every x coordinate with.
   * @param y Amount to adjust every y coordinate with.
   * @return A new rectangular drawing with every x and y coordinate modified by the specified amount.
   */
  def adjustCoordinates(x: Int, y: Int): RectangularLayout[VType, EType] = {
    val updatedVertexCoordinates = vs.map(entry => {
      val vertex = entry._1
      val start: (Int, Int) = entry._2
      val stop: (Int, Int) = entry._3
      (vertex, (start._1 + x, start._2 + y), (stop._1 + x, stop._2 + y))
    })

    val updatedSplitCoordinates = gs.map(entry => {
      val vertex = entry._1
      val start: (Int, Int) = entry._2
      val stop: (Int, Int) = entry._3
      (vertex, (start._1 + x, start._2 + y), (stop._1 + x, stop._2 + y))
    })

    val updatedDummyCoordinate = ds.map(entry => {
      val start: (Int, Int) = entry._1
      val stop: (Int, Int) = entry._2
      ((start._1 + x, start._2 + y), (stop._1 + x, stop._2 + y))
    })

    new RectangularLayout(updatedVertexCoordinates, updatedSplitCoordinates, updatedDummyCoordinate, graph)
  }

  /** Adjusts the coordinates of the drawing as close to 0 as possible. */
  def adjustToZero: RectangularLayout[VType, EType] = adjustCoordinates(-minX, -minY)

  /**
   * @param a A vertex in the original graph.
   * @param b Another vertex in the original graph, sharing an edge with 'a.
   * @return A copy of this layout with the input graph no longer containing the edge a~b.
   */
  def removeAdjacency(a: VType, b: VType): RectangularLayout[VType, EType] = {
    val edge = graph.get(a).edges.find(e => GraphCommons.oppositeVertex(a, e.toOuter) == b).getOrElse {
      throw new NoSuchElementException("The edge from " + a  + " to " + b + " did not exist in " + graph)
    }
    new RectangularLayout(vs, gs, ds, graph - edge)
  }

  /**
   * If this layout contains dummy rectangles on the outer face that shares both segments on one axis with
   * another regular (non-gate, non-dummy) rectangle, they are merged together. If both axis's can be used,
   * the x-axis is prioritized.
   * @return A copy of this layout with as many dummy rectangles as possible merged with regular rectangles.
   */
  def mergeDummyRectangles: RectangularLayout[VType, EType] = {

    // the first element is the dummy, the second the vertex
    val mergePairX = new ListBuffer[(Rectangle, Rectangle)]()
    val mergePairY = new ListBuffer[(Rectangle, Rectangle)]()

    for (d <- dummies; other <- allAreas if other.isVertex && Intersection(d.startX, d.startY, d.stopX, d.stopY, other.startX, other.startY, other.stopX, other.stopY).intersects) {
      if (d.startX == other.startX && d.stopX == other.stopX) {
        mergePairY += ((d, other))
      } else if (d.startY == other.startY && d.stopY == other.stopY) {
        mergePairX += ((d, other))
      }
    }

    val rToV = rectangles.map(_.swap)
    val removedDummyEntries = new ListBuffer[((Int, Int), (Int, Int))]()
    val removedVertexEntries = new ListBuffer[(VType, (Int, Int), (Int, Int))]()
    val addedVertexEntries = new ListBuffer[(VType, (Int, Int), (Int, Int))]()

    /* If one dummy has multiple rectangle candidates, pick the smallest one. */
    val candidates = mapMergeCandidates(mergePairX, mergePairY)
    val mergedRectangles = new mutable.HashSet[Rectangle]()

    for (dummyWithCandidates <- candidates) {
      val dummy = dummyWithCandidates._1
      val candidates = dummyWithCandidates._2

      // Resolve conflicts if multiple dummies chose the same rectangles to merge with
      candidates.x_candidates --= mergedRectangles
      candidates.y_candidates --= mergedRectangles

      val vsEntry: Option[(VType, (Int, Int), (Int, Int))] = if (!candidates.x_candidates.isEmpty) {
        val smallestCandidate = candidates.x_candidates.minBy(r => (r.stopX - r.startX) * (r.stopY - r.startY))
        mergedRectangles += smallestCandidate
        Some((rToV(smallestCandidate), (smallestCandidate.startX, smallestCandidate.startY), (smallestCandidate.stopX, smallestCandidate.stopY)))

      } else if (!candidates.y_candidates.isEmpty) {
        val smallestCandidate = candidates.y_candidates.minBy(r => (r.stopX - r.startX) * (r.stopY - r.startY))
        mergedRectangles += smallestCandidate
        Some((rToV(smallestCandidate), (smallestCandidate.startX, smallestCandidate.startY), (smallestCandidate.stopX, smallestCandidate.stopY)))

      } else {
        None
      }

      if (vsEntry.isDefined) {
        val entry = vsEntry.get
        val dummyEntry = ds.find(d => d._1 == (dummy.startX, dummy.startY) && d._2 == (dummy.stopX, dummy.stopY))
          .getOrElse(throw new Error("No entry for dummy " + dummy + " found during merge optimization."))
        val rectangleEntry = vs.find(v => v._2 == entry._2 && v._3 == entry._3)
          .getOrElse(throw new Error("No entry for rectangle " + entry + " found during merge optimization."))

        removedDummyEntries += dummyEntry
        removedVertexEntries += rectangleEntry

        val newRectangleStart = (Math.min(dummy.startX, entry._2._1), Math.min(dummy.startY, entry._2._2))
        val newRectangleStop = (Math.max(dummy.stopX, entry._3._1), Math.max(dummy.stopY, entry._3._2))
        addedVertexEntries += ((entry._1, newRectangleStart, newRectangleStop))
      }
    }

    new RectangularLayout((vs diff removedVertexEntries.toVector) ++ addedVertexEntries.toVector, gs, ds diff removedDummyEntries.toVector, graph)
  }

  private def mapMergeCandidates(x: ListBuffer[(Rectangle, Rectangle)], y: ListBuffer[(Rectangle, Rectangle)]): Map[Rectangle, MergeCandidates] = {
    val m = new mutable.HashMap[Rectangle, MergeCandidates]()

    for (xCandidate <- x) {
      val currentCandidates = m.get(xCandidate._1).getOrElse {
        val c = new MergeCandidates()
        m += xCandidate._1 -> c
        c
      }

      currentCandidates.x_candidates += xCandidate._2
    }

    for (yCandidate <- y) {
      val currentCandidates = m.get(yCandidate._1).getOrElse {
        val c = new MergeCandidates()
        m += yCandidate._1 -> c
        c
      }

      currentCandidates.y_candidates += yCandidate._2
    }

    m.toMap
  }


  private val str: String = {
    val builder = new StringBuilder()
    val nl = System.getProperty("line.separator")
    builder ++= "Rectangular dual coordinates:" + nl
    for (v <- rectangles) {
      builder ++= v._1.toString + " | " + v._2 + nl
    }
    for (g <- gates) {
      builder ++= "Gate: " + g.toString
    }

    builder.toString()
  }

  override def toString: String = str

  override def equals(other: Any): Boolean = other match {
    case rl: RectangularLayout[VType, EType] => rl.rectangles == rectangles && rl.gates == gates && adjacentAreas == rl.adjacentAreas
    case _ => false
  }

  override val hashCode: Int = rectangles.## ^ gates.##

  private class MergeCandidates {
    val x_candidates = new ListBuffer[Rectangle]()
    val y_candidates = new ListBuffer[Rectangle]()
  }
}
