package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.face.{FaceComputation, FaceMembershipManager}
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.{AngularMap, AngularVertex}
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, LabelEdge}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * For a more in-depth study of alternating 4-cycles and their applications in REL combinatorics, see the thesis
 * "Combinatoire des cartes planaires et applications algorithmiques" by Eric Fusy (there referred to as transversal
 * structures).
 *
 * This class computes the essential clockwise circuits of a planar embedding with a regular edge labeling attached
 * to it. An alternating 4-cycle is such a circuit where every edge in the cycle alternates between the sets T1 and T2.
 * Flipping a cycle means changing the set (from T1 to T2 or vice versa) for each edge present inside the cycle
 * (but not on it).
 *
 * An alternating 4-cycle can either be left- or right alternating. A left alternating cycle is one where the interior
 * edges of the cycle C connected to a vertex V on the cycle belongs to the same set (T1/T2) as the cycle-edge to the
 * left of them. A right alternating cycle is the same but for the right cycle-edge of V.
 *
 * @param regularEdgeLabeling The edge labeling that represents the current edge orientations of the 4-cycles in this
 *                            collection.
 * @param allCycles Every essential alternating 4-cycle in the regular edge labeling.
 */
class AlternatingFourCycles[V : TypeTag : ClassTag] private (val regularEdgeLabeling: EdgeLabeling[V],
                                                             val allCycles: Vector[FourCycle[V]]) {

  /**
   * Changes the T-set membership for every interior edge in the 4-cycle collection, and updates the regular edge
   * labeling associated with it.
   * @param c Cycle to flip.
   * @return A new 4-cycle collection with the cycle flipped.
   */
  def flipCycle(c: FourCycle[V]): AlternatingFourCycles[V] = {
    val index = c match {
      case ck: EssentialFourCycle[V] => ck.index
    }
    require(allCycles.isDefinedAt(index), "The supplied cycle " + c + " it not defined in this 4-cycle collection.")

    /* Some of the edges needs to be reversed. If any edge in a T-set X (before the flip) ends in an outer vertex
     * that lies at the end of an edge belonging to the opposite T-set Y, there's no need to change orientation
     * on edges belonging to X.
     */
    val outerRelEdges = c.outerEdges.map(regularEdgeLabeling.edges)
    val innerRelEdges = c.innerEdges.map(regularEdgeLabeling.edges)
    val outerT1 = outerRelEdges.filter(regularEdgeLabeling.isMemberOfT1)
    val outerT2 = outerRelEdges.filter(regularEdgeLabeling.isMemberOfT2)
    val innerT1 = innerRelEdges.filter(regularEdgeLabeling.isMemberOfT1).map(_.to).toSet
    val innerT2 = innerRelEdges.filter(regularEdgeLabeling.isMemberOfT2).map(_.to).toSet

    val t1Flip = !outerT2.exists(e => innerT1.contains(e.to))
    val t2Flip = !outerT1.exists(e => innerT2.contains(e.to))

    val reversed = c.innerEdges.map(e => {
      val edge = regularEdgeLabeling.edges(e)

      if (regularEdgeLabeling.isMemberOfT1(edge))
        t1Flip
      else
        t2Flip
    })

    val edgeLabeling = regularEdgeLabeling.flipIndexEdges(c.innerEdges, reversed)
    new AlternatingFourCycles(edgeLabeling, allCycles.updated(index, c.flip))
  }

  /** @return True if no 4-cycles existed in the REL, otherwise false. */
  def isEmpty: Boolean = allCycles.isEmpty

}

/**
 * Factory object used to instantiate an initial four cycle collection.
 */
object AlternatingFourCycles {

  /**
   * @param rel A regular edge labeling to parse all essential alternating 4-cycles from. This labeling must be minimal
   *            (i.e every alternating 4-cycle in it must be left-alternating).
   * @tparam V Vertex type in the edge labeling.
   * @return A collection of every essential alternating 4-cycle in the labeling.
   */
  def apply[V : TypeTag : ClassTag](rel: EdgeLabeling[V]): AlternatingFourCycles[V] = {
    val angles = new AngularMap(rel)
    val cycleValidation = new CycleVerifier()
    val fourCycles: Vector[Cycle[V]] = new FourCycleFinder[V]().findAlternatingFourCycles(rel)
    val faces = new FaceComputation[V]().computeFaces(rel.originalEmbedding)
    val fManager: FaceMembershipManager[V] = new FaceMembershipManager(faces)
    val regularToAngular = angles.angularEmbedding.embeddedVertices.filter(_.isBlack).map(bv => bv.vertex -> bv).toMap

    val faceCycles = computeRectangularFaceCycles(angles, cycleValidation, rel)
    val originalFaceCycles: Vector[Cycle[V]] = for (c <- faceCycles) yield findOriginalFaceCycle(c.angularCycle, angles)
    val potentialEightCycles = fourCycles diff (originalFaceCycles ++ originalFaceCycles.map(_.reverse)).map(_.makeUndirected)
    require(potentialEightCycles.size == fourCycles.size - faceCycles.size, "One or more face-cycles were not removed from the 4-cycle candidates.")
    val eightCycles = computeEighthCycles(potentialEightCycles, angles, regularToAngular, fManager, rel)

    val allCycles = faceCycles ++ eightCycles
    require(allCycles.size == fourCycles.size, "4-cycles found that didn't validate. Handle this case.")
    val disjoint: Vector[EdgeData[V]] = new DisjointCycles().disjointInnerEdges(allCycles.map(data => EdgeData(data.outer, data.inner)))
    val disjoint4Cycles = disjoint.zipWithIndex.map(d => {
            val index = d._2
            EssentialFourCycle(d._1.outer, d._1.inner, index, rel, angles)
          })

    new AlternatingFourCycles(rel, disjoint4Cycles)
  }

  // Temporarily stores cycle data.
  private case class CycleData[V](outer: Vector[LabelEdge[V]], inner: Vector[LabelEdge[V]], angularCycle: Cycle[AngularVertex[V]])

  private def computeRectangularFaceCycles[V : TypeTag : ClassTag](map: AngularMap[V],
                                                                  cycleValidation: CycleVerifier,
                                                                  rel: EdgeLabeling[V]): Vector[CycleData[V]] = {
    val faces = new FaceComputation[AngularVertex[V]]().computeFaces(map.angularEmbedding).filter(_.vertexSize == 4)
    val validFourCycles = new ListBuffer[CycleData[V]]()

    for (f <- faces) {
      val blackVertices = f.vertices.filter(_.isBlack)
      val validCycle: Option[Cycle[AngularVertex[V]]] = if (cycleValidation.verifyFaceCycle(f.cycle, map))
        Some(f.cycle)
      else if (cycleValidation.verifyFaceCycle(f.cycle.reverse, map)) {
        Some(f.cycle.reverse)
      } else {
        None
      }

      if (validCycle.isDefined) {
        val cycle = validCycle.get.vertices
        val centerEdge = findCenterEdge(blackVertices(0).vertex, blackVertices(1).vertex, rel)
        val blackWhitePairs = cycle zip (cycle.drop(1) :+ cycle.head) // Alternates between B/W and W/B
        val outerRelEdges = ListBuffer[LabelEdge[V]]()

        for (bw <- blackWhitePairs) {
          val black = if (bw._1.isBlack) bw._1 else bw._2
          val secondBlack = blackVertices.find(_ != black).get
          outerRelEdges += findOuterFaceEdge(bw._1, bw._2, secondBlack, map, rel)
        }

        validFourCycles += CycleData(outerRelEdges.toVector, Vector(centerEdge), validCycle.get)
      }
    }

    validFourCycles.toVector
  }

  /**
   * @param cycles The cycles that remain after the face-cycles has been removed.
   */
  private def computeEighthCycles[V](cycles: Vector[Cycle[V]],
                                   map: AngularMap[V],
                                   regularToAngular: Map[V, AngularVertex[V]],
                                   faceManager: FaceMembershipManager[V],
                                   rel: EdgeLabeling[V]): Vector[CycleData[V]] = {
    val result = new ListBuffer[CycleData[V]]()

    for (c <- cycles) {
      val cycle = if (eightCycleIsValid(c, map, regularToAngular, faceManager))
        c
      else if (eightCycleIsValid(c.reverse, map, regularToAngular, faceManager))
        c.reverse
      else
        throw new Error("Alternating 8-cycle detected that doesn't validate. Handle this case.")

      val angularCycle = new ListBuffer[AngularVertex[V]]()

      for (edge <- cycle.edges) {
        angularCycle += regularToAngular(edge._1)
        angularCycle += map.faceVertex(faceManager.leftFace(edge))
      }

      val innerEdges = parseInnerEdges(cycle.edges, cycle.vertices.toSet, rel, false) // All computed edges are left-alternating / cc
      val outerRelEdges = for {
          edge <- cycle.edges
          pair = Set(edge._1, edge._2)
          rEdge = rel.edges.find(e => Set(e.from, e.to) == pair).getOrElse(throw new Error("No REL edge " + edge + " found."))
        } yield rEdge
      
      result += CycleData[V](outerRelEdges, innerEdges, Cycle[AngularVertex[V]](angularCycle.toVector))
    }

    result.toVector
  }

  /*
   * Rectangular face help methods
   */

  /**
   * @param from A white or black vertex in the quadrangular face.
   * @param to The next black (white) vertex if 'from is white (black).
   * @param secondBlack Given a W/B edge, this is the second black vertex in the face.
   * @return The edge in the 4-cycle that is connected to the third edge of the white vertex.
   */
  private def findOuterFaceEdge[V](from: AngularVertex[V],
                                  to: AngularVertex[V],
                                  secondBlack: AngularVertex[V],
                                  map: AngularMap[V],
                                  rel: EdgeLabeling[V]): LabelEdge[V] = {
    require(from.isBlack != to.isBlack, "A white and a black vertex is needed to infer the outer edge on a quadrangular angular face.")
    val white = if (from.isWhite) from else to
    val black = if (from.isBlack) from else to
    val thirdBlack = map.angularEmbedding.neighborsOf(white).find(n => n != black && n != secondBlack).get

    /* There's no way to tell if the black vertex should be first or last in the edge, it differs depending on
     * weather the current angular edge is moving against or along the directed REL edges.
     */
    val blackPair = Set(black.vertex, thirdBlack.vertex)
    rel.edges.find(e => Set(e.from, e.to) == blackPair).get
  }

  private def findCenterEdge[V](from: V, to: V, rel: EdgeLabeling[V]): LabelEdge[V] = rel.edges.find(e => Set(e.from, e.to) == Set(from, to)).getOrElse {
    throw new NoSuchElementException("Could not find an inner edge " + from + " to " + to + ".")
  }

  private def findOriginalFaceCycle[V](cycle: Cycle[AngularVertex[V]], map: AngularMap[V]): Cycle[V] = {
    val oCycle = new ListBuffer[V]()

    // For each edge with a white vertex first, add the vertex that lies clockwise of the black vertex (only holds if
    // the cycle is left-alternating), then add the black vertex.
    for (e <- cycle.edges if e._2.isBlack) {
      val entry = map.angularEmbedding.embeddingFor(e._1).entryFor(e._2).next
      oCycle += entry.adjacentVertex.vertex
      oCycle += e._2.vertex
    }

    Cycle(oCycle.toVector)
  }

  /*
   * Eight-cycle help methods
   */

  private def eightCycleIsValid[V](cycle: Cycle[V], map: AngularMap[V], regularToAngular: Map[V, AngularVertex[V]], faceManager: FaceMembershipManager[V]): Boolean = {
    val whiteVertices = (for (e <- cycle.edges) yield map.faceVertex(faceManager.leftFace(e))).toSet

    for (e <- cycle.edges) {
      val leftFace = faceManager.leftFace(e)
      val whiteVertex = map.faceVertex(leftFace)

      /* The four black vertices of C have no outgoing edge in the interior of C. Start at the edge going from the
       * first vertex in the edge to the white vertex, then move counter clockwise until a previous white vertex
       * is found. Every edge along the way needs to be ingoing.
       */
      val blackVertex1 = regularToAngular(e._1)
      var edge = map.angularEmbedding.embeddingFor(blackVertex1).entryFor(whiteVertex).previous

      while (!whiteVertices.contains(edge.adjacentVertex)) {
        if (map.orientationOfEdge(blackVertex1, edge.adjacentVertex) == edge.adjacentVertex)
          return false

        edge = edge.previous
      }

      /* For the four white vertices of C, their unique incident edge not on C is interior to C. */
      val blackVertex2 = regularToAngular(e._2)
      val counterClockwiseNeighborOfBlack2 = map.angularEmbedding.embeddingFor(whiteVertex).entryFor(blackVertex2).previous

      // The cc vertex needs to be an inner vertex, not the previous black one (this occurs when the unique incident
      // edge not on C is exterior to C.
      if (counterClockwiseNeighborOfBlack2.adjacentVertex == blackVertex1)
        return false

    }

    true
  }

  private def findRELEdges[V](es: Vector[(V, V)], rel: EdgeLabeling[V]): Vector[LabelEdge[V]] = {
    val tupleSet = es.map(UnorderedPair(_)).toSet
    rel.edges.filter(e => tupleSet.contains(UnorderedPair(e.from, e.to)))
  }

  private def parseInnerEdges[V](edges: Vector[(V, V)],
                                 outer: Set[V],
                                 rel: EdgeLabeling[V],
                                 clockwiseCycle: Boolean): Vector[LabelEdge[V]] = {
    val edgePairs = edges zip (edges.drop(1) :+ edges.head) // Needed to find inner clockwise vertices
    val embedding = rel.originalEmbedding

    /* Start by finding a vertex whose neighbor isn't the vertex preceding it on the outer 4-cycle.
     * Which direction to look in depends on whether the cycle is clockwise or ccw.
     */
    val start: ((V, V), (V, V)) = edgePairs.find(p => {
      val vertex = p._1._2
      val next = p._2._2
      val previous = p._1._1
      val entry = embedding.embeddingFor(vertex).entryFor(next)
      val nextVertex = if (clockwiseCycle) entry.next.adjacentVertex else entry.previous.adjacentVertex
      nextVertex != previous
    }).get
    val startEntry = embedding.embeddingFor(start._1._2).entryFor(start._2._2)
    val startNeighbor = if (clockwiseCycle) startEntry.next.adjacentVertex else startEntry.previous.adjacentVertex
    val processed = new mutable.HashSet[V]()
    val addedEdges = new mutable.HashSet[UnorderedPair[V]]()
    val unProcessed = new mutable.Queue[V]()
    unProcessed += startNeighbor

    val innerEdges = new ListBuffer[UnorderedPair[V]]()

    while (!unProcessed.isEmpty) {
      val next = unProcessed.dequeue()
      processed += next
      val neighbors = embedding
        .neighborsOf(next)
        .filter(!processed.contains(_))

      for (n <- neighbors) {
        val edge = UnorderedPair(next, n)
        if (!addedEdges.contains(edge)) {
          innerEdges += edge
          addedEdges += edge
        }

        // Edges connected to outer vertices will be added when the inner vertex is processed, so ignore the outer ones.
        if (!processed.contains(n) && !outer.contains(n)) {
          unProcessed enqueue n
        }
      }

      processed += next
    }


    findRELEdges(innerEdges.map(_.asTuple).toVector, rel)
  }
}
