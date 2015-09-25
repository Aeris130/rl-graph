package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{OrthogonalDartRep, _}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.{ArtificialDart, SplitDart}

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Refines an orthogonal face of darts by shaping the face into a rectangle. This is done by inserting a single
 * new edge (dart) into the face in a position that splits it into two face, a secondary which is a rectangle, and
 * a primary that either is rectangular as well, or requires more refinement (think of it as the "original" face,
 * minus the part that was split off by the edge).
 *
 * The face MUST be simple (no edge-bends will be taken into consideration).
 *
 * Refinement is done by searching for patterns (ex: 001) in vertex bends, where:
 *    - 1 is a 270 degrees outward bend
 *    - 0 is a 90 degrees inward bend
 *    - 11 is a 360 degrees rotation
 *
 * @constructor Creates a new rectangle refiner.
 * @tparam VType Type of vertex used in graph.
 */
class RectangleRefiner[VType](wrapperFactory: VertexWrapperFactory[VType]) {

  /**
   * Refines every face in an orthogonal representation.
   *
   * @param representation All faces to refine. Needed since the external face has a separate refinement algorithm.
   * @return a new representation with all faces refined into rectangles.
   */
  def refineFaces(representation: OrthogonalDartRep[VType]): OrthogonalDartRep[VType] = {

    /* If the external face is split during operation 1, this reference keeps track of the external bit. */
    var externalFace = representation.externalFace

    /* All faces. Every split face is stored here as well. */
    var unrefinedFaces = representation.faces
    var refinedFaces = Set[OFace[VType]]()

    /* Step 1: Perform operation 1 on every face.
     *
     * Since doing so may result in a dart being split, op1 can only be performed once on each face, after which
     * the edge that were split (if any) must also have its opposite dart split in the face it belongs to before
     * op1 can be applied again.
     */

    while (!unrefinedFaces.isEmpty) {
      val face = unrefinedFaces.head // Arbitrary face
      val refinement = operation1(face)
      val faceIsExternal = face == externalFace

      if (refinement.isDefined) {
        val faces: (OFace[VType], OFace[VType]) = refinement.get._1
        val remainingFace = faces._1
        val splitFace = faces._2
        val splits: Option[(Dart[VType], (Dart[VType], Dart[VType]))] = refinement.get._2

        /* The face that gets split off is always 100% refined. */
        refinedFaces += splitFace
        unrefinedFaces -= face

        if (remainingFace.isInternallyRectangular || (faceIsExternal && operation1(remainingFace).isEmpty)) {
          refinedFaces += remainingFace

        } else {
          unrefinedFaces += remainingFace
        }

        if (faceIsExternal)
          externalFace = remainingFace

        /* Before the split edges are split further, they have to have the opposite of the split edge split as well. */
        if (splits.isDefined) {
          val splitDart = splits.get._1
          val split1 = splits.get._2._1 // .To is the dummy vertex used in the split
          val split2 = splits.get._2._2

          var oppositeFaceIsRefined = false
          var faceWithOppositeDart: Option[OFace[VType]] = unrefinedFaces.find(f => f.contains(splitDart.to, splitDart.from))

          if (faceWithOppositeDart.isEmpty) {
            faceWithOppositeDart = refinedFaces.find(f => f.contains(splitDart.to, splitDart.from))
            oppositeFaceIsRefined = true
          }

          val dartToSplit = faceWithOppositeDart.get.getDart(splitDart.to, splitDart.from).get
          val oppositeSplit1 = new SplitDart[VType](dartToSplit.from, split1.to, 2, dartToSplit, dartToSplit.direction)
          val oppositeSplit2 = new SplitDart[VType](split2.from, dartToSplit.to, dartToSplit.nextDegree, dartToSplit, dartToSplit.direction)

          val faceWithSplitAdded = faceWithOppositeDart.get.replaceDart(dartToSplit, Vector(oppositeSplit1, oppositeSplit2))

          if (faceWithOppositeDart.get == externalFace)
            externalFace = faceWithSplitAdded

          if (!oppositeFaceIsRefined) {
            unrefinedFaces -= faceWithOppositeDart.get
            unrefinedFaces += faceWithSplitAdded
          } else {
            refinedFaces -= faceWithOppositeDart.get
            refinedFaces += faceWithSplitAdded
          }
        }

      } else {

        // If refinement wasn't defined, the face is already refined according to operation 1.
        refinedFaces += face
        unrefinedFaces -= face
      }

    }

    /* Step 2: Refine the external face with operation 2 until it is refined. */
    var noRefinementPossible = false
    while (!externalFace.isExternallyRectangular && !noRefinementPossible) {
      val refinement: Option[(OFace[VType], OFace[VType])] = operation2(externalFace)

      if (refinement.isDefined) {
        val remainingFace = refinement.get._1
        val rectangularSplit = refinement.get._2
        refinedFaces -= externalFace
        refinedFaces += rectangularSplit
        refinedFaces += remainingFace
        externalFace = remainingFace
      } else {
        noRefinementPossible = true
      }
    }

    new OrthogonalDartRep[VType](refinedFaces, externalFace)
  }

  /**
   * Refines a face by searching for the pattern 100. An edge e is connected between the vertex 'A having bend 1,
   * and the edge going out of the second 0-bend. If that edge ends in any bend other than 0 (a 90 degrees bend
   * inward towards the vertex 'A that's being connected), the vertex at the end of the edge will be used to
   * connect e. If it is, a new dummy-vertex must be inserted on e by splitting it in two, with the upper part
   * becoming a part of the new face.
   *
   * @param face Face to refine.
   * @return None if no further refinement was possible. If refinement occurred, the result is a tuple containing:
   *
   *            - The two faces that were created as a result of inserting a new edge between two vertices. The first
   *              face is the original face minus the part that were split off (not guaranteed to be rectangular), and
   *              the second face is the rectangular split.
   *            - If an edge had to be split to create a dummy-vertex in order to connect aforementioned edge,
   *            a pair where the first element is the edge that were split, and the second is a tuple containing
   *            the two splits. None if no split were performed.
   */
  def operation1[EType[X] <: UnDiEdge[X]](face: OFace[VType]): Option[((OFace[VType], OFace[VType]), Option[(Dart[VType], (Dart[VType], Dart[VType]))])] = {
    noEdgeBendsPresentCheck(face)
    if (face.size < 3) return None

    var modifiedFace = face
    val darts = face.darts
    var n = 0
    while (n < darts.size) {
      val dart: Dart[VType] = darts(n)

      /* 3 times 90 degrees == 270, time to search for the corresponding 2nd internal bend-dart. */
      if (dart.nextDegree == 3 || dart.nextDegree == 4) {
        val iterator = face.iteratorFrom(dart.from, dart.to)
        iterator.next() // Gets rid of 'dart

        /* If refining the face turns out to be possible, the edge after the current dart that will
         * be the first in the new face.
         */
        val findingFirstDart = face.iteratorFrom(dart)
        findingFirstDart.next()
        val firstEdgeInNewFace = findingFirstDart.next()

        /* Look for 2 internal bends with no external bends between them, and report the edge coming after that. */
        val dartAfter2ndBend = findDartAfterTwoInternalBends(iterator)

        if (dartAfter2ndBend.isDefined) {
          val da2ndB = dartAfter2ndBend.get

          /* These splits might not be used, but they're declared here anyway since comparisons are needed later.
           * The first split has its angle set to 1, to reflect that it would be an internal angle in the face it
           * ends up being used in.
           */
          val dummyVertex: VertexWrapper[VType] = wrapperFactory.createEmptyVertexWrapper
          val split1 = new SplitDart[VType](da2ndB.from, dummyVertex, 2, da2ndB, None) // Angle is 180 degrees towards the next split  // TODO replace None <- original edge
          val split2 = new SplitDart[VType](dummyVertex, da2ndB.to, da2ndB.nextDegree, da2ndB, None) // TODO replace None <- original edge

          /* Insert the new edge between the current dart, and the dart that comes after the second bend. */
          val edgeToSplitFaceAt =
            if (da2ndB.nextDegree == 1) {

              /* If the vertex-bend around da2ndb is internal, the upcoming edge points towards the vertex in dart.to, making it
               * impossible to make use of da2ndb's vertex. Split the found dart in two, and connect to a dummy vertex
               * instead.
               *
               * Note that the face now contains an illegal angle in split1, that has already been set to 1 in
               * preparation for the split.
               */
              modifiedFace = modifiedFace.replaceDart(da2ndB, Vector(split1, split2))
              split1

            } else {

              /* Possible to connect to dartAfter2ndBend.to. But since dartAfter2ndBend will be leading up to
               * an internal angle in the new face, a copy with bend 1 is needed.
               */
              da2ndB.newAngle(1)
            }

          /* Now we have enough information to split off a subset of the current face into a new rectangular one. */
          val newFace = new ArrayBuffer[Dart[VType]]()
          val dartsOnFace = modifiedFace.iteratorFrom(dart)
          dartsOnFace.next() // We're splitting the face after the dart that contained the 270 degree angle
          var stop = false

          while (dartsOnFace.hasNext && !stop) {
            val d = dartsOnFace.next()
            if (d.from == edgeToSplitFaceAt.from && d.to == edgeToSplitFaceAt.to)
              stop = true
            else
              newFace += d
          }

          /* Add a copy of the edge to split at, but with an internal angle coming after it. */
          var copiedSplit = edgeToSplitFaceAt.newAngle(1)
          newFace += copiedSplit

          /* Add a dart from the dart that comes after the bend, to the end-vertex of the dart that originally had
           * the exterior bend.
           */
          val dummyEdge = new ArtificialDart(edgeToSplitFaceAt.to, dart.to, 1)
          newFace += dummyEdge
          val rectangularFace = new OFace(newFace.toVector)

          /* now that we've added an edge from the dart that comes after the second internal angle, an opposite edge
           * must be added to the remaining face. It's angle is determined by the angle of the dart it's connecting
           * to (either the original dart or the first of the splits) - 1. (angle - 1) is not defined when the angle
           * is 1, but that sort of angle would've been removed by splits and turned into a 2.
           */
          val angle = if (edgeToSplitFaceAt == split1) split1.nextDegree - 1 else da2ndB.nextDegree - 1
          val dummyEdgeInOriginalFace = new ArtificialDart(dart.to, edgeToSplitFaceAt.to, angle)
          val originalFaceMinusCutoff = modifiedFace.replaceDartSubset(firstEdgeInNewFace, edgeToSplitFaceAt, Vector(dummyEdgeInOriginalFace))

          /* The edge with the external angle still has that angle, so it has to be replaced by a copy with
           * the angle shifter 90 degree's towards the new dummy
           */
          val originalFaceWithCorrectAngles = originalFaceMinusCutoff.replaceDart(dart, Vector(dart.newAngle(dart.nextDegree - 1)), false)

          /* Since this operation only makes a single cutoff, the faces can now be returned. Split compairson
           * is done on vertices only, since the angle is now 1 instead of 2.
           */
          val splits = if (edgeToSplitFaceAt.from == split1.from && edgeToSplitFaceAt.to == split1.to)
            Option((da2ndB, (copiedSplit, split2)))
          else None
          return Option(((originalFaceWithCorrectAngles, rectangularFace), splits))
        }
      }
      n += 1
    }

    None
  }

  /**
   * Refines an external face by looking for the pattern "101", replacing it by "1" using two dummy edges and a
   * dummy vertex.
   *
   * @param face Face to refine.
   * @return None if no refinement was possible, or the primary/secondary face that was created by splitting
   *         the rectangle between the two external edges into a separate face.
   */
  def operation2[EType[X] <: UnDiEdge[X]](face: OFace[VType]): Option[(OFace[VType], OFace[VType])] = {
    noEdgeBendsPresentCheck(face)
    if (face.size < 3) return None

    val darts = face.darts.iterator
    while (!darts.isEmpty) {
      val dart: Dart[VType] = darts.next()

      /* Any corner that isn't an internal angle or straight (180 degrees) can be used as starting point. */
      if (dart.nextDegree == 3 || dart.nextDegree == 4) {
        val upcomingDarts = face.iteratorFrom(face.getDartAfter(dart).get)

        /* Contains all darts from the first external angle up to the first internal angle. */
        val nextInternalAngle = findNextAngle(upcomingDarts, Set(1))

        /* Contains all darts from the first internal angle up to the next external angle. */
        val nextExternalAngle = findNextAngle(upcomingDarts, Set(3, 4))

        if (!nextInternalAngle.isEmpty && !nextExternalAngle.isEmpty) {
          val firstDart = nextInternalAngle.head
          val lastDart = nextExternalAngle(nextExternalAngle.size - 1)
          val firstVertex = nextInternalAngle.head.from
          val lastVertex = lastDart.to
          val dummyVertex = wrapperFactory.createEmptyVertexWrapper
          val internalFaceDummyEdge1 = new ArtificialDart[VType](lastVertex, dummyVertex, 1)
          val internalFaceDummyEdge2 = new ArtificialDart[VType](dummyVertex, firstVertex, 1)

          var newSecondaryFace = new OFace(nextInternalAngle ++ nextExternalAngle ++ Vector(internalFaceDummyEdge1, internalFaceDummyEdge2))
          newSecondaryFace = newSecondaryFace.replaceDart(lastDart, Vector(lastDart.newAngle(1)), false)

          val newExternalDummyEdge1 = new ArtificialDart[VType](firstVertex, dummyVertex, 3)

          /* The angle on the last dart depends on the previous degree, as it is decreased by 1. */
          val newExternalDummyEdge2 = new ArtificialDart[VType](dummyVertex, lastVertex, lastDart.nextDegree - 1)

          var newPrimaryFace = face.replaceDartSubset(firstDart, lastDart, Vector(newExternalDummyEdge1, newExternalDummyEdge2))

          /* Finally, the dart on the external face that had the initial external angle must have its angle
           * decreased by 1.
           */
          newPrimaryFace = newPrimaryFace.replaceDart(dart, Vector(dart.newAngle(dart.nextDegree - 1)), false)

          return Option(newPrimaryFace, newSecondaryFace)
        }
      }
    }

    None
  }

  /**
   * Starts at a dart and traverses two internal bends (degree == 1), then returns the dart that comes after the
   * second bend. If a bend other than 1 is found, search is aborted.
   *
   * @param darts Darts to traverse. Does not include the initial dart containing the degree that triggered this call.
   * @param bendsEncountered Keeps track of how many internal 90-degree bends have been encountered so far.
   * @return The dart that comes after the outgoing edge from the second internal bend, or None if two bends weren't
   *         found.
   */
  private def findDartAfterTwoInternalBends[EType[X] <: UnDiEdge[X]](darts: Iterator[Dart[VType]], bendsEncountered: Int = 0): Option[Dart[VType]] = {
    if (darts.isEmpty) return None

    val dart = darts.next()

    /* Only internal bends (1) or no bend (2) is expected, so as soon as others are found, it is safe to abort. */
    if (dart.nextDegree == 4 || dart.nextDegree == 3) {
      None

    } else if (dart.nextDegree == 2) {

      /* Ignore darts that continue straight. */
      findDartAfterTwoInternalBends(darts, bendsEncountered)

    } else {

      /* Degree 1 found. */
      if (bendsEncountered + 1 > 1) {
        Option(darts.next())
      } else {
        findDartAfterTwoInternalBends(darts, bendsEncountered + 1)
      }
    }
  }

  /**
   * Steps through an iterator, looking for the first dart whose next degree is contained in a set.
   * Darts of degree 2 are ignored.
   *
   * @param iterator Iterator over all darts to look for.
   * @param degreesToLookFor All degrees to look for.
   * @return The first dart in the iterator with one of the specified degrees.
   */
  private def findNextAngle[EType[X] <: UnDiEdge[X]](iterator: Iterator[Dart[VType]], degreesToLookFor: Set[Int]): Vector[Dart[VType]] = {
    val result = new ArrayBuffer[Dart[VType]]()

    while (iterator.hasNext) {
      val dart = iterator.next()
      result += dart
      if (degreesToLookFor.contains(dart.nextDegree))
        return result.toVector
      else if (dart.nextDegree != 2)
        return Vector()
    }

    Vector()
  }

  private def noEdgeBendsPresentCheck[EType[X] <: UnDiEdge[X]](face: OFace[VType]) =
    for (dart <- face.darts)
      if (dart.bends > 0)
        throw new IllegalArgumentException("An edge bend was found in face " + face + ", rectangularization not possible.")

}
