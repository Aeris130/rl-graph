package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.RectangleRefiner
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.{OrthogonalDartRep, _}
import net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts.{DefaultDart, SplitDart}
import rlgraph.SpecImports

class RectangleRefinerSpec extends SpecImports {
  private val wrapperFact = new VertexWrapperFactory[String]()
  private val refiner = new RectangleRefiner(wrapperFact)

  def faceWithExternalAngle = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")
    val G = new VertexWrapper[String](7, "G")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 3, 0) // <- External angle
    val CtoD = new DefaultDart[String](C, D, 1, 0) // <- Internal angle 1
    val DtoE = new DefaultDart[String](D, E, 1, 0) // <- Internal angle 2
    val EtoF = new DefaultDart[String](E, F, 2, 0) // 180 degree angle, vertex F can be used to connect to
    val FtoG = new DefaultDart[String](F, G, 1, 0)
    val GtoA = new DefaultDart[String](G, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoE, EtoF, FtoG, GtoA)
  }

  def faceWithExternalAngleAndInternalAngleAfterwards = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")
    val G = new VertexWrapper[String](7, "G")
    val H = new VertexWrapper[String](8, "H")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 3, 0) // <- External angle
    val CtoD = new DefaultDart[String](C, D, 1, 0) // <- Internal angle 1
    val DtoE = new DefaultDart[String](D, E, 1, 0) // <- Internal angle 2
    val EtoF = new DefaultDart[String](E, F, 1, 0) // 90 degree angle, vertex F can't be used to connect to, EtoF must be split
    val FtoA = new DefaultDart[String](F, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoE, EtoF, FtoA)
  }

  def rectangularFaceWithBridge = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")
    val G = new VertexWrapper[String](7, "G")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoD = new DefaultDart[String](C, D, 4, 0) // Bridge pt1, 60 degree external angle
    val DtoC = new DefaultDart[String](D, C, 1, 0) // Bridge pt2
    val CtoE = new DefaultDart[String](C, E, 1, 0)
    val EtoF = new DefaultDart[String](E, F, 2, 0) // 180 degrees bend, can be used to connect dummy
    val FtoG = new DefaultDart[String](F, G, 1, 0)
    val GtoA = new DefaultDart[String](G, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoC, CtoE, EtoF, FtoG, GtoA)
  }

  def externalFaceWithStaircase = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")

    val AtoB = new DefaultDart[String](A, B, 3, 0)
    val BtoC = new DefaultDart[String](B, C, 3, 0) // External angle before internal
    val CtoD = new DefaultDart[String](C, D, 1, 0) // Internal angle
    val DtoE = new DefaultDart[String](D, E, 3, 0)
    val EtoF = new DefaultDart[String](E, F, 3, 0) // External angle after internal
    val FtoA = new DefaultDart[String](F, A, 3, 0)

    val face = new OFace(AtoB, BtoC, CtoD, DtoE, EtoF, FtoA)
  }

  def twoBridgesWithAnAngle = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")

    val AtoB = new DefaultDart[String](A, B, 4, 0)
    val BtoA = new DefaultDart[String](B, A, 1, 0)
    val AtoC = new DefaultDart[String](A, C, 4, 0)
    val CtoA = new DefaultDart[String](C, A, 3, 0)

    val face = new OFace(AtoB, BtoA, AtoC, CtoA)
  }

  /*
   * Test graph for multiple-face refinement, with the following properties:
   *  - An external face having both pattern 1 and 2 to refine
   *  - multiple internal faces
   *  - internal faces requiring multiple refinements and splits
   */
  def multipleFaceRefinementTest = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")
    val G = new VertexWrapper[String](7, "G")
    val H = new VertexWrapper[String](8, "H")
    val I = new VertexWrapper[String](9, "I")
    val J = new VertexWrapper[String](10, "J")
    val K = new VertexWrapper[String](11, "K")
    val L = new VertexWrapper[String](12, "L")

    // External face
    val AtoI = new DefaultDart[String](A, I, 2, 0)
    val ItoH = new DefaultDart[String](I, H, 3, 0)
    val HtoG = new DefaultDart[String](H, G, 3, 0)
    val GtoF = new DefaultDart[String](G, F, 1, 0)
    val FtoE = new DefaultDart[String](F, E, 3, 0)
    val EtoB = new DefaultDart[String](E, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoD = new DefaultDart[String](C, D, 4, 0)
    val DtoC = new DefaultDart[String](D, C, 3, 0)
    val CtoB = new DefaultDart[String](C, B, 2, 0)
    val BtoA = new DefaultDart[String](B, A, 3, 0)

    val externalFace = new OFace(AtoI, ItoH, HtoG, GtoF, FtoE, EtoB, BtoC, CtoD, DtoC, CtoB, BtoA)

    // Internal face 1
    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoE = new DefaultDart[String](B, E, 1, 0)
    val EtoF = new DefaultDart[String](E, F, 2, 0)
    val FtoL = new DefaultDart[String](F, L, 1, 0)
    val LtoJ = new DefaultDart[String](L, J, 1, 0)
    val JtoK = new DefaultDart[String](J, K, 4, 0)
    val KtoJ = new DefaultDart[String](K, J, 2, 0)
    val JtoI = new DefaultDart[String](J, I, 1, 0)
    val ItoA = new DefaultDart[String](I, A, 1, 0)

    val internalFace1 = new OFace(AtoB, BtoE, EtoF, FtoL, LtoJ, JtoK, KtoJ, JtoI, ItoA)

    // Internal face 2
    val ItoJ = new DefaultDart[String](I, J, 1, 0)
    val JtoL = new DefaultDart[String](J, L, 3, 0)
    val LtoF = new DefaultDart[String](L, F, 1, 0)
    val FtoG = new DefaultDart[String](F, G, 1, 0)
    val GtoH = new DefaultDart[String](G, H, 1, 0)
    val HtoI = new DefaultDart[String](H, I, 1, 0)

    val internalFace2 = new OFace(ItoJ, JtoL, LtoF, FtoG, GtoH, HtoI)

    val representation = new OrthogonalDartRep[String](Set(externalFace, internalFace1, internalFace2), externalFace)
  }


  describe("RectangleRefiner") {

    /*
     *
     *    >>> Operation 1 tests <<<
     *
     *      Refining an internal face by the pattern '100'.
     *
     *      All tests are presumed to have the condition that an external angle is found, followed by two internals.
     *
     */

    it ("should split off a face into a new rectangular face") {

      Given("a face with an external angle followed by two internal angles and a 180 degree angle")
      val f = faceWithExternalAngle
      import f._

      When("refining the face")
      val refinedFaces: ((OFace[String], OFace[String]), Option[(Dart[String], (Dart[String], Dart[String]))]) = refiner.operation1(face).get
      val secondaryFace: OFace[String] = refinedFaces._1._2
      val splits: Option[(Dart[String], (Dart[String], Dart[String]))] = refinedFaces._2

      Then("the second face should be the edges CtoD, DtoE, EtoF and a dummy connecting F and C")
      secondaryFace.size should be (4)

      val face2Edges = secondaryFace.iteratorFrom(CtoD)
      val f_CtoD = face2Edges.next()
      val f_DtoE = face2Edges.next()
      val f_EtoF = face2Edges.next()
      val face2Dummy = face2Edges.next()

      f_DtoE should equal (DtoE)
      f_EtoF.from should equal (EtoF.from) // using vertex values for this, since the angle is different in the new dart
      f_EtoF.to should equal (EtoF.to)
      face2Dummy.from should equal (F)
      face2Dummy.to should equal (C)

      And("every angle inside the face should be a 90 degrees internal angle")
      f_CtoD.nextDegree should be (1)
      f_DtoE.nextDegree should be (1)
      f_EtoF.nextDegree should be (1)
      face2Dummy.nextDegree should be (1)

      And("no splits should be reported")
      splits should be (None)

    }

    it ("remove the split-off face from the original") {

      Given("a face with an external angle followed by two internal angles and a 180 degree angle")
      val f = faceWithExternalAngle
      import f._

      When("refining the face")
      val refinedFaces: ((OFace[String], OFace[String]), Option[(Dart[String], (Dart[String], Dart[String]))]) = refiner.operation1(face).get
      val primaryFace: OFace[String] = refinedFaces._1._1

      Then("the second face should be the edges FtoG, GtoA, AtoB, BtoC and a dummy connecting C and F")
      primaryFace.size should be (5)

      val face1Edges = primaryFace.iteratorFrom(FtoG)
      val f_FtoG = face1Edges.next()
      val f_GtoA = face1Edges.next()
      val f_AtoB = face1Edges.next()
      val f_BtoC = face1Edges.next()
      val face1Dummy = face1Edges.next()

      f_FtoG should equal (FtoG)
      f_GtoA should equal (GtoA)
      f_AtoB should equal (AtoB)
      f_BtoC.to should equal (BtoC.to) // using vertex values for this, since the angle is different in the new dart
      f_BtoC.from should equal (BtoC.from)
      face1Dummy.from should equal (C)
      face1Dummy.to should equal (F)

      And("every angle inside the face should be a 90 degrees internal angle")
      f_FtoG.nextDegree should be (1)
      f_GtoA.nextDegree should be (1)
      f_AtoB.nextDegree should be (1)
      f_BtoC.nextDegree should be (2)
      face1Dummy.nextDegree should be (1)

    }

    it ("should split an edge when splitting the face, if no vertex is available to connect the dummy to") {

      Given("a face with an internal bend after the second internal bend from the first found external bend")
      val f = faceWithExternalAngleAndInternalAngleAfterwards
      import f._

      When("refining the face")
      val refinedFaces: ((OFace[String], OFace[String]), Option[(Dart[String], (Dart[String], Dart[String]))]) = refiner.operation1(face).get
      val primaryFace: OFace[String] = refinedFaces._1._1
      val secondaryFace: OFace[String] = refinedFaces._1._2
      val splits: Option[(Dart[String], (Dart[String], Dart[String]))] = refinedFaces._2

      Then("the edge E->F should be split")
      splits should be ('defined)
      val splitEdge = splits.get._1
      val split1 = splits.get._2._1
      val split2 = splits.get._2._2
      val dummyVertex = split1.to

      splitEdge should equal (EtoF)
      split1.from should be (E)
      split2.from should be (dummyVertex)
      split2.to should be (F)

      And("the edge that comes after BtoC in the main face should connect from C to the dummy")
      val nextDartMain = primaryFace.getDartAfter(primaryFace.getDart(B, C).get).get
      nextDartMain.from should be (C)
      nextDartMain.to should be (dummyVertex)

      And("the edge that comes after the dummy in the main face should connect to F")
      val afterDummyMain = primaryFace.getDartAfter(nextDartMain).get
      afterDummyMain should equal (split2)

      And("the edge that comes after DtoE in the secondary face should connect from E to the dummy")
      val nextDartSecond = secondaryFace.getDartAfter(DtoE).get
      nextDartSecond should equal (split1)

      And("the edge that comes after the split in the secondary face should be the dummy to C")
      val afterDummySecond = secondaryFace.getDartAfter(split1).get
      afterDummySecond.from should be (dummyVertex)
      afterDummySecond.to should be (C)

      And("all angles around the split should be 1")
      primaryFace.getDart(C, dummyVertex).get.nextDegree should be (1)
      secondaryFace.getDart(E, dummyVertex).get.nextDegree should be (1)

    }

    it ("should refine bridges") {

      Given("a graph with a bridge")
      val f = rectangularFaceWithBridge
      import f._

      When("refining the face")
      val refinedFaces: ((OFace[String], OFace[String]), Option[(Dart[String], (Dart[String], Dart[String]))]) = refiner.operation1(face).get
      val primaryFace: OFace[String] = refinedFaces._1._1
      val secondaryFace: OFace[String] = refinedFaces._1._2

      Then("the end of the bridge (D) should be joined with the first vertex that comes after the two internal angles (F) in the primary face")
      primaryFace.size should be (6)
      val newDummy1 = primaryFace.getDartAfter(CtoD).get
      newDummy1.from should be (D)
      newDummy1.to should be (F)

      primaryFace.getDart(C, D).get.nextDegree should be (3)

      And("the end of dart EtoF (F) should be connected to the end of the bridge (D) in the scondary face")
      secondaryFace.size should be (4)
      val newDummy2 = secondaryFace.getDartAfter(secondaryFace.getDart(E, F).get).get // New angle, so vertices has to be used
      newDummy2.from should be (F)
      newDummy2.to should be (D)
      newDummy2.nextDegree should be (1)
    }

    it ("should throw an exception if attempting to process a face with edge bends") {

      Given("a face with an edge bend")
      val f = faceWithExternalAngle
      import f._
      val illegalFace = new OFace(new DefaultDart[String](A, B, 1, 3), BtoC, CtoD, DtoE, EtoF, FtoG, GtoA)

      When("refining the face")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        refiner.operation1(illegalFace)
      }
    }

    it ("should store the original edge inside the splits when splitting it usng op1") {

      Given("a face where the edge E->F gets split")
      val f = faceWithExternalAngleAndInternalAngleAfterwards
      import f._

      When("refining the face")
      val refinedFaces: ((OFace[String], OFace[String]), Option[(Dart[String], (Dart[String], Dart[String]))]) = refiner.operation1(face).get
      val splits: Option[(Dart[String], (Dart[String], Dart[String]))] = refinedFaces._2

      Then("the original dart in both splits should be EtoF")
      val split1 = splits.get._2._1
      val split2 = splits.get._2._2

      split1 match {
        case sd: SplitDart[String] => {
          sd.originalDart should be (EtoF)
        }
        case d: Dart[String] => fail("The split dart was not of object type " + classOf[SplitDart[String]] + ", but " + d.getClass)
      }
      split2 match {
        case sd: SplitDart[String] => {
          sd.originalDart should be (EtoF)
        }
        case d: Dart[String] => fail("The split dart was not of object type " + classOf[SplitDart[String]] + ", but " + d.getClass)
      }

    }

    /*
     *
     *    >>> Operation 2 tests <<<
     *
     *      Refining an internal face by the pattern '101'.
     *
     *      All tests are presumed to have the condition that an external angle is found, followed by an
     *      internal angle and another external.
     *
     */

    it ("should split an external face into a rectangular subset") {

      Given("a graph with a staircase between vertex C, D and E")
      val f = externalFaceWithStaircase
      import f._

      When("refining the face")
      val refinedFaces: (OFace[String], OFace[String]) = refiner.operation2(face).get
      val secondaryFace = refinedFaces._2

      Then("the secondary face should be made up of the vertices C, D, E, Dummy")
      secondaryFace.size should be (4)

      val darts = secondaryFace.iteratorFrom(C, D)
      val f_CtoD = darts.next()
      val f_DtoE = darts.next()
      val f_EtoDummy = darts.next()
      val f_DummyToC = darts.next()

      f_CtoD should equal (CtoD)
      f_DtoE.from should be (D) // Different angle
      f_DtoE.to should be (E)
      f_EtoDummy.from should be (E)
      f_EtoDummy.to.isDefined should be (false)
      f_DummyToC.from should equal (f_EtoDummy.to)
      f_DummyToC.to should be (C)

      And("every angle should be internal")
      f_CtoD.nextDegree should be (1)
      f_DtoE.nextDegree should be (1)
      f_EtoDummy.nextDegree should be (1)
      f_DummyToC.nextDegree should be (1)

    }

    it ("should remove the rectangular subset from the external face") {

      Given("a graph with a staircase between vertex C, D and E")
      val f = externalFaceWithStaircase
      import f._

      When("refining the face")
      val refinedFaces: (OFace[String], OFace[String]) = refiner.operation2(face).get
      val primaryFace = refinedFaces._1

      Then("the internal vertex sequence C, D, E should have been replaced with the external corner C, Dummy, E")
      primaryFace.size should be (6)

      val darts = primaryFace.iteratorFrom(B, C)
      val f_BtoC = darts.next()
      val f_CtoDummy = darts.next()
      val f_DummyToE = darts.next()

      f_BtoC.from should be (B)
      f_BtoC.to should be (C)
      f_CtoDummy.from should be (C)
      f_CtoDummy.to.isDefined should be (false)
      f_DummyToE.from should be (f_CtoDummy.to)
      f_DummyToE.to should be (E)

      And("the angle between C and the dummy should be 2")
      f_BtoC.nextDegree should be (2)

      And("the angle of the dummy should be 3")
      f_CtoDummy.nextDegree should be (3)

      And("the angle after the dummy and E should be 2")
      f_DummyToE.nextDegree should be (2)

    }

    it ("should correct angles even when connecting bridges") {

      Given("a graph with two bridges forming an angle")
      val f = twoBridgesWithAnAngle
      import f._

      When("refining the face")
      val refinedFaces: (OFace[String], OFace[String]) = refiner.operation2(face).get
      val primaryFace = refinedFaces._1

      Then("all angles in the outer face should be 3")
      val darts = primaryFace.iteratorFrom(A, B)

      val f_AtoB = darts.next()
      val BtoDummy = darts.next()
      val DummyToC = darts.next()
      val CtoA = darts.next()

      f_AtoB.nextDegree should be (3)
      BtoDummy.nextDegree should be (3)
      DummyToC.nextDegree should be (3)
      CtoA.nextDegree should be (3)

    }

    /*
     *
     * Testing multiple-face refinement
     *
     */

    it ("should refine the external face of a multi-face representation") {

      Given("a representation with an external face having one pattern of 101, and one of 100")
      val f = multipleFaceRefinementTest
      import f._

      When("refining the representation")
      val refinedRep = refiner.refineFaces(representation)

      Then("the external face should be A->I->H->G->Dummy->D->C->B->A")

      /* The dart H->G is split by the internal face, I->H is the closest intact dart. */
      val extFaceDarts = refinedRep.externalFace.iteratorFrom(I, H)
      extFaceDarts.next() // Remove I->H
      extFaceDarts.next() // Remove H->Dummy

      val dummyToG = extFaceDarts.next()
      dummyToG.nextDegree should be (2)

      val GtoDummy = extFaceDarts.next()
      assert(!GtoDummy.to.isDefined, "The dart " + GtoDummy + " did not have a dummy as .to value")
      GtoDummy.nextDegree should be (3)

      val dummyToD =  extFaceDarts.next()
      dummyToD.from should be (GtoDummy.to)
      dummyToD.to should be (D)
      dummyToD.nextDegree should be (2)

      extFaceDarts.next() should be (DtoC)

    }

    it ("should split the opposite dart whenever a dart is split") {

      Given("a representation with an internal face having one pattern of 100, causing a split on dart A->B")
      val f = multipleFaceRefinementTest
      import f._

      When("refining the representation")
      val refinedRep = refiner.refineFaces(representation)

      Then("the dart B->A on the external face should be split as well")
      val extFaceDarts = refinedRep.externalFace.iteratorFrom(C, B)
      extFaceDarts.next() // Remove C->B

      val BtoDummy = extFaceDarts.next()
      BtoDummy.from should be (B)

      assert(!BtoDummy.to.isDefined, "Vertex " + BtoDummy.to + " in " + BtoDummy + " was not a dummy, dart " + BtoDummy + " was not split.")

    }

    it ("should refine a face multiple times if needed") {

      Given("a face with internal pattern 100 that leaves another face with pattern 100 when it is refined")
      val f = multipleFaceRefinementTest
      import f._
      val face = internalFace1

      When("refining the representation")
      val refinedRep = refiner.refineFaces(representation)

      Then("the face should be refined into face: K, J, I, A, Dummy1...")
      val face1 = refinedRep.faces.find(f => f.contains(K, J)).get
      val darts1 = face1.iteratorFrom(K, J)

      darts1.next() should be (KtoJ)
      darts1.next() should be (JtoI)
      darts1.next() should be (ItoA)

      val AtoDummy = darts1.next()
      AtoDummy.from should be (A)
      assert(!AtoDummy.to.isDefined, "Expected a dummy as .to, was " + AtoDummy.to + " in " + AtoDummy)
      val dummy1 = AtoDummy.to

      val dummyToK = darts1.next()
      dummyToK.to should be (K)

      And("the face should be refined into face: K, Dummy1, B, Dummy2")
      val face2 = refinedRep.faces.find(f => f.contains(K, dummy1)).get
      val darts2 = face2.iteratorFrom(K, dummy1)
      darts2.next() // Remove K->Dummy1

      val dummyToB = darts2.next()
      dummyToB.from should be (dummy1)
      dummyToB.to should be (B)

      val bToDummy2 = darts2.next()
      bToDummy2.from should be (B)
      val dummy2 = bToDummy2.to
      assert(!dummy2.isDefined, "Expected a dummy as .to, was " + dummy2 + " in " + bToDummy2)

      val dummy2toK = darts2.next()
      dummy2toK.from should be (dummy2)
      dummy2toK.to should be (K)

      And("the face should be refined into face: Dummy2, E, F, L, J, K")
      val face3 = refinedRep.faces.find(f => f.contains(dummy2, E)).get
      val darts3 = face3.iteratorFrom(dummy2, E)
      darts3.next() // Remove dummy2->E

      darts3.next() should be (EtoF)
      darts3.next() should be (FtoL)
      darts3.next() should be (LtoJ)
      darts3.next() should be (JtoK.newAngle(2)) // From refinement

      val KtoDummy2 = darts3.next()
      KtoDummy2.from should be (K)
      KtoDummy2.to should be (dummy2)

    }

    it ("should refine a representation into the correct amount of faces") {

      Given("a representation with 3 faces, that refines into 7 faces + external face")
      val f = multipleFaceRefinementTest
      import f._

      When("refining the representation")
      val refinedRep = refiner.refineFaces(representation)

      Then("the number of faces should be 8")
      refinedRep.faces.size should be (8)

      And("the external face should be present in the face collection")
      refinedRep.faces should contain (refinedRep.externalFace)

    }

    it ("should refine a single external face that can't be refined anymore") {

      Given("a single face of two vertices with an edge")
      val A = new VertexWrapper[String](1, "A")
      val B = new VertexWrapper[String](2, "B")

      val AtoB = new DefaultDart[String](A, B, 4, 0)
      val BtoA = new DefaultDart[String](B, A, 4, 0)

      val face = new OFace(AtoB, BtoA)

      val representation = new OrthogonalDartRep[String](Set(face), face)

      When("refining the representation")
      val refinedRep = refiner.refineFaces(representation)

      Then("the resulting representation should equal the old one")
      refinedRep should equal (representation)

    }
  }
}
