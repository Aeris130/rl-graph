package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.DirectionPointer
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DartDirection._
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, OFace, OrthogonalDartRep, VertexWrapper}
import rlgraph.SpecImports

class DirectionPointerSpec extends SpecImports {
  val directions = new DirectionPointer()

  def rectangularFaceWithInternalBends = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoD = new DefaultDart[String](C, D, 1, 0)
    val DtoA = new DefaultDart[String](D, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoA)
  }

  def rectangularFaceWithExternalBends = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")

    val AtoB = new DefaultDart[String](A, B, 3, 0)
    val BtoC = new DefaultDart[String](B, C, 3, 0)
    val CtoD = new DefaultDart[String](C, D, 3, 0)
    val DtoA = new DefaultDart[String](D, A, 3, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoA)
  }

  def faceWIth180DegreeBend = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")
    val E = new VertexWrapper(5, "E")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 2, 0) // 180 degree bend
    val CtoD = new DefaultDart[String](C, D, 1, 0)
    val DtoE = new DefaultDart[String](D, E, 1, 0)
    val EtoA = new DefaultDart[String](E, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoD, DtoE, EtoA)
  }

  def faceWIth360DegreeBend = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")
    val E = new VertexWrapper(5, "E")
    val F = new VertexWrapper(6, "F")
    val G = new VertexWrapper(7, "G")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
      val CtoF = new DefaultDart[String](C, F, 4, 0) // 360 degree bend
      val FtoC = new DefaultDart[String](F, C, 1, 0)
    val CtoD = new DefaultDart[String](C, D, 1, 0)
    val DtoE = new DefaultDart[String](D, E, 1, 0)
    val EtoA = new DefaultDart[String](E, A, 1, 0)

    val face = new OFace[String](AtoB, BtoC, CtoF, FtoC, CtoD, DtoE, EtoA)
  }

  def internalAndExternalFace = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")

    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoD = new DefaultDart[String](C, D, 1, 0)
    val DtoA = new DefaultDart[String](D, A, 1, 0)
    val internalFace = new OFace[String](AtoB, BtoC, CtoD, DtoA)

    val BtoA = new DefaultDart[String](B, A, 3, 0)
    val AtoD = new DefaultDart[String](A, D, 3, 0)
    val DtoC = new DefaultDart[String](D, C, 3, 0)
    val CtoB = new DefaultDart[String](C, B, 3, 0)
    val externalFace = new OFace[String](BtoA, AtoD, DtoC, CtoB)
  }

  describe("DirectionPointer") {

    /*
     *
     *  Tests for computing directions of a single face.
     *
     */
    it ("should assign a specified dart a specific direction") {

      Given("a face with a dart AtoB")
      val f = rectangularFaceWithInternalBends
      import f._

      When("computing dart directions by specifying the direction Up for the dart")
      val faceWithDirections = directions.computeDirection[String](face, AtoB, Up)

      Then("AtoB in the resulting face should have direction Up")
      val dart = faceWithDirections.getDart(A, B).get
      dart.direction.get should be (Up)

    }

    it ("should compute correct directions after an internal bend") {

      Given("a face with internal bends")
      val f = rectangularFaceWithInternalBends
      import f._

      When("computing dart directions")
      val faceWithDirections = directions.computeDirection[String](face, AtoB, Right)

      Then("the dart after AtoB should have direction Down")
      val f_BtoC = faceWithDirections.getDart(B, C).get
      f_BtoC.direction.get should be (Down)

      And("the dart after BtoC should have direction Left")
      val f_CtoD = faceWithDirections.getDart(C, D).get
      f_CtoD.direction.get should be (Left)

      And("the dart after CtoD should have direction Up")
      val f_DtoA = faceWithDirections.getDart(D, A).get
      f_DtoA.direction.get should be (Up)

      /* Re-run the test again with some other dart than AtoB as start */
      And("the dart after DtoA should have direction Right")
      val faceWithOtherStart = directions.computeDirection[String](face, DtoA, f_DtoA.direction.get)
      val f_AtoB = faceWithOtherStart.getDart(A, B).get
      f_AtoB.direction.get should be (Right)

    }

    it ("should compute correct directions after an external bend") {

      Given("a face with external bends")
      val f = rectangularFaceWithExternalBends
      import f._

      When("computing dart directions")
      val faceWithDirections = directions.computeDirection[String](face, AtoB, Right)

      Then("the dart after AtoB should have direction Up")
      val f_BtoC = faceWithDirections.getDart(B, C).get
      f_BtoC.direction.get should be (Up)

      And("the dart after BtoC should have direction Left")
      val f_CtoD = faceWithDirections.getDart(C, D).get
      f_CtoD.direction.get should be (Left)

      And("the dart after CtoD should have direction Down")
      val f_DtoA = faceWithDirections.getDart(D, A).get
      f_DtoA.direction.get should be (Down)

      /* Re-run the test again with some other dart than AtoB as start */
      And("the dart after DtoA should have direction Right")
      val faceWithOtherStart = directions.computeDirection[String](face, DtoA, f_DtoA.direction.get)
      val f_AtoB = faceWithOtherStart.getDart(A, B).get
      f_AtoB.direction.get should be (Right)

    }

    it ("should maintain the current direction when traversing a dart with a 180 degree bend") {

      Given("a face with a 180 degree bend after edge BtoC")
      val f = faceWIth180DegreeBend
      import f._

      When("computing dart directions")
      val faceWithDirections = directions.computeDirection[String](face, AtoB, Right)

      Then("the edge after BtoC should have the same direction as BtoC")
      val f_BtoC = faceWithDirections.getDart(B, C).get
      val f_CtoD = faceWithDirections.getDart(C, D).get
      f_CtoD.direction.get should equal (f_BtoC.direction.get)

    }

    it ("should reverse the direction of a dart the comes after a 360 degree bend") {

      Given("a face with a 360 degree bend after edge CtoF")
      val f = faceWIth360DegreeBend
      import f._

      When("computing dart directions")
      val faceWithDirections = directions.computeDirection[String](face, CtoF, Right)

      Then("the edge after CtoF should have the opposite direction of Right")
      val f_FtoC = faceWithDirections.getDart(F, C).get
      f_FtoC.direction.get should be (Left)

    }

    /*
     *
     *  Tests for computing directions of multiple faces.
     *
     */

    it ("should compute directions for all edges in each face") {

      Given("multiple faces")
      val f = internalAndExternalFace
      import f._

      When("computing dart directions")
      val fSet: Set[OFace[String]] = Set(internalFace, externalFace)
      val rep = new OrthogonalDartRep(fSet, externalFace)
      val directedRepresentation= directions.computeDirection[String](rep, AtoB, Down)

      Then("both faces should have 4 darts")
      val faceA = directedRepresentation.faces.toVector(0)
      val faceB = directedRepresentation.faces.toVector(1)

      faceA.size should be (4)
      faceB.size should be (4)

    }

    it ("should compute opposite directions for opposite darts belonging to different faces") {

      Given("an external and an internal face, sharing the same (but opposite) darts")
      val f = internalAndExternalFace
      import f._

      When("computing dart directions")
      val fSet: Set[OFace[String]] = Set(internalFace, externalFace)
      val rep = new OrthogonalDartRep(fSet, externalFace)
      val directedRepresentation = directions.computeDirection[String](rep, AtoB, Down)

      Then("the dart from one face should have its opposite present in the other face with the opposite direction")
      directedRepresentation.faces should have size (2)
      val faceA = directedRepresentation.faces.toVector(0)
      val faceB = directedRepresentation.faces.toVector(1)

      val dartInFaceA = findDartWithAnyOrder(B, C, faceA)
      val dartInFaceB = findDartWithAnyOrder(B, C, faceB)

      val directionInA = dartInFaceA.direction.get
      val expectedDirectionInB = directionInA.opposite

      dartInFaceB.direction.get should be (expectedDirectionInB)
    }

    it ("should throw an exception if one face contains darts not specified in the other") {

      Given("two faces where one has all darts found in the other, and additional darts on top of that")
      val f = internalAndExternalFace
      import f._
      val F = new VertexWrapper(7, "F")
      val CtoF = new DefaultDart[String](C, F, 2, 0)
      val FtoB = new DefaultDart[String](F, B, 1, 0)
      val externalFaceWithAdditionalDarts = new OFace[String](BtoA, AtoD, DtoC, CtoF, FtoB)

      When("computing dart directions")
      Then("an exception should be thrown")
      val fSet: Set[OFace[String]] = Set(internalFace, externalFaceWithAdditionalDarts)
      val rep = new OrthogonalDartRep(fSet, externalFaceWithAdditionalDarts)
      intercept[Error] {
        val faces = directions.computeDirection[String](rep, AtoB, Down)
      }
    }

  }

  /**
   * Finds a dart with the specified vertices, regardless of which one comes first.
   */
  private def findDartWithAnyOrder(a: VertexWrapper[String], b: VertexWrapper[String], face: OFace[String]): Dart[String] = {
    val result = face.getDart(a, b)
    if (result.isDefined)
      result.get
    else
      face.getDart(b, a).get
  }
}
