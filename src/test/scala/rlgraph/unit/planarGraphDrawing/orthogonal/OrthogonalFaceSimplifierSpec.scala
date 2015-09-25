package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.OrthogonalFaceSimplifier
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{OFace, OrthogonalDartRep, VertexWrapper, VertexWrapperFactory}
import rlgraph.SpecImports

/**
 * Not a lot to test here, most of the simplification is done on individual darts.
 */
class OrthogonalFaceSimplifierSpec extends SpecImports {
  private val wrapperFact = new VertexWrapperFactory[String]()
  private val faceSimplifier = new OrthogonalFaceSimplifier(wrapperFact)

  def squareWithTwoAdjacentFaces = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")

    // Inner face
    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoA = new DefaultDart[String](C, A, 1, 1) // Bends inside

    // Outer face
    val AtoC = new DefaultDart[String](A, C, 3, 0)
    val CtoB = new DefaultDart[String](C, B, 3, 0)
    val BtoA = new DefaultDart[String](B, A, 3, 0)

    val innerFace = new OFace(Vector(AtoB, BtoC, CtoA))
    val outerFace = new OFace(Vector(AtoC, CtoB, BtoA))

    val orthogonalRepresentation = new OrthogonalDartRep[String](Set(innerFace, outerFace), outerFace)
  }

  /* The edge CtoA bends twice inside, and once outside in AtoC */
  def squareWithBendsOnBothSidesOfAnEdge = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")

    // Inner face
    val AtoB = new DefaultDart[String](A, B, 1, 0)
    val BtoC = new DefaultDart[String](B, C, 1, 0)
    val CtoA = new DefaultDart[String](C, A, 1, 2) // 2 bends inside

    // Outer face
    val AtoC = new DefaultDart[String](A, C, 2, 1) // 1 bend outside
    val CtoB = new DefaultDart[String](C, B, 3, 0)
    val BtoA = new DefaultDart[String](B, A, 3, 0)

    val innerFace = new OFace(Vector(AtoB, BtoC, CtoA))
    val outerFace = new OFace(Vector(AtoC, CtoB, BtoA))

    val orthogonalRepresentation = new OrthogonalDartRep[String](Set(innerFace, outerFace), outerFace)
  }

  describe("OrthogonalFaceSimplifier") {

    it ("simplify darts with bends") {

      Given("a graph with a face A->B->C having a single bend between C and A")
      val f = squareWithTwoAdjacentFaces
      import f._

      When("simplifying the face of the orthogonal representation")
      val newFaceAndMapping = faceSimplifier.simplifyFace(innerFace, orthogonalRepresentation)

      Then("the dart after BtoC should have been replaced with two darts with a 90 degree bends between them")
      val face = newFaceAndMapping._1
      val upcomingDarts = face.iteratorFrom(B, C)
      upcomingDarts.next() // gets rid of B->C

      val firstDart = upcomingDarts.next()
      firstDart.from should be (C)
      firstDart.nextDegree should be (1)

      val secondDart = upcomingDarts.next()
      secondDart.from should be (firstDart.to)
      secondDart.to should be (A)

      And("the vertex connecting them should be a dummy")
      assert(!firstDart.to.isDefined, "The vertex " + firstDart.to + " was not a dummy despite being used to simplify a dart.")

    }

    it ("should return a mapping over all edges that were simplified") {

      Given("a graph with a face A->B->C having a single bend between C and A")
      val f = squareWithTwoAdjacentFaces
      import f._

      When("simplifying the face of the orthogonal representation")
      val simplificationMapping = faceSimplifier.simplifyFace(innerFace, orthogonalRepresentation)._2

      Then("the returned mapping should contain 1 entry")
      simplificationMapping.size should be (1)

      And("that entry should be edge C->A")
      simplificationMapping.keySet should contain ((C, A))

    }

    it ("should simplify a darts with bends crossing an edge from both directions in all faces the bends belong to") {

      // Simplify the dart from one side, then take the last of the darts created that way and replace it with the bend-simplifications from the other side
      Given("a graph where the edge CtoA bends twice in the inner face, and the edge AtoC bends once in the outer face")
      val f = squareWithBendsOnBothSidesOfAnEdge
      import f._

      When("simplifying the face of the orthogonal representation")
      val simpleRepresentation = faceSimplifier.simplifyFaces(orthogonalRepresentation)

      Then("the dart CtoA following BtoC in the internal face should consist of 4 darts with bends 1, 1, 3")
      val internalFace = simpleRepresentation.faces.find(f => f.contains(AtoB)).get // AtoB is unchanged, so it can be used for lookups

      internalFace.darts.size should be (6)

      val upcomingInternalDarts = internalFace.iteratorFrom(B, C) // BtoC comes right before CtoA
      upcomingInternalDarts.next() // gets rid of B->C

      val firstInternalDart = upcomingInternalDarts.next()
      firstInternalDart.from should be (C)
      val firstDummy = firstInternalDart.to
      assert(!firstDummy.isDefined, "Expected dart " + firstInternalDart + " to contain a dummy, but " + firstDummy + " was defined.")
      firstInternalDart.nextDegree should be (1)

      val secondInternalDart = upcomingInternalDarts.next()
      secondInternalDart.from should be (firstDummy)
      val secondDummy = secondInternalDart.to
      assert(!secondDummy.isDefined, "Expected dart " + secondInternalDart + " to contain a dummy, but " + secondDummy + " was defined.")
      secondInternalDart.nextDegree should be (1)

      val thirdInternalDart = upcomingInternalDarts.next()
      thirdInternalDart.from should be (secondDummy)
      val thirdDummy = thirdInternalDart.to
      assert(!thirdDummy.isDefined, "Expected dart " + thirdInternalDart + " to contain a dummy, but " + thirdDummy + " was defined.")
      thirdInternalDart.nextDegree should be (3)

      val fourthInternalDart = upcomingInternalDarts.next()
      fourthInternalDart.from should be (thirdDummy)
      fourthInternalDart.to should be (A)
      fourthInternalDart.nextDegree should be (1)

      And("the dart AtoC following BtoA on the outer face should consist of four darts with bends 1, 3, 3")
      val externalFace = simpleRepresentation.faces.find(f => f.contains(BtoA)).get
      externalFace.darts.size should be (6)

      val upcomingExternalDarts = externalFace.iteratorFrom(B, A) // BtoC comes right before CtoA
      upcomingExternalDarts.next() // gets rid of B->A

      // Dart 1
      val firstExternalDart = upcomingExternalDarts.next()
      firstExternalDart.from should be (A)
      val firstExternalDummy = firstExternalDart.to
      assert(!firstExternalDummy.isDefined, "Expected dart " + firstExternalDart + " to contain a dummy, but " + firstExternalDummy + " was defined.")
      firstExternalDart.nextDegree should be (1)

      // Dart 2
      val secondExternalDart = upcomingExternalDarts.next()
      secondExternalDart.from should be (firstExternalDummy)
      val secondExternalDummy = secondExternalDart.to
      assert(!secondExternalDummy.isDefined, "Expected dart " + secondExternalDart + " to contain a dummy, but " + secondExternalDummy + " was defined.")
      secondExternalDart.nextDegree should be (3)

      // Dart 3
      val thirdExternalDart = upcomingExternalDarts.next()
      thirdExternalDart.from should be (secondExternalDummy)
      val thirdExternalDummy = thirdExternalDart.to
      assert(!thirdExternalDummy.isDefined, "Expected dart " + thirdExternalDart + " to contain a dummy, but " + thirdExternalDummy + " was defined.")
      thirdExternalDart.nextDegree should be (3)

      // Dart 4
      val fourthExternalDart = upcomingExternalDarts.next()
      fourthExternalDart.from should be (thirdExternalDummy)
      fourthExternalDart.to should be (C)
      fourthExternalDart.nextDegree should be (2)

    }

    it ("should simplify multiple faces sharing vertices") {

      Given("an internal and external face sharing vertices")
      val f = squareWithTwoAdjacentFaces
      import f._

      When("simplifying all faces")
      val simpleRepresentation = faceSimplifier.simplifyFaces(orthogonalRepresentation)

      Then("the dart CtoA on the internal face should have been replaced with two darts with a 90 degree bends between them")
      val internalFace = simpleRepresentation.faces.find(f => f.contains(AtoB)).get // AtoB is unchanged, so it can be used for lookups

      internalFace.darts.size should be (4)

      val upcomingInternalDarts = internalFace.iteratorFrom(B, C) // BtoC comes right before CtoA
      upcomingInternalDarts.next() // gets rid of B->C

      val firstDart = upcomingInternalDarts.next()
      firstDart.from should be (C)
      firstDart.nextDegree should be (1)

      val secondDart = upcomingInternalDarts.next()
      secondDart.from should be (firstDart.to)
      secondDart.to should be (A)

      And("the edge AtoC should have been replaced with two darts with a 90 degree bend between them")
      val externalFace = simpleRepresentation.faces.find(f => f.contains(BtoA)).get

      externalFace.darts.size should be (4)

      val upcomingExternalDarts = externalFace.iteratorFrom(B, A)
      upcomingExternalDarts.next() // gets rid of B->A

      val firstExDart = upcomingExternalDarts.next()
      firstExDart.from should be (A)
      firstExDart.nextDegree should be (3)

      val secondExDart = upcomingExternalDarts.next()
      secondExDart.from should be (firstExDart.to)
      secondExDart.to should be (C)

    }

    it ("should set the external face in the returned representation") {

      Given("an internal and external face sharing vertices")
      val f = squareWithTwoAdjacentFaces
      import f._

      When("simplifying all faces")
      val simpleRepresentation = faceSimplifier.simplifyFaces(orthogonalRepresentation)

      Then("the external face should be set to the face containing the un-simplified darts of the previous external face")
      val externalFace = simpleRepresentation.faces.find(f => f.contains(BtoA)).get

      simpleRepresentation.externalFace should equal (externalFace)

    }
  }
}
