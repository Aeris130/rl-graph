package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, OFace, VertexWrapper}
import rlgraph.SpecImports

class OFaceSpec extends SpecImports {

  def faceWithTwoDarts = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")

    val AtoB: Dart[String] = new DefaultDart[String](A, B, 4, 0)
    val BtoA: Dart[String] = new DefaultDart[String](B, A, 4, 0)

    val face = new OFace(Vector(AtoB, BtoA))
  }

  /**
   * Note that face two isn't complete, these two faces don't have
   * enough darts to form the entire graph. They're only used to check
   * adjacency.
   */
  def twoAdjacentFaces = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")

    // Face 1
    val AtoB: Dart[String] = new DefaultDart[String](A, B, 1, 0)
    val BtoC: Dart[String] = new DefaultDart[String](B, C, 1, 0)
    val CtoA: Dart[String] = new DefaultDart[String](C, A, 1, 1)

    // Face 2
    val AtoD: Dart[String] = new DefaultDart[String](A, D, 3, 0)
    val DtoC: Dart[String] = new DefaultDart[String](D, C, 3, 0)
    val CtoB: Dart[String] = new DefaultDart[String](C, B, 3, 0)
    val BtoA: Dart[String] = new DefaultDart[String](B, A, 3, 0)

    val face1 = new OFace(Vector(AtoB, BtoC, CtoA))
    val face2 = new OFace(Vector(AtoD, DtoC, CtoB, BtoA))

  }

  def rectangularAndNonRectagularFaces = new {
    val A = new VertexWrapper[String](1, "A")
    val B = new VertexWrapper[String](2, "B")
    val C = new VertexWrapper[String](3, "C")
    val D = new VertexWrapper[String](4, "D")
    val E = new VertexWrapper[String](5, "E")
    val F = new VertexWrapper[String](6, "F")

    // Rectangular inner face with a flat angle
    val rifwfa_AtoB = new DefaultDart[String](A, B, 1, 0)
    val rifwfa_BtoC = new DefaultDart[String](B, C, 2, 0)
    val rifwfa_CtoD = new DefaultDart[String](C, D, 1, 0)
    val rifwfa_DtoE = new DefaultDart[String](D, E, 1, 0)
    val rifwfa_EtoA = new DefaultDart[String](E, A, 1, 0)

    val face1 = new OFace(rifwfa_AtoB, rifwfa_BtoC, rifwfa_CtoD, rifwfa_DtoE, rifwfa_EtoA)

    // Rectangular outer face with a flat angle
    val rofwfa_AtoB = new DefaultDart[String](A, B, 3, 0)
    val rofwfa_BtoC = new DefaultDart[String](B, C, 2, 0)
    val rofwfa_CtoD = new DefaultDart[String](C, D, 3, 0)
    val rofwfa_DtoE = new DefaultDart[String](D, E, 3, 0)
    val rofwfa_EtoA = new DefaultDart[String](E, A, 3, 0)

    val face2 = new OFace(rofwfa_AtoB, rofwfa_BtoC, rofwfa_CtoD, rofwfa_DtoE, rofwfa_EtoA)

    // Face with both internal, flat and external angles
    val misc_AtoB = new DefaultDart[String](A, B, 1, 0)
    val misc_BtoC = new DefaultDart[String](B, C, 3, 0)
    val misc_CtoD = new DefaultDart[String](C, D, 1, 0)
    val misc_DtoE = new DefaultDart[String](D, E, 2, 0)
    val misc_EtoA = new DefaultDart[String](E, A, 1, 0)

    val face3 = new OFace(misc_AtoB, misc_BtoC, misc_CtoD, misc_DtoE, misc_EtoA)

  }

  describe("OFace") {

    it ("should confirm that a dart is member of the face") {

      Given("a face with two darts")
      val f = faceWithTwoDarts
      import f._

      When("checking for existence of the dart BtoA")
      val existsBasedOnDart = face.contains(BtoA)
      val existsBasedOnVertices = face.contains(B, A)

      Then("the result should be true")
      assert(existsBasedOnDart, "Dart B to A wasn't found based on the dart itself")
      assert(existsBasedOnVertices, "Dart B to A wasn't found based on its vertices")

    }

    it ("should confirm that a dart is not a member of the face") {

      Given("a face with two darts")
      val f = faceWithTwoDarts
      import f._

      When("checking for existence for a dart that doesn't exist")
      val C = new VertexWrapper[String](3, "C")
      val dartThatDoesntExist = new DefaultDart[String](A, C, 4, 0)

      val existsBasedOnDart = face.contains(dartThatDoesntExist)
      val existsBasedOnVertices = face.contains(A, C)

      Then("the result should be true")
      assert(!existsBasedOnDart, "Dart B to A was found based on the dart itself even though it doesn't exist")
      assert(!existsBasedOnVertices, "Dart B to A was found based on its vertices even though it doesn't exist")

    }

    it ("should return a dart based on its vertices") {

      Given("a face with two darts")
      val f = faceWithTwoDarts
      import f._

      When("retrieving a dart with vertice A-B")
      val dart = face.getDart(A, B)

      Then("the result should be defined")
      assert(dart.isDefined, "No dart from A to B was found.")

      And("the dart returned should be AtoB")
      dart.get should be (AtoB)

    }

    it ("should return None when attempting to retrieve a dart that doesn't exist") {

      Given("a face with two darts")
      val f = faceWithTwoDarts
      import f._

      When("checking retrieving a dart that doesn't exist")
      val C = new VertexWrapper[String](3, "C")
      val dart = face.getDart(A, C)

      Then("the result should be None")
      dart should be (None)

    }

    it ("should report size based on the number of darts") {

      Given("a face with two darts")
      val f = faceWithTwoDarts
      import f._

      When("checking the size of the face")
      val size = face.size

      Then("the size should be 2")
      size should be (2)

    }

    it ("should report which set of darts that are shared between two faces, based on their opposites") {

      Given("two faces that share dart A<->B and B<->C (with A->B and B->C in face 1)")
      val f = twoAdjacentFaces
      import f._

      When("checking which darts are adjacent to the ones in face 1")
      val adjacentDarts = face1.edgesAdjacentTo(face2)

      Then("the result should contain two darts")
      adjacentDarts.size should be (2)

      And("the result should contain darts B->A and C->B")
      adjacentDarts should contain (BtoA)
      adjacentDarts should contain (CtoB)

    }

    it ("should create an iterator over the entire face, starting at the first dart") {

      Given("a face with darts AtoB, BtoC, CtoA (face1)")
      val f = twoAdjacentFaces
      import f._

      When("computing an iterator over the entire face starting at the first dart")
      val darts = face1.iteratorFrom(A, B)

      Then("the first dart should be A->B")
      darts.next() should be (AtoB)

      And("the next dart should be B->C")
      darts.next() should be (BtoC)

      And("the final dart should be C->A")
      darts.next() should be (CtoA)
      darts should be ('empty)

    }

    it ("should create an iterator over the entire face, starting at the last dart") {

      Given("a face with darts AtoB, BtoC, CtoA (face1)")
      val f = twoAdjacentFaces
      import f._

      When("computing an iterator over the entire face starting at the last dart")
      val darts = face1.iteratorFrom(C, A)

      Then("the first dart should be C->A")
      darts.next() should be (CtoA)

      And("the next dart should be A->B")
      darts.next() should be (AtoB)

      And("the final dart should be B->C")
      darts.next() should be (BtoC)
      darts should be ('empty)

    }

    it ("should create an iterator over the entire face, starting at a dart between the first and last") {

      Given("a face with darts AtoB, BtoC, CtoA (face1)")
      val f = twoAdjacentFaces
      import f._

      When("computing an iterator over the entire face starting at the middle dart")
      val darts = face1.iteratorFrom(B, C)

      Then("the first dart should be B->C")
      darts.next() should be (BtoC)

      And("the next dart should be C->A")
      darts.next() should be (CtoA)

      And("the final dart should be A->B")
      darts.next() should be (AtoB)
      darts should be ('empty)

    }

    it ("should return an empty iterator when starting at a non-existent vertex") {

      Given("a face with darts AtoB, BtoC, CtoA (face1)")
      val f = twoAdjacentFaces
      import f._

      When("computing an iterator over the entire face starting at a dart that doesn't exist")
      val darts = face1.iteratorFrom(D, C)

      Then("the iterator should be empty")
      darts should be ('empty)

    }

    it ("should throw an exception when attempting to construct a face with a broken chain of vertices") {

      Given("two darts A->B, C->A")
      val A = new VertexWrapper[String](1, "A")
      val B = new VertexWrapper[String](2, "B")
      val C = new VertexWrapper[String](3, "C")
      val d1 = new DefaultDart[String](A, B, 1, 0)
      val d2 = new DefaultDart[String](C, A, 1, 0)

      When("constructing a new face from them")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new OFace(Vector(d1, d2))
      }
    }

    it ("should replace a dart with a list of darts with no bends") {

      Given("a face with a dart BtoC (face1)")
      val f = twoAdjacentFaces
      import f._

      When("replacing the dart with two darts BtoD, DtoC")
      val BtoD = new DefaultDart[String](B, D, 2, 0)
      val DtoC = new DefaultDart[String](D, C, 1, 0)
      val newFace = face1.replaceDart(BtoC, Vector(BtoD, DtoC))

      Then("the darts after the dart before BtoC (AtoB) should be BtoD and DtoC")
      val darts = newFace.iteratorFrom(A, B)
      darts.next() // Get rid if AtoB

      darts.next() should equal (BtoD)
      val nxt = darts.next()
      nxt should equal (DtoC)
      nxt.nextDegree should be (BtoC.nextDegree)

    }

    it ("should replace a subset of darts with another list of darts") {

      Given("a face with multiple darts (face2)")
      val f = twoAdjacentFaces
      import f._

      When("replacing darts from DtoC to BtoA with new darts")
      val E = new VertexWrapper[String](5, "E")
      val nd1 = new DefaultDart[String](D, E, 2, 0)
      val nd2 = new DefaultDart[String](E, A, 2, 0)
      val newDarts = Vector(nd1, nd2)

      val newFace = face2.replaceDartSubset(DtoC, BtoA, newDarts)

      Then("the dart AtoD should be followed by nd1 and nd2")
      val currentFace = newFace.iteratorFrom(AtoD)
      currentFace.next()

      currentFace.next() should be (nd1)
      currentFace.next() should be (nd2)

      And("the replaced darts shouldn't be a part of the face")
      assert(!newFace.contains(DtoC), "DtoC was still in face despite being replaced")
      assert(!newFace.contains(BtoA), "BtoA was still in face despite being replaced")

    }

    it ("should iterate over all vertices in the face") {

      Given("a face with darts")
      val A = new VertexWrapper[String](1, "A")
      val B = new VertexWrapper[String](2, "B")
      val C = new VertexWrapper[String](3, "C")

      val AtoB: Dart[String] = new DefaultDart[String](A, B, 2, 0)
      val BtoC: Dart[String] = new DefaultDart[String](B, C, 4, 0)
      val CtoA: Dart[String] = new DefaultDart[String](C, A, 4, 0)

      val face = new OFace(AtoB, BtoC, CtoA)

      When("iterating over all vertices in the face")
      val vertices: Iterator[VertexWrapper[String]] = face.vertices

      Then("the vertices should appear in the same order as in the face")
      vertices.next() should be (A)
      vertices.next() should be (B)
      vertices.next() should be (C)
      vertices should be ('empty)
    }

    it ("should confirm that it is rectangular if it only contains internal and flat angles") {

      Given("a face with only darts having flat and internal angles")
      val f = rectangularAndNonRectagularFaces
      import f._
      val face = face1

      When("checking if the face is internally rectangular")
      val result = face.isInternallyRectangular

      Then("the result should be true")
      result should be (true)

    }

    it ("should deny that it is rectangular if it is internal and contains external angles") {

      Given("a face with one or more external angles")
      val f = rectangularAndNonRectagularFaces
      import f._
      val face = face3

      When("checking if the face is internally rectangular")
      val result = face.isInternallyRectangular

      Then("the result should be false")
      result should be (false)

    }

    it ("should confirm that it is rectangular if it is external and only contains external and flat angles") {

      Given("a face with only external and flat angles")
      val f = rectangularAndNonRectagularFaces
      import f._
      val face = face2

      When("checking if the face is internally rectangular")
      val result = face.isExternallyRectangular

      Then("the result should be true")
      result should be (true)

    }

    it ("should deny that it is rectangular if it is external and contains internal angles") {

      Given("a face with one or more internal angles")
      val f = rectangularAndNonRectagularFaces
      import f._
      val face = face3

      When("checking if the face is externally rectangular")
      val result = face.isExternallyRectangular

      Then("the result should be false")
      result should be (false)

    }

    it ("should retrieve darts based on defined vertex values") {

      Given("a face with the dart A->B")
      val f = twoAdjacentFaces
      import f._
      val face = face1

      When("retreiving a dart based on vertex values")
      val dart = face.getDart("A", "B")

      Then("the dart should be present")
      assert(dart.isDefined, "No dart A->B found")

      And("the dart should be the dart containing the vertices A and B")
      dart.get should equal (AtoB)

    }

  }
}
