package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.CycleVerifier
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.AngularMap
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, LabelEdge}
import rlgraph.SpecImports
import rlgraph.help.RegularEdgeLabelingData

class CycleVerifierSpec extends SpecImports {
  private val verifier = new CycleVerifier()

  private def validClockwise8Cycle = new {
    val f = RegularEdgeLabelingData.essentialEightCycleClockwise
    import f._
    val map = new AngularMap(labeling)
    val a1 = map.angularEmbedding.embeddedVertices.find(_.vertex == 1).get
    val a2 = map.angularEmbedding.embeddedVertices.find(_.vertex == 2).get
    val a3 = map.angularEmbedding.embeddedVertices.find(_.vertex == 3).get
    val a4 = map.angularEmbedding.embeddedVertices.find(_.vertex == 4).get
    val a5 = map.angularEmbedding.embeddedVertices.find(_.vertex == 5).get
    val a6 = map.angularEmbedding.embeddedVertices.find(_.vertex == 6).get
    val faceVertex1 = map.faceVertex(Face(1, 2, 6))
    val faceVertex2 = map.faceVertex(Face(2, 3, 6))
    val faceVertex3 = map.faceVertex(Face(3, 4, 5))
    val faceVertex4 = map.faceVertex(Face(1, 5, 4))
  }

  private def validCounterClockwise8Cycle = new {
    val f = RegularEdgeLabelingData.essentialEightCycleClockwise
    import f._

    // Labeling needs its inner edges reversed before it becomes CC
    val edges = findEdge(labeling) _
    val e1 = edges(3, 5)
    val e2 = edges(3, 6)
    val e3 = edges(5, 1)
    val e4 = edges(6, 1)
    val e5 = edges(4, 5)
    val e6 = edges(5, 6)
    val e7 = edges(6, 2)
    val flipped = labeling.flipEdges(Vector(e1, e2, e3, e4, e5, e6, e7), Vector(true, true, true, true, false, false, false))
    val map = new AngularMap(flipped)
    val a1 = map.angularEmbedding.embeddedVertices.find(_.vertex == 1).get
    val a2 = map.angularEmbedding.embeddedVertices.find(_.vertex == 2).get
    val a3 = map.angularEmbedding.embeddedVertices.find(_.vertex == 3).get
    val a4 = map.angularEmbedding.embeddedVertices.find(_.vertex == 4).get
    val a5 = map.angularEmbedding.embeddedVertices.find(_.vertex == 5).get
    val a6 = map.angularEmbedding.embeddedVertices.find(_.vertex == 6).get
    val faceVertex1 = map.faceVertex(Face(1, 2, 6))
    val faceVertex2 = map.faceVertex(Face(2, 3, 6))
    val faceVertex3 = map.faceVertex(Face(3, 4, 5))
    val faceVertex4 = map.faceVertex(Face(1, 5, 4))
  }

  describe("CycleVerifier") {

    /*
     *
     * Face cycles, length 4 (B -> W -> B -> W)
     *
     */

    it ("should consider an alternating 4-cycle valid based on a face") {

      Given("an angular map with a cycle 4, 3, 2, 1 corresponding to an essential alternating four-cycle, divided by the edge 1~3")
      val map = new AngularMap(RegularEdgeLabelingData.essentialFaceCycle.labeling)
      val a1 = map.angularEmbedding.embeddedVertices.find(_.vertex == 1).get
      val a3 = map.angularEmbedding.embeddedVertices.find(_.vertex == 3).get
      val faceVertex1 = map.faceVertex(Face(1, 2, 3))
      val faceVertex2 = map.faceVertex(Face(1, 3, 4))

      When("validating the cycle 1 -> f2 -> 3 -> f1")
      val isValid = verifier.verifyFaceCycle[Int](Cycle(faceVertex2, a3, faceVertex1, a1), map)

      Then("the cycle should be valid")
      isValid should be (true)

    }

    it ("should reject an alternating 4-cycle based on a face if the vertices are not supplied in the order they appear in the maps circuit") {

      Given("an angular map with a cycle 4, 3, 2, 1 corresponding to an essential alternating four-cycle, divided by the edge 1~3")
      val map = new AngularMap(RegularEdgeLabelingData.essentialFaceCycle.labeling)
      val a1 = map.angularEmbedding.embeddedVertices.find(_.vertex == 1).get
      val a3 = map.angularEmbedding.embeddedVertices.find(_.vertex == 3).get
      val faceVertex1 = map.faceVertex(Face(1, 3, 4))
      val faceVertex2 = map.faceVertex(Face(1, 2, 3))

      When("validating the cycle 1 -> f2 -> 3 -> f1 using a different order")
      val isValid = verifier.verifyFaceCycle[Int](Cycle(faceVertex1, a1, faceVertex2, a3), map)

      Then("the cycle should be invalid")
      isValid should be (false)

    }

    it ("should reject a face that isn't a 4-cycle") {

      Given("an angular map with a cycle North, 2, 3, 1 being a non-alternating cycle divided by the edge 1~2")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val map = new AngularMap(labeling)
      val a2 = map.angularEmbedding.embeddedVertices.find(_.vertex == 2).get
      val aN = map.angularEmbedding.embeddedVertices.find(_.vertex == north).get
      val faceVertex1 = map.faceVertex(Face(north, 2, 1))
      val faceVertex2 = map.faceVertex(Face(1, 2, 3))

      When("validating the angular cycle North -> f1 -> 2 -> f2")
      val isValid = verifier.verifyFaceCycle[Int](Cycle(aN, faceVertex1, a2, faceVertex2), map)

      Then("the cycle should be invalid")
      isValid should be (false)

    }

  }

  private def findEdge[V](labeling: EdgeLabeling[V])(from: V, to: V): LabelEdge[V] = labeling.edges.find(e => e.from == from && e.to == to).get

}
