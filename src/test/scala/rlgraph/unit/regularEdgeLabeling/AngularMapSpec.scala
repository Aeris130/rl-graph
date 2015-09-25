package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.AngularMap
import rlgraph.SpecImports
import rlgraph.help.RegularEdgeLabelingData

class AngularMapSpec extends SpecImports {

  describe("AngularMap") {

    it ("should not add a white vertex representing the outer face") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("no white vertex should be embedded with all four outer vertices as neighbors")
      val whiteVertices = map.angularEmbedding.embeddedVertices.filter(_.isWhite)
      val north_a = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == north).get
      val south_a = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == south).get
      val west_a = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == west).get
      val east_a = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == east).get

      assert(!whiteVertices.exists(v => map.angularEmbedding.embeddingFor(v).iterator.map(_.adjacentVertex).toSet == Set(north_a, west_a, south_a, east_a)))

    }

    it ("should give every white vertex degree 3") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("every white vertex should have degree 3")
      val whiteVertices = map.angularEmbedding.embeddedVertices.filter(_.isWhite)
      assert(!whiteVertices.exists(v => map.angularEmbedding.embeddingFor(v).toVector.size != 3))

    }

    it ("should given every black inner vertex out-degree 4") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("every black inner vertex should have degree 4")
      val outer = Set(north, west, south, east)
      val blackVertices = map.angularEmbedding.embeddedVertices.filter(_.isBlack).filter(v => !outer.contains(v.vertex))
      val veerticesWithInvalidDegree = blackVertices
        .filter(v => map.angularEmbedding.embeddingFor(v).toVector.count(n => map.orientationOfEdge(v, n.adjacentVertex) == n.adjacentVertex) != 4)

      veerticesWithInvalidDegree should be ('empty)

    }

    it ("should give the four outer vertices out-degree 0") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("every black inner vertex should have degree 0")
      val outer = Set(north, west, south, east)
      val verticesWithDegreeHigherThan0 = map.angularEmbedding.embeddedVertices.filter(_.isBlack).filter(v => outer.contains(v.vertex))
        .filter(v => map.angularEmbedding.embeddingFor(v).toVector.count(n => map.orientationOfEdge(v, n.adjacentVertex) == n.adjacentVertex) > 0)

      verticesWithDegreeHigherThan0 should be ('empty)

    }

    it ("should orient a black-white edge towards black if it lies between two edges in T1 in the REL") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("the edge between the white vertex of face 1, 3, 4 and vertex 3 should be oriented to 3")
      val white = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(1, 3, 4)).get
      val black = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == 3).get

      map.orientationOfEdge(white, black) should be (black)

    }

    it ("should orient a black-white edge towards black if it lies between two edges in T2 in the REL") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("the edge between the white vertex of face 6, 1, 2 and vertex 6 should be oriented to 6")
      val white = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(6, 1, 2)).get
      val black = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == 6).get

      map.orientationOfEdge(white, black) should be (black)

    }

    it ("should orient a black-white edge towards white if the edge lies between two edges belonging to different sets") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("the edge between the white vertex of face 1, 3, 4 and vertex 1 should be oriented to 1")
      val white = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(1, 3, 4)).get
      val black = map.angularEmbedding.embeddedVertices.find(v => v.isBlack && v.vertex == 1).get

      map.orientationOfEdge(white, black) should be (white)

    }

    it ("should only assign one outgoing edge from each white vertex not adjacent to the outer face") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("every white vertex should have a single outgoing edge except for the ones in faces adjacent to the outer face")
      val whiteVertices = map.angularEmbedding.embeddedVertices.filter(_.isWhite)
      val outer = Set(labeling.north, labeling.west, labeling.south, labeling.east)
      for (v <- whiteVertices if map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet.intersect(outer).size < 2) {
        val numberOfOutgoingEdges = map.angularEmbedding
          .embeddingFor(v)
          .toVector
          .count(neighbor => map.orientationOfEdge(v, neighbor.adjacentVertex) == neighbor.adjacentVertex)

        numberOfOutgoingEdges should be (1)
      }
    }

    // This out-degree is meaningless, but the article that the map is based upon stores it that way.
    it ("should assign two outgoing edges from each white vertex adjacent to the outer face") {

      Given("a REL")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("the white vertices representing the four faces with edges adjacent to the outer face should have outdegree 2")
      val w1 = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(5, 6, 1)).get
      val w2 = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(5, 4, 7)).get
      val w3 = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(7, 3, 8)).get
      val w4 = map.angularEmbedding.embeddedVertices
        .find(v => map.angularEmbedding.neighborsOf(v).filter(_.isBlack).map(_.vertex).toSet == Set(8, 2, 6)).get

      for (v <- List(w1, w2, w3, w4)) {
        val numberOfOutgoingEdges = map.angularEmbedding
          .embeddingFor(v)
          .toVector
          .count(neighbor => map.orientationOfEdge(v, neighbor.adjacentVertex) == neighbor.adjacentVertex)

        numberOfOutgoingEdges should be (2)
      }
    }

    it ("should map faces of T1+T2 to the vertex that represents them in the angular map") {

      Given("a REL with the edge sets T1 and T2 forming the face 1, 3, 4")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("constructing the angular map")
      val map = new AngularMap(labeling)

      Then("the white vertex representing 1, 3, 4 should be adjacent to 1, 3 and 4 in the map")
      val a1 = map.angularEmbedding.embeddedVertices.find(_.vertex == 1).get
      val a2 = map.angularEmbedding.embeddedVertices.find(_.vertex == 2).get
      val a3 = map.angularEmbedding.embeddedVertices.find(_.vertex == 3).get
      val vertex = map.faceVertex(Face(1, 2, 3))

      vertex.isWhite should be (true)
      map.angularEmbedding.neighborsOf(vertex).toSet should be (Set(a1, a2, a3))

    }
  }
}
