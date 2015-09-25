package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.network.util.{FlowEdgeAssoc, FlowVertex, NetworkRepresentation}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.OrthogonalFaceFactory
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, OFace, VertexWrapper, VertexWrapperFactory}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class OrthogonalFaceFactorySpec extends SpecImports {
  private implicit def edge2FlowEdgeAssoc[F <: FlowVertex](e: DiEdge[F]) = new FlowEdgeAssoc[F](e)
  private val wrapperFact = new VertexWrapperFactory[Int]()
  private val faceFactory = new OrthogonalFaceFactory(wrapperFact)

  /**
   * A graph with 3 vertices. The only unit of flow that is arbitrary is the single edge-bend
   * required to avoid a diagonal edge. It has been placed in a face-to-face edge that crosses
   * edge 2~>3 in the "original" graph (not constructed here since the face data is all that's needed).
   */
  def triangularGraph = new {
    val innerFace = new Face(Vector(1, 2, 3))
    val outerFace = new Face(Vector(3, 2, 4, 2, 1))

    // Regular vertices, sources
    val v1 = new FlowVertex(1, 4)
    val v2 = new FlowVertex(2, 4)
    val v3 = new FlowVertex(3, 4)
    val v4 = new FlowVertex(4, 4) // <- Bridge on outer face

    // Face-vertices, sinks
    val f1 = new FlowVertex(4, -2) // Inner face, 2 * a(f) - 4
    val f2 = new FlowVertex(5, -14) // Outer face  2 * a(f) + 4

    // Edges between vertices and faces
    val e1 = v1 ~> f1 ## (11, 1, 4, 0, 1)
    val e2 = v1 ~> f2 ## (12, 1, 4, 0, 3)
    val e3 = v2 ~> f1 ## (21, 1, 4, 0, 1)
    val e4 = v2 ~> f2 ## (22, 1, 4, 0, 3)
    val e5 = v3 ~> f1 ## (31, 1, 4, 0, 1)
    val e6 = v3 ~> f2 ## (32, 1, 4, 0, 3)
    val e7 = v4 ~> f2 ## (42, 1, 4, 0, 4)

    // Edges between face vertices
    val e8 = f1 ~> f2 ## (121, 0, -1, 1, 0, Some((1, 2)))
    val e9 = f1 ~> f2 ## (122, 0, -1, 1, 1, Some((2, 3))) // <-- Only one edge will have a bend in a triangular graph
    val e10 = f1 ~> f2 ## (123, 0, -1, 1, 0, Some((3, 1)))
    val e11 = f2 ~> f1 ## (211, 0, -1, 1, 0, Some((3, 2)))
    val e12 = f2 ~> f1 ## (212, 0, -1, 1, 0, Some((2, 1)))
    val e13 = f2 ~> f1 ## (213, 0, -1, 1, 0, Some((1, 3)))
    val e14 = f2 ~> f2 ## (221, 0, -1, 1, 0, Some((2, 4))) // <- Self-loop through bridge))
    val e15 = f2 ~> f2 ## (222, 0, -1, 1, 0, Some((4, 2))) // <- Self-loop through bridge))

    val flowNetwork = Graph.from(List(v1, v2, v3, v4, f1, f2), List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15))

    val networkRepresentation = new NetworkRepresentation[Int]()
    networkRepresentation.graph = flowNetwork
    networkRepresentation.externalFace = f2
    networkRepresentation.vertexMapping += (v1 -> 1)
    networkRepresentation.vertexMapping += (v2 -> 2)
    networkRepresentation.vertexMapping += (v3 -> 3)
    networkRepresentation.vertexMapping += (v4 -> 4)
    networkRepresentation.faceMapping += (f1 -> innerFace)
    networkRepresentation.faceMapping += (f2 -> outerFace)
  }

  describe("OrthogonalFaceFactory") {

    it ("should create a dart for every edge in a face") {

      Given("a face with vertices 1 -> 2 -> 3")
      val f = triangularGraph
      import f._

      When("computing darts from the face")
      val orthoFace = faceFactory.createFace(innerFace, networkRepresentation)

      Then("there should be 3 darts")
      orthoFace.size should be (3)

      And("there should be one dart for edge 1 -> 2")
      assert(containsDart(1, 2, orthoFace), "No dart for edge 1->2 found.")

      And("there should be one dart for edge 2 -> 3")
      assert(containsDart(2, 3, orthoFace), "No dart for edge 2->3 found.")

      And("there should be one dart for edge 3 -> 1")
      assert(containsDart(3, 1, orthoFace), "No dart for edge 3->1 found.")

    }

    it ("should re-use previous wrappers from other faces") {

      Given("an inner face with vertex 1")
      val f = triangularGraph
      import f._

      When("computing darts from the face with the vertex 1 mapped to a wrapper")
      val wrapper = new VertexWrapper[Int](1, 11)
      val orthoFace = faceFactory.createFace(innerFace, networkRepresentation, Map(1 -> wrapper))

      Then("the wrapper for vertex 1 should be the specified wrapper instance")
      assert(orthoFace.vertices.toSet.exists(v => v eq wrapper), "The wrapper for " + wrapper.value + " was not found in the created face despite being specified to be used for the vertex " + wrapper.value)

    }

    it ("should set the vertex-bend between two darts to the value specified by a flow network") {

      Given("a face with vertices 1 -> 2 -> 3, and 1 unit of flow going from vertex 2 to the inner face")
      val f = triangularGraph
      import f._

      When("computing darts from the face")
      val orthoFace = faceFactory.createFace(innerFace, networkRepresentation)

      Then("the dart 1~>2 should have a 1 * 90 degree bend to the next dart 2~>3")
      val dart = findDart(1, 2, orthoFace)
      dart.nextDegree should be (1)

    }

    it ("should set the edge-bend inside a dart to the value specified by a flow network") {

      Given("a face with vertices 1 -> 2 -> 3, and 1 unit of flow going from the inner face to the outer, through edge 2~>3")
      val f = triangularGraph
      import f._

      When("computing darts from the face")
      val orthoFace = faceFactory.createFace(innerFace, networkRepresentation)

      Then("the dart 2~>3 should have 1 edge-bend inside the face")
      val dart = findDart(2, 3, orthoFace)
      dart.bends should be (1)

    }

    it ("should distribute vertex-flow of a single vertex between multiple darts if they make up a cutpoint in the face") {

      Given("a face with a bridge to vertex v4 on the outer face, creating two vertex-angles at vertex 2")
      val f = triangularGraph
      import f._

      When("computing darts from the face")
      val orthoFace = faceFactory.createFace(outerFace, networkRepresentation)

      Then("the darts 3~>2 and 4~>2 should have the combined flow 3 since the flow from v2 to f2 is still 3")
      val d3to2 = findDart(3, 2, orthoFace)
      val d4to2 = findDart(4, 2, orthoFace)
      d3to2.nextDegree + d4to2.nextDegree should be (3)

      And("neither dart should have the entire flow (3) by itself")
      d3to2.nextDegree should not be 3
      d4to2.nextDegree should not be 3

    }

    it ("should create darts for multiple faces") {

      Given("an internal and an external face")
      val f = triangularGraph
      import f._

      When("computing darts for both faces")
      val faceComputation = faceFactory.createFaces(Set[Face[Int]](outerFace, innerFace), outerFace, networkRepresentation)
      val orthoFaces = faceComputation._1

      Then("there should be 3 darts on the internal face")
      val internalFace = orthoFaces.find(f => f.getDart(1, 2).isDefined).get
      internalFace.size should be (3)

      And("there should be one dart for edge 1 -> 2")
      assert(containsDart(1, 2, internalFace), "No dart for edge 1->2 found.")

      And("there should be one dart for edge 2 -> 3")
      assert(containsDart(2, 3, internalFace), "No dart for edge 2->3 found.")

      And("there should be one dart for edge 3 -> 1")
      assert(containsDart(3, 1, internalFace), "No dart for edge 3->1 found.")

      And("there should be 5 darts on the external face")
      val externalFace = orthoFaces.find(f => f.getDart(3, 2).isDefined).get
      externalFace.size should be (5)

      // External face
      And("there should be one dart for edge 3 -> 2")
      assert(containsDart(3, 2, externalFace), "No dart for edge 3->2 found.")

      And("there should be one dart for edge 2 -> 4")
      assert(containsDart(2, 4, externalFace), "No dart for edge 2->4 found.")

      And("there should be one dart for edge 4 -> 2")
      assert(containsDart(4, 2, externalFace), "No dart for edge 4->2 found.")

      And("there should be one dart for edge 2 -> 1")
      assert(containsDart(2, 1, externalFace), "No dart for edge 2->1 found.")

      And("there should be one dart for edge 1 -> 3")
      assert(containsDart(1, 3, externalFace), "No dart for edge 1->3 found.")

      And("vertices used in both faces should use the same wrapper")
      val internalDart = internalFace.getDart(1, 2).get
      val externalDart = externalFace.getDart(2, 1).get
      assert(internalDart.from eq externalDart.to)

    }

    it ("should report the outer face when computing multiple orthogonal faces") {

      Given("an internal and an external face")
      val f = triangularGraph
      import f._

      When("computing darts for both faces")
      val faceComputation = faceFactory.createFaces(Set[Face[Int]](outerFace, innerFace), outerFace, networkRepresentation)
      val orthoFaces = faceComputation._1
      val reportedExternalFace = faceComputation._2
      val externalFace = orthoFaces.find(f => f.getDart(3, 2).isDefined).get

      Then("the fae containing darts from the outer face should be the one reported")
      reportedExternalFace should be (externalFace)

    }
  }

  private def findDart(from: Int, to: Int, oFace: OFace[Int]): Dart[Int] = {
    for (dart <- oFace.darts) {
      if (dart.from.value.get == from && dart.to.value.get == to) return dart
    }

    null
  }

  private def containsDart(from: Int, to: Int, oFace: OFace[Int]): Boolean = findDart(from, to, oFace) != null
}
