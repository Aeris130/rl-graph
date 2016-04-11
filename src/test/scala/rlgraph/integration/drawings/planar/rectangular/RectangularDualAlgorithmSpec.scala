package rlgraph.integration.drawings.planar.rectangular

import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.embedding.{Embedding, Vertex}
import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.drawings.planar.rectangular.dataStructure.RVertex
import net.cyndeline.rlgraph.drawings.planar.rectangular.{OuterFaceSelection, RAlgorithmSettings, RectangularDualAlgorithm}
import net.cyndeline.rlgraph.regularEdgeLabeling.factories.CanonicalOrderFactory
import net.cyndeline.rlgraph.regularEdgeLabeling.{RELFactory, RegularEdgeLabeling}
import org.scalamock.scalatest.MockFactory
import rlgraph.SpecImports
import rlgraph.help.RLayoutValidation

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RectangularDualAlgorithmSpec extends SpecImports with MockFactory {
  private val algorithm = RectangularDualAlgorithm.regularAlgorithm[Int, UnDiEdge]

  /** A graph with 4 vertices and it accompanying REL. */
  private def minimalGraphREL = new {
    val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~3)

    val r1 = RVertex(1)
    val r2 = RVertex(2)
    val r3 = RVertex(3)
    val r4 = RVertex(4)
    val north = RVertex.outer[Int]("North")
    val south = RVertex.outer[Int]("South")
    val east = RVertex.outer[Int]("East")
    val west = RVertex.outer[Int]("West")

    val originalEmbedding = UndirectedEmbedding()
      .embed(west, r1).embed(west, r4)
      .embed(south, r4).embed(south, r3)
      .embedEdge(Vertex(r1) withInsertPosition west inVertex r4 withInsertPosition west)
      .embedEdge(Vertex(r1) withDefaultPositionInVertex r2 withInsertPosition west)
      .embedEdge(Vertex(r4) withDefaultPositionInVertex r3 withInsertPosition r1)
      .embedEdge(Vertex(r1) withInsertPosition r4 inVertex r3 withInsertPosition r2)
      .embedEdge(Vertex(r2) withInsertPosition r1 inVertex r3 withDefaultInsertPosition)
      .embedEdge(Vertex(r1) withDefaultPositionInVertex north withInsertPosition west)
      .embedEdge(Vertex(r2) withDefaultPositionInVertex north withInsertPosition r1)
      .embedEdge(Vertex(r2) withDefaultPositionInVertex east withInsertPosition north)
      .embedEdge(Vertex(r3) withDefaultPositionInVertex east withInsertPosition r2)
      .embedEdge(Vertex(south) withInsertPosition r2 inVertex east withInsertPosition r3)
      .embedEdge(Vertex(south) withInsertPosition r4 inVertex west withInsertPosition east)
      .embedEdge(Vertex(north) withInsertPosition south inVertex west withInsertPosition r1)
      .embedEdge(Vertex(north) withInsertPosition r2 inVertex east withInsertPosition west)

    val regularEdgeLabeling = new CanonicalOrderFactory().produceRegularEdgeLabeling(originalEmbedding, north, south, west, east)
  }

  describe("RectangularDualAlgorithm") {

    /*
     * Regular algorithm
     */

    it ("should compute a drawing of one vertex") {

      Given("a graph with one vertex")
      val graph = Graph[Int, UnDiEdge](1)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should compute a drawing of two vertices") {

      Given("a graph with two vertices")
      val graph = Graph(1~2)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should compute a drawing of three vertices") {

      Given("a graph with four vertices")
      val graph = Graph(1~2, 2~3)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should compute a drawing of four vertices") {

      Given("a graph with four vertices")
      val graph = Graph(1~2, 2~3, 3~4)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("Should compute a drawing from a vertex connected to a biconnected component") {

      Given("a cycle 1, 2, 3, 4 and a vertex 5 connected to vertex 1")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should compute a drawing from a maximally planar graph") {

      // The outer face augmentation breaks the requirement that no edge should have 3 common edges

      Given("a triangular graph")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 2~5, 4~5, 1~3, 2~6, 3~6, 4~6, 5~6)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should compute a drawing from a tree") {

      Given("a tree")
      val graph = Graph(1~2, 2~3, 2~4, 3~5, 3~6, 4~7, 7~8)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should select an outer face based on a custom face selection") {

      Given("a graph with faces of size 7, 5 and 4")
      val graph = Graph(1~2, 2~3, 3~4, 4~5, 5~6, 6~7, 7~1, 7~3)

      When("computing a rectangular dual using a customs election that picks a face of size 5")
      val selection = new OuterFaceSelection[Int, UnDiEdge] {
        def select(faces: Vector[Face[Int]], graph: Graph[Int, UnDiEdge]): Face[Int] = faces.find(_.vertexSize == 5).get
      }
      val algorithm = RectangularDualAlgorithm.regularAlgorithmWithSettings(RAlgorithmSettings().withOuterFaceSelect(selection))
      val dual = algorithm.computeLayout(graph)

      Then("the drawing should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should keep gates adjacent to the original neighbors of the split edge") {

      Given("a maximally planar graph causing an internal triangle to be split")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 2~5, 4~5, 1~3, 2~6, 3~6, 4~6, 5~6)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("there should be gates")
      dual.gates should not be 'empty

      And("gates should be adjacent to their neighbors")
      RLayoutValidation.gatesAreAdjacentToNeighbors(dual)

    }

    it ("should compute layouts deterministically") {

      Given("a graph that triggers every algorithm, including biconnection")
      //val graph = Graph(1~2, 2~3, 2~4, 3~5, 3~6, 4~7, 7~8)
      val graph = Graph(4~1, 4~3, 6~4, 7~2, 7~5, 7~6)

      When("computing its rectangular dual 100 times")
      val initialDual = algorithm.computeLayout(graph)

      Then("every dual should equal the first")
      for (i <- 0 until 100) {
        val nextDual = algorithm.computeLayout(graph)
        nextDual should equal (initialDual)
      }

    }

    it ("should compute a layout from a star graph") {

      /* A star graph comes in two cases: A face F with size 4+ where every vertex is adjacent to some vertex V not
       * in F, or a face F with 5+ vertices where a vertex V is in F and every vertex in F is adjacent to V.
       */
      val starGraph1 = Graph(1~2, 2~3, 3~4, 4~1, 1~5, 2~5, 3~5, 4~5)
      val starGraph2 = Graph(1~2, 2~3, 3~4, 4~5, 5~6, 6~1, 3~1, 4~1, 5~1)

      // Should throw an exception if no dual possible
      val dual1 = algorithm.computeLayout(starGraph1)
      val dual2 = algorithm.computeLayout(starGraph2)

    }

    it ("should replace gates between a regular vertex and one of the four outer vertices (N,S,W,E) with dummies") {

      // This might have to change if the algorithm changes
      Given("a graph that results in a gate between an outer and an inner vertex")
      val graph = Graph(3~2, 4~1, 5~3, 6~4, 6~5)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("a dummy should exist")
      assert(dual.allAreas.exists(_.isDummy))
      dual.dummies should have size 1

    }

    it ("should invoke the REL factory during the regular algorithm when one is supplied by the user") {

      Given("a REL factory that produces a REL")
      val f = minimalGraphREL
      import f._
      val factoryMock = mock[RELFactory]
      (factoryMock.produceRegularEdgeLabeling[RVertex[Int]](_: Embedding[RVertex[Int]], _: RVertex[Int], _: RVertex[Int], _: RVertex[Int], _: RVertex[Int])(_: TypeTag[RVertex[Int]], _: ClassTag[RVertex[Int]])) expects(*, *, *, *, *, *, *) returns(regularEdgeLabeling) once()
      val settings = RAlgorithmSettings[Int, UnDiEdge]().withRELFactory(factoryMock)
      val algorithm = RectangularDualAlgorithm.regularAlgorithmWithSettings(settings)

      When("computing the rectangular dual")
      algorithm.computeLayout(graph)

      Then("the factory should be invoked")
      // Error if factory is not invoked

    }

    it ("should produce a drawing using directed edges") {

      Given("a graph with directed edges")
      val graph = Graph[Int, UnDiEdge](0~>1, 0~>5, 1~>4, 3~>1, 1~>2, 4~>3, 4~>2, 6~>4, 2~>5, 2~>7, 7~>6, 6~>5, 7~>8, 8~>9, 9~>7)

      When("computing a rectangular dual")
      val dual = algorithm.computeLayout(graph)

      Then("the resulting layout should be valid")
      RLayoutValidation.layoutIsValid(dual)

    }

    it ("should throw an exception if the user inputs graphs with vertices sharing more than one edge") {

      Given("a graph with edges 1->2 and 2->1")
      val graph = Graph[Int, UnDiEdge](1~>2, 2~>1)

      When("computing a layout for the graph")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        algorithm.computeLayout(graph)
      }

    }

    /*
     * REL algorithm
     */

    it ("should invoke the REL factory during the REL algorithm when one is supplied by the user") {

      Given("a REL factory that produces a REL")
      val f = minimalGraphREL
      import f._
      val factoryMock = mock[RELFactory]
      (factoryMock.produceRegularEdgeLabeling[RVertex[Int]](_: Embedding[RVertex[Int]], _: RVertex[Int], _: RVertex[Int], _: RVertex[Int], _: RVertex[Int])(_: TypeTag[RVertex[Int]], _: ClassTag[RVertex[Int]])) expects(*, *, *, *, *, *, *) returns(regularEdgeLabeling) once()

      When("setting up the initial edge labeling using the factory")
      val settings = RAlgorithmSettings[Int, UnDiEdge]().withRELFactory(factoryMock)
      RectangularDualAlgorithm.edgeLabelAlgorithmWithSettings(graph, settings)

      Then("the factory should be invoked")
      // Error if factory is not invoked

    }

    it ("should invoke the face selection during the REL algorithm") {

      Given("a face selection that produces a face 1, 2, 3, 4")
      val faceMock = mock[OuterFaceSelection[Int, UnDiEdge]]

      // No way to tell which way the embedder will embed this face, may need to switch direction
      (faceMock.select _) expects(*, *) returns (Face(4, 3, 2, 1)) once()

      When("constructing a REL algorithm using a graph with the face 1, 2, 3, 4")
      val settings = RAlgorithmSettings[Int, UnDiEdge]().withOuterFaceSelect(faceMock)
      RectangularDualAlgorithm.edgeLabelAlgorithmWithSettings(Graph(1~2, 2~3, 3~4, 4~1, 1~3), settings)

      Then("the factory should be invoked")
      // Error if factory is not invoked

    }

    it ("should throw an exception if the supplied REL was not derived from the one originally set up during the algorithm construction") {

      Given("a REL algorithm setup")
      val setup = RectangularDualAlgorithm.edgeLabelAlgorithm(Graph(1~2, 2~3, 3~4, 4~1, 1~3))
      val algorithm = setup._1
      val rel = setup._2

      When("attemping to invoke the algorithm using another REL")
      val another = RegularEdgeLabeling[RVertex[Int]](Vector(), Vector(), null, null, null, RVertex(0), RVertex(0), RVertex(0), RVertex(0))

      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        algorithm.computeLayout(another)
      }

    }

    it ("should produce a drawing from a modified version of the initial REL") {

      Given("a REL algorithm setup")
      val setup = RectangularDualAlgorithm.edgeLabelAlgorithm(Graph(1~2, 2~3, 3~4, 4~1, 1~3))
      val algorithm = setup._1
      val rel = setup._2

      When("producing a layout from the REL that results from flipping the edge 1~3")
      val edge = rel.edges.find(e => e.from == RVertex(1) && e.to == RVertex(3)).get
      val flipped = rel.flipEdge(edge, false)
      val newLayout = algorithm.computeLayout(flipped)

      Then("the resulting layout should be valid")
      RLayoutValidation.layoutIsValid(newLayout)

    }

  }

}
