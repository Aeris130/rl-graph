package rlgraph.unit.rectangular

import net.cyndeline.rlgraph.face.FaceComputation
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.embedding.BhaskerSahniEmbedder
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BhaskerSahniEmbedderSpec extends SpecImports {
  private val embedder = new BhaskerSahniEmbedder[Int]()
  private val faceComp = new FaceComputation[Int]()

  /* Tests circular lists to see if they match each other using some cyclical order (1,2 3 == 2, 3, 1 is true). */
  private val cycleMatcher = new CycleOrderMatcher()

  private def componentWithTwoInternalFaces(a: Int, b: Int, c: Int, d: Int): Graph[Int, UnDiEdge] = {
    Graph(a~b, b~d, d~c, c~a, b~c)
  }

  describe("BhaskerSahniEmbedderSpec") {

    it ("should reject an empty graph") {

      Given("an empty graph")
      val graph = Graph[Int, UnDiEdge]()

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should reject a disconnected graph") {

      Given("a disconnected graph")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1, 1~3, 5)

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should reject a graph with more than one face having length > 3") {

      Given("a graph with an inner and an outer face having length 4")
      val graph = Graph[Int, UnDiEdge](1~2, 2~3, 3~4, 4~1)

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should reject a graph with multiple biconnected components if one of them has two faces of length > 3") {

      Given("a graph with a component having two faces of length > 4")
      val c1 = componentWithTwoInternalFaces(1, 2, 3, 4)
      val invalid = Graph(4~5, 5~6, 6~7, 7~4)
      val graph = c1 ++ invalid

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should reject a graph with an internal vertex with degree < 4") {

      Given("a graph consisting of only vertices with degree 3")
      val graph = Graph(1~2, 2~3, 3~1, 1~4, 2~4, 3~4)

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should reject a graph where all cut points do not appear on the outer face") {

      Given("a graph with two biconnected components having length > 3, with a cutpoint that does not" +
        "appear on that face in component 2")
      val comp1 = Graph(1~2, 2~3, 3~4, 4~1, 5~1, 5~2, 5~3, 5~4) // The face 1, 2, 3, 4 has size > 3, so no face with vertex 5 can be selected as unbounded
      val comp2 = Graph(5~6, 6~7, 7~5) // Connects to the inner vertex 5 in comp1
      val graph = comp1 ++ comp2

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should embed a graph with no internal vertices and a triangular outer face") {

      Given("a triangle")
      val graph = Graph(1~2, 2~3, 3~1)

      When("embedding the graph")
      val embedding = embedder.embed(graph)
      val faces = faceComp.computeFaces(embedding.getOrElse {
        fail("No embedding generated.")
      })

      Then("the faces 1, 2, 3 and 3, 2, 1 should be embedded")
      assert(faces.exists(f => cycleMatcher.compares(Vector(1, 2, 3), f.vertices)))
      assert(faces.exists(f => cycleMatcher.compares(Vector(3, 2, 1), f.vertices)))

    }

    it ("should embed a graph with an internal vertex of degree 4") {

      Given("a graph with an outer face of size 4, with every vertex connecting to an inner vertex")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 5~1, 5~2, 5~3, 5~4)

      When("embedding the graph")
      val embedding = embedder.embed(graph)
      val faces = faceComp.computeFaces(embedding.getOrElse {
        fail("No embedding generated.")
      })

      Then("5 faces should be embedded")
      faces should have size 5

      And("the inner faces should be present")
      val f1 = Vector(1, 2, 5)
      val f2 = Vector(2, 3, 5)
      val f3 = Vector(3, 4, 5)
      val f4 = Vector(4, 1, 5)

      assert(faces.exists(f => cycleMatcher.compareBothDirections(f1, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f2, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f3, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f4, f.vertices)))

      And("the outer face should be embedded")
      assert(faces.exists(f => cycleMatcher.compareBothDirections(Vector(1, 2, 3, 4), f.vertices)))

    }

    it ("should embed a graph with multiple internal vertices of degree 4") {

      Given("a graph with an outer face [1...4] and two internal vertices 5 and 6")
      val graph = Graph(1~2, 2~3, 3~4, 4~1, 5~1, 5~3, 5~4, 5~6, 6~1, 6~2, 6~3)

      When("embedding the graph")
      val embedding = embedder.embed(graph)
      val faces = faceComp.computeFaces(embedding.getOrElse {
        fail("No embedding generated.")
      })

      Then("7 faces should be embedded")
      faces should have size 7

      And("the inner faces should be present")
      val f1 = Vector(1, 2, 6)
      val f2 = Vector(1, 6, 5)
      val f3 = Vector(1, 5, 4)
      val f4 = Vector(5, 3, 4)
      val f5 = Vector(5, 6, 3)
      val f6 = Vector(2, 3, 6)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f1, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f2, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f3, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f4, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f5, f.vertices)))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f6, f.vertices)))

      And("the outer face should be embedded")
      assert(faces.exists(f => cycleMatcher.compareBothDirections(Vector(1, 2, 3, 4), f.vertices)))

    }

  }
}
