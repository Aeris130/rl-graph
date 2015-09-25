package rlgraph.unit.face

import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import rlgraph.SpecImports
import rlgraph.help.CycleOrderMatcher

class FaceComputationSpec extends SpecImports {
  private val orderMatcher = new CycleOrderMatcher()

  def fixture = new {
    val triangleEmbedding = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(1, 3)

    val triangleBridge = UndirectedEmbedding[Int]()
      .embed(1, 2)
      .embed(1, 3)
      .embed(2, 4, None, Option(1))
      .embed(2, 3, Option(1), Option(4))

    val butterfly = UndirectedEmbedding[Int]()
      .embed(1, 5)
      .embed(1, 3)
      .embed(2, 5, Option(1), None)
      .embed(2, 4, None, Option(5))
      .embed(4, 5, Option(2), Option(2))
      .embed(3, 5, Option(4), Option(1))

    val singleEdge = UndirectedEmbedding[Int]().embed(1, 2)

    /* A butterfly graph with one wing inside the other.
     * 2, 4, 5 is the inner wing, 2, 1, 3 the outer.
     */
    val faceWithin = UndirectedEmbedding[Int]()
      .embed(1, 2)
      .embed(1, 3, None, Option(2))
      .embed(2, 3, Option(1), Option(1))
      .embed(2, 4, None, Option(3))
      .embed(2, 5, None, Option(3))
      .embed(4, 5, Option(2), Option(2))

    val bridgeJoined = UndirectedEmbedding[Int]()
      .embed(1, 2)
      .embed(1, 3, None, Option(2))
      .embed(2, 3, Option(1), Option(1))
      .embed(3, 4, None, Option(1))
      .embed(4, 5, None, Option(3))
      .embed(4, 6, None, Option(5))
      .embed(5, 6, Option(4), Option(4))

    val computation = new FaceComputation[Int]()


  }

  describe("FaceComputation") {

    it ("should compute one internal and one external face for a simple triangular embedding") {

      Given("an embedding representing a triangular graph")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(triangleEmbedding)

      Then("the result should contain two faces")
      result.size should be (2)

      And("both faces should contain the vertices 1, 2 and 3")
      result(0).vertices.toSet should equal (Set(1, 2, 3))
      result(1).vertices.toSet should equal (Set(1, 2, 3))

      And("one of the faces should have vertices in the cyclical order 1, 2, 3")
      val order = Vector(1, 2, 3)
      val oppositeOrder = order.reverse
      val orderMatch = orderMatcher.compares(result(0).vertices, order)
      val oppositeOrderMatch = orderMatcher.compares(result(0).vertices, oppositeOrder)
      assert(orderMatch || oppositeOrderMatch, "Neither order matched the first face")

      And("the other should have the same vertices but in the opposite order")
      if (orderMatch) assert(orderMatcher.compares(result(1).vertices, oppositeOrder))
      else if (oppositeOrderMatch) assert(orderMatcher.compares(result(1).vertices, order))
      else assert(false, "Neither order matched, shouldn't happen.")
    }

    it ("should compute a bridge by repeating a vertex twice") {

      Given("a triangular embedding with a single bridge (vertex 4) sticking out of the graph from edge 2")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(triangleBridge)

      Then("the bridge should appear in one face by vertex transition 2, 4, 2.")

      /* The face with edge 2-4 has more vertices than the other */
      val face = if (result(0).vertices.size > result(1).vertices.size) result(0) else result(1)
      face.vertices.size should be (5)

      /* Opposite order since the edge 2-4 is on the outer face. */
      val order = Vector(3, 2, 4, 2, 1)
      assert(orderMatcher.compares(face.vertices, order), "the face " + face + " doesn't have the same cyclical order as " + order)

    }

    it ("should compute a butterfly graph") {

      Given("a butterfly graph with 5 vertices")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(butterfly)

      Then("the result should contain 3 faces")
      result.size should be (3)

      And("the outer face should have size 6")
      val outerList = result.filter(face => face.vertices.size == 6)
      outerList.size should be (1)
      val outer = outerList(0)

      And("the outer face should contain vertices in the order 1, 5, 2, 4, 5, 3")
      val order = Vector(1, 3, 5, 4, 2, 5)
      assert(orderMatcher.compares(outer.vertices, order), "the face " + outer + " didn't match the cyclical order " + order)

      And("the remaining 2 faces should include every vertex in the graph")
      val remaining = result.filter(face => face.vertices.size == 3)

      remaining.size should be (2)
      (remaining(0).vertices ++ remaining(1).vertices).toSet should equal (Set(1, 2, 3, 4, 5))
      remaining(0).vertices.size should be (3)
      remaining(1).vertices.size should be (3)

      And("the remaining two faces should be 1, 3, 5 and 2, 4, 5")
      val aOrder = Vector(1, 5, 3)
      val bOrder = Vector(2, 4, 5)
      val faceA = if (remaining(0).vertices.contains(1)) remaining(0).vertices else remaining(1).vertices
      val faceB = if (remaining(0).vertices.contains(4)) remaining(0).vertices else remaining(1).vertices

      faceA.toSet intersect faceB.toSet should equal (Set(5))
      assert(orderMatcher.compares(faceA, aOrder), "the face " + faceA + " didn't match the cyclical order " + aOrder)
      assert(orderMatcher.compares(faceB, bOrder), "the face " + faceB + " didn't match the cyclical order " + bOrder)

    }

    it ("should compute the faces of a single edge") {

      Given("a single edge 1-2")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(singleEdge)

      Then("the result should contain a single outer face")
      result.size should be (1)

      And("the face should have the cyclical order 1, 2")
      val order = Vector(1, 2)
      assert(orderMatcher.compares(result(0).vertices, order), "the face " + result(0) + " didn't match the cyclical order " + order)
    }

    it ("should compute a face within a face") {

      Given("A butterfly graph with one wing inside itself")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(faceWithin)

      Then("the result should contain 3 faces")
      result.size should be (3)

      And("the largest face should be bounded and contain all vertices")
      val largestBounded = result.filter(face => face.vertices.size == 6)(0)
      val order = Vector(1, 2, 4, 5, 2, 3)
      assert(orderMatcher.compares(largestBounded.vertices, order), "the face " + largestBounded + " didn't match the cyclical order " + order)

      And("the other bounded face should contain vertices 4, 2, 5")
      val smallestBounded = result.filter(face => face.vertices.size == 3 && face.vertices.contains(4))(0) // 4 not present in outer face
      val smallOrder = Vector(4, 2, 5)
      assert(orderMatcher.compares(smallestBounded.vertices, smallOrder), "the face " + smallestBounded + " didn't match the cyclical order " + smallOrder)

      And("there should be an unbounded vade with vertices 1, 3, 2")
      val unbounded = result.filter(face => face.vertices.size == 3 && face.vertices.contains(1))(0) // 1 not present in smallest bounded face
      val unboundedOrder = Vector(1, 3, 2)
      assert(orderMatcher.compares(unbounded.vertices, unboundedOrder), "the face " + unbounded + " didn't match the cyclical order " + unboundedOrder)

    }

    it("should compute faces for two connected components joined by a bridge") {

      Given("an embedding for two connected components joined by a bridge")
      val f = fixture
      import f._

      When("computing faces for the graph")
      val result = computation.computeFaces(bridgeJoined)

      Then("the result should contain 3 faces")
      result.size should be (3)

      And("the unbounded face should traverse vertices 1, 2, 3, 4, 6, 5, 4, 3")
      val unbounded = result.filter(face => face.vertices.size == 8)(0)
      val order = Vector(1, 2, 3, 4, 6, 5, 4, 3)
      assert(orderMatcher.compares(unbounded.vertices, order), "the face " + unbounded + " didn't match the cyclical order " + order)

      And("the bounded faces should be 1, 3, 2 and 4, 5, 6")
      val faceA = result.filter(face => face.vertices.size == 3 && face.vertices.contains(1))(0)
      val faceB = result.filter(face => face.vertices.size == 3 && face.vertices.contains(4))(0)
      val aOrder = Vector(1, 3, 2)
      val bOrder = Vector(4, 5, 6)
      assert(orderMatcher.compares(faceA.vertices, aOrder), "the face " + faceA + " didn't match the cyclical order " + aOrder)
      assert(orderMatcher.compares(faceB.vertices, bOrder), "the face " + faceB + " didn't match the cyclical order " + bOrder)

    }

    it ("should compute a single face") {

      Given("an embedding with the faces [1, 2, 3, 4], [1, 4, 5] and [5, 4, 3, 2, 1]")
      val embedding = UndirectedEmbedding[Int]()
        .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 1)
        .embedEdge(Vertex(1) withDefaultPositionInVertex 5 withInsertPosition 4)
        .embedEdge(Vertex(4) withInsertPosition 1 inVertex 5 withInsertPosition 3)

      When("computing the single face having edge 1,2")
      val face = new FaceComputation[Int]().computeSingleFace(1, 2, embedding)

      Then("the resulting face should be 1, 2, 3, 4")
      face should be (Face(1, 2, 3, 4))

    }
  }
}
