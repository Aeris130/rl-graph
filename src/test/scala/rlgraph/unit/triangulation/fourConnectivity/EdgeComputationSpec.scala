package rlgraph.unit.triangulation.fourConnectivity

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import net.cyndeline.rlgraph.face.{Face, FaceComputation}
import net.cyndeline.rlgraph.triangulation.fourConnectivity.help.EdgeComputation
import net.cyndeline.rlgraph.util.GraphCommons
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class EdgeComputationSpec extends SpecImports {
  private val faceComp = new FaceComputation[Int]()

  /**
   * A face with 7 vertices in it. An outer edge is embedded between 3 and 7 to prevent to faces with the same vertex
   * sets from existing. Be sure that it doesn't affect tests.
   */
  private def faceSize7 = new {
    val face = Face(1, 2, 3, 4, 5, 6, 7)
    val v = 1
    val u1 = 1
    val uP = 7
  }

  /**
   * Face with 11 vertices in it. k is not j - 1, and no edge u(j) ~ u(p) exists, so the edge v~j is added.
   * Vertex 10 has an edge to p, making vertex 9 the maximal k.
   */
  private def faceSize11V_JAllowed = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 6).embed(6, 7).embed(7, 8).embed(8, 9).embed(9, 10).embed(10, 11).embed(11, 1)
      .embedEdge(Vertex(10) withInsertPosition 6 inVertex 7 withInsertPosition 9)
    val face = faceComp.computeFaces(embedding).find(_.vertexSize == 11).get
    val v = 1
    val uJ = 7
  }

  /**
   * Face with 11 vertices in it. An edge j to p exists, causing edges from v to u(k) ... u(p-1) to be added.
   * Vertex 10 has an edge to p, making vertex 9 the maximal k.
   */
  private def faceSize11V_JNotAllowed = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 6).embed(6, 7).embed(7, 8).embed(8, 9).embed(9, 10).embed(10, 11).embed(11, 1)
      .embedEdge(Vertex(10) withInsertPosition 6 inVertex 7 withInsertPosition 9)
      .embedEdge(Vertex(11) withInsertPosition 6 inVertex 7 withInsertPosition 10)
    val face = faceComp.computeFaces(embedding).find(_.vertexSize == 11).get
    val v = 1
    val uJ = 7
  }

  /**
   * Used when testing reverse face traversal.
   * Face with 11 vertices in it. No edge u(j) ~ u(p) exists, so the edge v~j is added.
   * Vertex 6 has an edge to 3, making vertex 4 the maximal k.
   */
  private def faceSize11ReversedV_JAllowed = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 6).embed(6, 7).embed(7, 8).embed(8, 9).embed(9, 10).embed(10, 11).embed(11, 1)
      .embedEdge(Vertex(3) withInsertPosition 5 inVertex 6 withInsertPosition 2)

    val face = faceComp.computeFaces(embedding).find(_.vertexSize == 11).get
    val v = 1
    val uJ = 6
  }

  /**
   * Used when testing reverse face traversal.
   * Face with 11 vertices in it. An edge j to p exists, causing edges from v to u(k) ... u(p-1) to be added.
   * Vertex 6 has an edge to 3, making vertex 4 the maximal k.
   */
  private def faceSize11ReversedV_JNotAllowed = new {
    val embedding = UndirectedEmbedding[Int]()
      .embed(1, 2).embed(2, 3).embed(3, 4).embed(4, 5).embed(5, 6).embed(6, 7).embed(7, 8).embed(8, 9).embed(9, 10).embed(10, 11).embed(11, 1)
      .embedEdge(Vertex(3) withInsertPosition 5 inVertex 6 withInsertPosition 2)
      .embedEdge(Vertex(6) withInsertPosition 1 inVertex 2 withInsertPosition 3)

    val face = faceComp.computeFaces(embedding).find(_.vertexSize == 11).get
    val v = 1
    val uJ = 6
  }

  /*
   * Adds aedges from u(k) -> u1 and from v to u(k) ... u(p - 1)
   */
  private def multipleEdgesNeeded = new {
    val embedding = UndirectedEmbedding[Int]()
      .embedEdge(Vertex(8) withDefaultPositionInVertex 5 withDefaultInsertPosition)
      .embedEdge(Vertex(5) withDefaultPositionInVertex 7 withDefaultInsertPosition)
      .embedEdge(Vertex(7) withDefaultPositionInVertex 6 withDefaultInsertPosition)
      .embedEdge(Vertex(6) withDefaultPositionInVertex 1 withDefaultInsertPosition)
      .embedEdge(Vertex(1) withInsertPosition 5 inVertex 8 withInsertPosition 6)
      .embedEdge(Vertex(5) withDefaultPositionInVertex 2 withInsertPosition 8)
      .embedEdge(Vertex(1) withDefaultPositionInVertex 2 withInsertPosition 6)
      .embedEdge(Vertex(7) withInsertPosition 6 inVertex 1 withInsertPosition 5)

    val face = Face(5, 7, 6, 1, 8)
    val v = 8
    val w = 2
    val u1 = 5
    val uP = 1
    val uJ = 7

  }

  describe("EdgeComputation") {

    /*
     *
     *
     * u(j) == v
     *
     *
     */

    it ("should add edges to every vertex and not return a face") {

      Given("a face with 7 vertices with v = 1")
      val f = faceSize7
      import f._

      When("augmenting the face having the neighbor w = 4 and no common neighbor between v and w")
      val augmentation = EdgeComputation.ujEqualsV(v, u1, uP, 4, face, true)

      Then("the edges from 1 to 3 ... 6 should be returned")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(1, 3))
      unordered should contain (UnorderedPair(1, 4))
      unordered should contain (UnorderedPair(1, 5))
      unordered should contain (UnorderedPair(1, 6))

    }

    it ("should skip adding an edge between v and w, and return the remaining face") {

      Given("a face with 7 vertices with v = 1")
      val f = faceSize7
      import f._

      When("augmenting the face having the neighbor w = 4 and a common neighbor between v and w")
      val augmentation = EdgeComputation.ujEqualsV(v, u1, uP, 4, face, false)

      Then("the edges 1 to 3 and 1 to 5,6 should be returned")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(1, 3))
      unordered should contain (UnorderedPair(1, 5))
      unordered should contain (UnorderedPair(1, 6))

    }

    it ("should compute the correct edge additions and faces when u1 -> u(p) traverses the face in reverse") {

      Given("a face with 7 vertices with v = 1, u1 = 7 and u(p) = 2")
      val f = faceSize7
      import f._
      val u1 = 7
      val uP = 2

      When("augmenting the face having the neighbor w = 4 and a common neighbor between v and w")
      val augmentation = EdgeComputation.ujEqualsV(v, u1, uP, 4, face, false)

      Then("the edges 1 to 3 and 1 to 5,6 should be returned")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(1, 3))
      unordered should contain (UnorderedPair(1, 5))
      unordered should contain (UnorderedPair(1, 6))

    }

    /*
     *
     *
     * Adding edges from u1 -> u(j) ... u(j - 2)
     * maximal K.
     *
     * Moving clockwise.
     *
     *
     */

    it ("should add embed new edges from u1 to u(j) ... u(j - 2) and from v to j (if no edge from u(j) to u(p) exists)") {

      Given("a face with 11 vertices, causing the new edges (2,5), (2,6), (2,7), (1,7) to be added")
      val f = faceSize11V_JAllowed
      import f._

      When("augmenting the face using 2 and u1 and 11 as uP")
      val augmentation = EdgeComputation.maximalK(v, 2, 11, uJ, 999, face, embedding)

      Then("the edges 2 to 5 ... 7 and 1 to 7 should be added")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(7, 2))
      unordered should contain (UnorderedPair(7, 3))
      unordered should contain (UnorderedPair(7, 4))
      unordered should contain (UnorderedPair(7, 5))
      unordered should contain (UnorderedPair(1, 7))

    }

    it ("should compute new edges when multiple edges are added between v and u(k) ... u(p-1)") {

      Given("a face with vertex u(p) having an edge to u(j), causing edges from v (1) to u(k) ... u(p-1) to be added")
      val f = faceSize11V_JNotAllowed
      import f._

      When("augmenting the face using 2 and u1 and 11 as uP")
      val augmentation = EdgeComputation.maximalK(v, 2, 11, uJ, 999, face, embedding)

      Then("the edges (2,9), (1,9) and (1,10) should be added")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(2, 9))
      unordered should contain (UnorderedPair(1, 9))
      unordered should contain (UnorderedPair(1, 10))

    }

    it ("should add the edge v~u(j) if u(j) == u(p-1)") {

      Given("a face with vertex u(p) having an edge to u(j)")
      val f = faceSize11V_JNotAllowed
      import f._

      When("augmenting the face using u(p-1) as uJ")
      val newUJ = 10
      val augmentation = EdgeComputation.maximalK(v, 2, 11, newUJ, 999, face, embedding)

      Then("the edges (1,10) should be added")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(1, 10))

    }

    it ("should add an edge from u(j+1) to u1 if the maximal k is u(j)") {

      Given("a face where the maximal k is uJ (7)")
      val f = multipleEdgesNeeded
      import f._

      When("augmenting the face using 5 as u1 and 1 as uP")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, w, face, embedding)

      Then("an edge from 5 to 6 should be added")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(5, 6))

    }

    it ("should compute edges when u1 and u(p) traverses the face in reverse and multiple edges are added between v and u(k) ... u(p-1)") {

      Given("a face with vertex u(p) having an edge to u(j), causing edges from v (1) to u(k) ... u(p-1) to be added")
      val f = faceSize11ReversedV_JNotAllowed
      import f._

      When("augmenting the face using 11 and u1 and 2 as uP")
      val augmentation = EdgeComputation.maximalK(v, 11, 2, uJ, 999, face, embedding)

      Then("the edges (11,4), (1,4) and (1,3) should be added as well as (6,8), (6,9), (6,10), (6,11)")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(6, 8))
      unordered should contain (UnorderedPair(6, 9))
      unordered should contain (UnorderedPair(6, 10))
      unordered should contain (UnorderedPair(6, 11))
      unordered should contain (UnorderedPair(11, 4))
      unordered should contain (UnorderedPair(1, 4))
      unordered should contain (UnorderedPair(1, 3))

    }

    it ("should add embed new edges from u1 to u(j) ... u(j - 2) and from v to j (if no edge from u(j) to u(p) exists) when traversing the face in reverse") {

      Given("a face with 11 vertices and no edge between u(p) and u(j)")
      val f = faceSize11ReversedV_JAllowed
      import f._

      When("augmenting the face using 11 as u1 and 2 as uP")
      val augmentation = EdgeComputation.maximalK(v, 11, 2, uJ, 999, face, embedding)

      Then("the edge (1,6) should be added")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered should contain (UnorderedPair(1, 6))

    }

    it ("should not compute an edge from v to v when uJ != uP-1 or uJ contains an entry for uP") {

      Given("a face F with a common neighbor of u1 and up not on F")
      val f = multipleEdgesNeeded
      import f._

      When("augmenting the face using 5 as u1 and 1 as uP")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, w, face, embedding)

      Then("the edge 8, 8 should not be computed")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered.toSet should not contain UnorderedPair(v, v)

    }

    it ("should not add edges already on the graph when uJ != uP-1 or uJ contains an entry for uP") {

      Given("a face F with a common neighbor of u1 and up not on F")
      val f = multipleEdgesNeeded
      import f._

      When("augmenting the face using 5 as u1 and 1 as uP")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, w, face, embedding)

      Then("the edge 5, 8 should not be computed")
      val unordered = augmentation.edges.map(UnorderedPair(_))
      unordered.toSet should not contain UnorderedPair(u1, v)

    }

    // Trigger spec
    it ("should not introduce additional separating triangles when triangulating a face where every vertex except j and v is connected to w") {

      Given("a graph where every face except 8, 11, 6, 2, 4 is handled using a simple uP~u1 edge")
      val embedding = UndirectedEmbedding[Int]()
        .embed(12, 5).embed(5, 13).embed(13, 7).embed(7, 14).embed(14, 9).embed(9, 15).embed(15, 8).embed(8, 12) // Outer face
        .embedEdge(Vertex(8) withDefaultPositionInVertex 3 withInsertPosition 12)
        .embedEdge(Vertex(3) withInsertPosition 5 inVertex 12 withInsertPosition 8)
        .embedEdge(Vertex(5) withInsertPosition 12 inVertex 3 withInsertPosition 13)
        .embedEdge(Vertex(5) withInsertPosition 14 inVertex 7 withInsertPosition 13)
        .embedEdge(Vertex(5) withDefaultPositionInVertex 10 withInsertPosition 7)
        .embedEdge(Vertex(8) withInsertPosition 5 inVertex 10 withInsertPosition 3)
        .embedEdge(Vertex(7) withInsertPosition 15 inVertex 9 withInsertPosition 14)
        .embedEdge(Vertex(9) withDefaultPositionInVertex 4 withInsertPosition 15)
        .embedEdge(Vertex(4) withInsertPosition 8 inVertex 15 withInsertPosition 9)
        .embedEdge(Vertex(4) withInsertPosition 10 inVertex 8 withInsertPosition 15)
        .embedEdge(Vertex(8) withDefaultPositionInVertex 11 withInsertPosition 10)
        .embedEdge(Vertex(11) withInsertPosition 4 inVertex 9 withInsertPosition 8)
        .embedEdge(Vertex(4) withDefaultPositionInVertex 2 withInsertPosition 8)
        .embedEdge(Vertex(2) withDefaultPositionInVertex 6 withInsertPosition 4)
        .embedEdge(Vertex(6) withInsertPosition 4 inVertex 9 withInsertPosition 2)
        .embedEdge(Vertex(6) withDefaultPositionInVertex 1 withInsertPosition 2)
        .embedEdge(Vertex(1) withInsertPosition 6 inVertex 9 withInsertPosition 6)
        .embedEdge(Vertex(15) withInsertPosition 8 inVertex 12 withInsertPosition 9)
        .embedEdge(Vertex(12) withInsertPosition 5 inVertex 13 withInsertPosition 15)
        .embedEdge(Vertex(13) withInsertPosition 7 inVertex 14 withInsertPosition 12)
        .embedEdge(Vertex(14) withInsertPosition 9 inVertex 15 withInsertPosition 13)

      // Previous triconnection edges
        .embedEdge(Vertex(4) withInsertPosition 9 inVertex 6 withInsertPosition 2)
        .embedEdge(Vertex(11) withInsertPosition 9 inVertex 7 withInsertPosition 8)

      val v = 8
      val u1 = 11
      val uP = 4
      val uJ = 2
      val face = Face(8, 11, 1, 6, 2, 4)

      When("triangulating the face")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, 9, face, embedding)

      Then("the graph should not have separating triangles")
      val graphEdges: Vector[UnDiEdge[Int]] = GraphCommons.embeddingAsGraph(embedding).edges.toVector.map(_.toOuter)
      val graph = Graph.from(Nil, graphEdges ++ augmentation.edges.map(e => e._1~e._2))
      separatingTriangles(graph) should be ('empty)

    }

    it ("should not introduce inner triangles when a face has a common neighbor w in it") {

      Given("the face [8, 7, 6, 1, 4, 2] with an edge between 7 and 4, giving u(1) and u(p) the common neighbor v and 4")
      val embedding = UndirectedEmbedding[Int]()
        .embed(8, 7).embed(7, 6).embed(6, 1).embed(1, 4).embed(4, 2).embed(2, 8)
        .embedEdge(Vertex(7) withInsertPosition 1 inVertex 4 withInsertPosition 8)

      val face = Face(8, 7, 6, 1, 4, 2)
      val v = 8
      val u1 = 2
      val uP = 7
      val uJ = 6

      When("triangulating the face with 6 as the vertex not having any adjacency to 4, and then adding an edge between 7 and 1 in the other face introduced by the extra edge 4~7")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, 4, face, embedding)
      val graphEdges: Vector[UnDiEdge[Int]] = GraphCommons.embeddingAsGraph(embedding).edges.toVector.map(_.toOuter)
      val graph = Graph.from(Nil, graphEdges ++ augmentation.edges.map(e => e._1~e._2)) + 7~1

      Then("the graph should not have separating triangles")
      separatingTriangles(graph) should be ('empty)

    }

    it ("should not add an edge from j to a common neighbor w when w is in face F and between u(1) and u(j - 2) and j and w share a neighbor") {

      Given("the face [12, 13, 5, 11, 6, 3, 51, 9], with v = 12, w = 11 and u1 = 13, having external edges from 11 to u(1) and u(p)")
      val embedding = UndirectedEmbedding[Int]()
        .embed(12, 13).embed(13, 5).embed(5, 11).embed(11, 6).embed(6, 3).embed(3, 51).embed(51, 9).embed(9, 12)
        .embedEdge(Vertex(13) withInsertPosition 5 inVertex 11 withInsertPosition 12)
        .embedEdge(Vertex(9) withInsertPosition 13 inVertex 11 withInsertPosition 51)

      val face = Face(12, 13, 5, 11, 6, 3, 51, 9)
      val v = 12
      val u1 = 13
      val uP = 9
      val uJ = 3
      val w = 11

      When("triangulating the face, causing u1 to become a neighbor of j before the edge j~w is added")
      val augmentation = EdgeComputation.maximalK(v, u1, uP, uJ, w, face, embedding)

      Then("no edge j~w should be added")
      augmentation.edges.toList should not contain ((uJ, w))

    }

  }

  private def separatingTriangles(graph: Graph[Int, UnDiEdge]): Vector[String] = {
    for {
      e <- graph.edges.toVector
      a = e._1
      b = e._2
      commonNeighbors = a.neighbors intersect b.neighbors
      if commonNeighbors.size > 2
    } yield e.toOuter + " has neighbors " + commonNeighbors
  }
}
