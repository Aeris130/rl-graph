package rlgraph.unit.canonicalOrder.planarBiconnected

import net.cyndeline.rlgraph.canonicalOrder.planarBiconnected.{Contour, Left, Right}
import net.cyndeline.rlgraph.embedding.Vertex
import net.cyndeline.rlgraph.embedding.immutable.UndirectedEmbedding
import rlgraph.SpecImports
import rlgraph.help.GridLayoutData

import scala.language.postfixOps

class ContourSpec extends SpecImports {

  private def twoRows = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2) // Start edge
      .embed(1, 3).embed(3, 4).embed(4, 5).embed(5, 2) // first row
      // Second row
      .embedEdge(Vertex(6) withInsertPosition 2 inVertex 1 withDefaultInsertPosition)
      .embedEdge(Vertex(7) withDefaultPositionInVertex 6 withDefaultInsertPosition)
      .embedEdge(Vertex(4) withDefaultPositionInVertex 7 withInsertPosition 3)
  }

  /* A single row with vertex 3, covered by a second row 4, 6, 5 */
  private def rowCoveringSingleVertex = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(2, 3)
      .embedEdge(Vertex(4) withInsertPosition 2 inVertex 1 withDefaultInsertPosition)
      .embedEdge(Vertex(5) withInsertPosition 3 inVertex 2 withDefaultInsertPosition)
      .embedEdge(Vertex(6) withDefaultPositionInVertex 4 withDefaultInsertPosition)
      .embedEdge(Vertex(6) withDefaultPositionInVertex 5 withDefaultInsertPosition)
  }

  private def twoPaths = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 3).embed(3, 4).embed(2, 5).embed(5, 4)
  }

  private def triangular = new {
    val e = UndirectedEmbedding[Int]().embed(1, 2).embed(2, 3).embed(3, 1)
  }

  describe("Contour") {

    it ("should add a single vertex to the contour") {

      Given("an embedding with the edge 1-2")
      val f = triangular
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the vertex 3")
      val newContour = contour.addVertex(3).newContour

      Then("th new contour should be 1, 3, 2")
      newContour.vertices should be (Seq(1, 3, 2))

    }

    it ("should add a row of unique vertices") {

      Given("an embedding with the path 1, 3, 4, 5, 2")
      val f = GridLayoutData.singleRow
      import f._

      When("adding the vertices 3, 4, 5 to a contour starting with 1, 2")
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(4)
        .addVertex(5).newContour

      Then("the contour should be (1, 4, 5, 2, as v4 has left support and covers v3 with an edge to v1")
      contour.vertices should be (Seq(1, 4, 5, 2))

    }

    it ("should add a cutpoint to the beginning of a row") {

      Given("a contour with a row 1, 3, 2")
      val f = twoRows
      import f._
      val contour = new Contour(1, 2, e)
        .addVertex(3)

      When("adding the vertex 6 to vertex 1")
      val contourWith6 = contour.addVertex(6).newContour

      Then("the new contour should be 1, 6, 3, 2")
      contourWith6.vertices should be (Seq(1, 6, 3, 2))

    }

    it ("should add a cutpoint to the end of a row") {

      Given("a contour with a row 1, 2")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the vertex 5 to vertex 2")
      val contourWith5 = contour.addVertex(5).newContour

      Then("the new contour should be 1, 5, 2")
      contourWith5.vertices should be (Seq(1, 5, 2))

    }

    it ("should add a cutpoint to the middle of a row") {

      Given("a contour with a row 1, 4, 5, 2")
      val f = twoRows
      import f._
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(4)
        .addVertex(5)

      When("adding the vertex 7 to vertex 4")
      val contourWith7 = contour.addVertex(7).newContour

      Then("the new contour should be 1, 4, 7, 5, 2")
      contourWith7.vertices should be (Seq(1, 7, 4, 5, 2))

    }

    it ("should add cutpoints to another cutpoint") {

      Given("an embedding with a row 1, 3, 4, 5, 2")
      val f = twoPaths
      import f._
      val contour = new Contour(1, 2, e)

      When("adding vertex 3 to vertex 1, and vertex 4 to vertex 3")
      val newContour = contour.addVertex(3).addVertex(4).newContour

      Then("the vertex 4 should have left support and be inserted before 3")
      newContour.vertices should be (Seq(1, 4, 3, 2))

    }

    it ("should increase the contour with a vertex having two neighbors") {

      Given("a contour with a row 1, 6, 3, 2 where vertices 6 and 3 has neighbor 7")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(6)

      When("adding vertex 7")
      contour.newContour.contains(7) should be (false)
      val newContour = contour.addVertex(7).newContour

      Then("the new contour should be 1, 6, 7, 3, 2")
      newContour.vertices should be (Seq(1, 6, 7, 3, 2))
      newContour.onContour(7) should be (true)

    }

    it ("should remove vertices from the contour when they're covered") {

      Given("a contour with a row 1, 6, 7, 4, 3, 2 where an external vertex 5 connects to 4 and 2")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(6)
        .addVertex(7)
        .addVertex(4)

      When("adding the vertex 5")
      val newContour = contour.addVertex(5).newContour

      Then("the new contour should be 1, 6, 7, 4, 5, 2")
      newContour.vertices should be (Seq(1, 6, 7, 4, 5, 2))

      And("cover for vertex 3 should be true")
      newContour.onContour(3) should be (false)
      newContour.contains(3) should be (true)

    }

    it ("should cover parts of the current contour when joining two cutpoints") {

      Given("a contour with a vertex 3 connected to v1 and v2, and two paths of cutpoints going from v1 and v2")
      val f = rowCoveringSingleVertex
      import f._
      val contour = new Contour(1, 2, e).addVertex(3).addVertex(4).addVertex(5)

      When("joining vertices 4 and 5 with 6, above 3")
      val newContour = contour.addVertex(6).newContour

      Then("the new contour should cover vertex 3")
      newContour.vertices should be (Seq(1, 4, 6, 5, 2))

      And("cover for vertex 3 should be true")
      newContour.onContour(3) should be (false)
      newContour.contains(3) should be (true)

    }

    it ("should consider a vertex whose only neighbor is v1 to have right support if the vertex clockwise to it is on C(k-1)") {

      Given("a contour 1, 2 and a vertex 3 embedded counter clockwise of v2")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking right support for vertex 3")
      val hasRightSupport = contour.hasRightSupport(3)

      Then("right support should exist")
      hasRightSupport should be (true)

    }

    it ("should not consider a vertex whose only neighbor is v2 to have right support") {

      Given("a contour 1, 2 and a vertex 8 embedded counter clockwise of v1")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking right support for vertex 8")
      val hasRightSupport = contour.hasRightSupport(8)

      Then("no right support should exist")
      hasRightSupport should be (false)

    }

    it ("should not consider a vertex whose only neighbor is v1 to have right support if the vertex clockwise to it isn't on C(k-1)") {

      Given("a contour 1, 2 and a vertex 6 embedded counter clockwise of v1, but separated by vertex 3")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking right support for vertex 6")
      val hasRightSupport = contour.hasRightSupport(6)

      Then("no right support should exist")
      hasRightSupport should be (false)

    }

    it ("should consider a vertex whose only neighbor is v2 to have left support if the vertex counter clockwise to it is on C(k-1)") {

      Given("a contour 1, 2 and a vertex 5 embedded counter clockwise of v1")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking left support for vertex 5")
      val hasLeftSupport = contour.hasLeftSupport(5)

      Then("left support should exist")
      hasLeftSupport should be (true)

    }

    it ("should not consider a vertex whose only neighbor is v1 to have left support") {

      Given("a contour 1, 2 and a vertex 6 embedded clockwise of v2")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking left support for vertex 6")
      val hasLeftSupport = contour.hasLeftSupport(6)

      Then("no left support should exist")
      hasLeftSupport should be (false)

    }

    it ("should not consider a vertex whose only neighbor is v2 to have left support if the vertex counter clockwise to it isn't on C(k-1)") {

      Given("a contour 1, 2 and a vertex 8 embedded clockwise of v2, but separated by vertex 5")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("checking left support for vertex 8")
      val hasLeftSupport = contour.hasLeftSupport(8)

      Then("no left support should exist")
      hasLeftSupport should be (false)

    }

    it ("should not contain a covered vertex") {

      Given("a contour with vertex 3")
      val f = rowCoveringSingleVertex
      import f._
      val contour = new Contour(1, 2, e).addVertex(3).newContour
      assert(contour.onContour(3))

      When("covering vertex 3")
      val covered = contour.addVertex(4).addVertex(5).addVertex(6).newContour

      Then("the contour should not contain vertex 3")
      assert(!covered.onContour(3), "The contour contained vertex 3 even after it had been covered.")

    }

    it ("should return the vertex cover") {

      Given("an embedding with vertices 4 and 5 on the contour")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e).addVertex(3).addVertex(6).addVertex(7).addVertex(4).addVertex(5).newContour

      When("adding a vertex that covers vertex 4 and 5")
      val mod = contour.addVertex(8)

      Then("vertices 4 and 5 should be in the vertex cover")
      mod.vertexCover should be (Seq(4, 5))

    }

    it ("should return the right support if one exists") {

      Given("an embedding with the single row 1-3-4-5-2, and a contour 1-2")
      val f = GridLayoutData.singleRow
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the leftmost vertex 3")
      val mod = contour.addVertex(3)

      Then("the support should be Right")
      mod.support should be ('defined)
      mod.support.get should be (Right)

    }

    it ("should return the left support if one exists") {

      Given("an embedding with the single row 1-3-4-5-2, and a contour 1-2")
      val f = GridLayoutData.singleRow
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the rightmost vertex 5")
      val mod = contour.addVertex(5)

      Then("the support should be Left")
      mod.support should be ('defined)
      mod.support.get should be (Left)

    }

    it ("should not return any support if none is needed") {

      Given("a triangular embedding with the edge 1-2 on the contour")
      val f = triangular
      import f._
      val contour = new Contour(1, 2, e)

      When("adding vertex 3")
      val mod = contour.addVertex(3)

      Then("no left/right support should be specified")
      mod.support should be ('empty)

    }

    it ("should return the left and right neighbors when adding a vertex with two neighbors on C(k-1)") {

      Given("a triangular embedding with the edge 1-2 on the contour")
      val f = triangular
      import f._
      val contour = new Contour(1, 2, e)

      When("adding vertex 3")
      val mod = contour.addVertex(3)

      Then("the left vertex should be 1")
      mod.leftNeighbor should be (1)

      And("the right vertex should be 2")
      mod.rightNeighbor should be (2)

    }

    it ("should return the right support as the right neighbor") {

      Given("an embedding with the single row 1-3-4-5-2, and a contour 1-2")
      val f = GridLayoutData.singleRow
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the leftmost vertex 3")
      val mod = contour.addVertex(3)

      Then("the rightmost support should be the right neighbor")
      mod.rightNeighbor should be (2)

      And("the left neighbor should be the neighbor of 3 on C(k-1)")
      mod.leftNeighbor should be (1)

    }

    it ("should return the left support as the left neighbor") {

      Given("an embedding with the single row 1-3-4-5-2, and a contour 1-2")
      val f = GridLayoutData.singleRow
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the rightmost vertex 5")
      val mod = contour.addVertex(5)

      Then("the leftmost support should be the left neighbor")
      mod.leftNeighbor should be (1)

      And("the right neighbor should be the neighbor of 5 on C(k-1)")
      mod.rightNeighbor should be (2)

    }

    it ("should throw an exception when adding a vertex that doesn't have a neighbor on C(k-1)") {

      Given("a contour 1-2 and an embedding with the edges 1-3-4-5-2")
      val f = GridLayoutData.singleRow
      import f._
      val contour = new Contour(1, 2, e)

      When("adding the vertex 4 without adding one of its neighbors 3 or 5")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        contour.addVertex(4)
      }

    }

    it ("should throw an error when adding a cutpoint vertex that lacks both left and right support") {

      Given("a contour 1-2 and an embedding with the vertex 6 having only one neighbor in C(k-1) and lacking both left and right support")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e)

      When("adding vertex 6 to C")
      Then("an error should be thrown")
      val t = intercept[Error] {
        contour.addVertex(6)
      }

      assert(t.getMessage == "Attempted to add a single vertex to a cutpoint on the contour C(k-1), but the vertex lacked both left and right support.")

    }

    it ("should throw an error if attempting to add a vertex that already exists on the contour") {

      Given("a contour 1, 3, 2")
      val f = GridLayoutData.embedding
      import f._
      val contour = new Contour(1, 2, e).addVertex(3)

      When("adding the vertex 3 again")
      Then("an error should be thrown")
      val t = intercept[IllegalArgumentException] {
        contour.addVertex(3)
      }
      assert(t.getMessage == "requirement failed: Attempted to add vertex 3 twice to C(k-1).")

    }

    it ("should throw an error if attempting to add a vertex that is in G(k-1) but no C(k-1)") {

      Given("a contour with vertex 3 covered")
      val f = rowCoveringSingleVertex
      import f._
      val contour = new Contour(1, 2, e).addVertex(3).addVertex(4).addVertex(5).addVertex(6)

      When("attempting to add vertex 3 again")
      Then("an exception should be thrown")
      val t = intercept[IllegalArgumentException] {
        contour.addVertex(3)
      }
      assert(t.getMessage == "requirement failed: Attempted to add vertex 3 twice to G(k-1).")

    }

    it ("should update it internal embedding when adding vertices with right support") {

      Given("an embedding that can only have its order 1, 2, 3, 4, 5 computed if an edge between 3 and 2 is added after 3 has been added as v(k)")
      val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5).embed(2, 5).embed(3, 4)
        .embedEdge(Vertex(3) withInsertPosition 5 inVertex 1 withDefaultInsertPosition)
        .embedEdge(Vertex(5) withInsertPosition 1 inVertex 3 withInsertPosition 2)
        .embedEdge(Vertex(5) withDefaultPositionInVertex 4 withInsertPosition 2)

      When("adding vertices in the order 1, 2, 3, 4, 5")
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(4)
        .addVertex(5)

      Then("the embedding should be valid")
      // Errors thrown before reaching here if this wasn't the case
      contour.newContour.vertices should be (Seq(1, 5, 2))

    }

    it ("should update it internal embedding when adding vertices with left support") {

      Given("an embedding that can only have its order 1, 2, 3, 4, 5 computed if an edge between 3 and 1 is added after 3 has been added as v(k)")
      val e = UndirectedEmbedding[Int]().embed(1, 2).embed(1, 5).embed(2, 5).embed(3, 4)
        .embedEdge(Vertex(3) withInsertPosition 1 inVertex 2 withDefaultInsertPosition)
        .embedEdge(Vertex(5) withInsertPosition 3 inVertex 4 withInsertPosition 2)
        .embedEdge(Vertex(5) withInsertPosition 4 inVertex 3 withInsertPosition 2)

      When("adding vertices in the order 1, 2, 3, 4, 5")
      val contour = new Contour(1, 2, e)
        .addVertex(3)
        .addVertex(4)
        .addVertex(5)

      Then("the embedding should be valid")
      // Errors thrown before reaching here if this wasn't the case
      contour.newContour.vertices should be (Seq(1, 5, 2))

    }

  }

}
