package rlgraph.integration.drawings.planar.annealing

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import net.cyndeline.rlgraph.drawings.planar.straightLinePreProcess.centerField.CenterPreProcessing
import rlgraph.SpecImports

class CenterPreProcessingSpec extends SpecImports {

  describe("CenterPreProcessing") {

    it ("should center a vertex using the centroid of its neighbors") {

      Given("four neighbors in a rectangle, with two of them having a vertex v between them")
      val n1 = (1, Point(2, 2))
      val n2 = (2, Point(2, 6))
      val n3 = (3, Point(6, 6))
      val n4 = (4, Point(6, 2))
      val v = (5, Point(2, 3))
      val coordinates = Map(n1, n2, n3, n4, v)
      val drawing = buildDrawing(Vector(1, 2, 3, 4, 5), Vector((1,5), (2,5), (3,5), (4,5)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("a modification should be made")
      centered should be ('defined)

      And("the vertex v should have the centroid coordinate (4,4)")
      centered.get.coordinateFor(v._1) should be (Point(4, 4))

    }

    it ("should not adjust a vertex whose movement results in overlaps between its neighbors") {

      // This test has an invalid initial state

      Given("four collinear verices")
      val n1 = (1, Point(2, 2))
      val n2 = (2, Point(3, 2))
      val n3 = (3, Point(7, 2))
      val v = (4, Point(6, 2)) // Centroid lies to the left
      val coordinates = Map(n1, n2, n3, v)
      val drawing = buildDrawing(Vector(1, 2, 3, 4), Vector((1,4), (2,4), (3,4)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("no modification should be made")
      centered should be (None)

    }

    it ("should reject changes to v that causes edges connected to v to intersect other edges") {

      Given("a vertex v, three neighbors of v with a centroid at (5,5), and a separate segment intersecting the" +
        "edge between v and its lower right neighbor if v moves further than (5,2)")
      val n1 = (1, Point(2, 1))
      val n2 = (2, Point(8, 1))
      val n3 = (3, Point(5, 9))
      val v = (4, Point(5, 1))
      val other1 = (5, Point(4, 2))
      val other2 = (6, Point(2, 4))
      val coordinates = Map(n1, n2, n3, v, other1, other2)
      val drawing = buildDrawing(Vector(1, 2, 3, 4, 5, 6), Vector((1,4), (2,4), (3,4), (5,6)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("v should only be moved to (5,2)")
      centered.get.coordinateFor(v._1) should be (Point(5, 2))

    }

    it ("should not adjust a vertex already at the centroid") {

      Given("a vertex v in the middle between two neighbors")
      val n1 = (1, Point(1, 1))
      val n2 = (2, Point(5, 1))
      val v = (3, Point(3, 1))
      val coordinates = Map(n1, n2, v)
      val drawing = buildDrawing(Vector(1, 2, 3), Vector((1,3), (2,3)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("no modification should be made")
      centered should be (None)

    }

    it ("should not adjust a vertex with no space to move") {

      Given("a vertex between two neighbor coordinates")
      val n1 = (1, Point(1, 1))
      val n2 = (2, Point(3, 1))
      val v = (3, Point(2, 1))
      val coordinates = Map(n1, n2, v)
      val drawing = buildDrawing(Vector(1, 2, 3), Vector((1,3), (2,3)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("no modification should be made")
      centered should be (None)

    }

    it ("should place a vertex with a single neighbor as close to the neighbor as possible") {

      Given("a vertex v at (5,5) and a neighbor at (2,1)")
      val n1 = (1, Point(2, 1))
      val v = (2, Point(5, 5))
      val coordinates = Map(n1, v)
      val drawing = buildDrawing(Vector(1, 2), Vector((1,2)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("v should be placed (rounded up) at (3,2)")
      centered.get.coordinateFor(v._1) should be (Point(3, 2))

    }

    it ("should not move a vertex with a single neighbor if it is right next to the neighbor") {

      Given("a vertex v at (-1,-5) with a single neighbor at (-2,-6)")
      val n1 = (1, Point(7, 4))
      val v = (2, Point(8, 5))
      val coordinates = Map(n1, v)
      val drawing = buildDrawing(Vector(1, 2), Vector((1,2)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("no modification should be made")
      centered should be (None)

    }

    it ("should overlap v or its edges with an edge no connected to v") {

      Given("four neighbors connected to a vertex v, and a non-related edge at its centroid, preventing v from reaching " +
        "the centroid due to vertex overlap, and the point before the centroid due to edge overlap")
      val n1 = (1, Point(2, 9))
      val n2 = (2, Point(8, 9))
      val n3 = (3, Point(2, 3))
      val n4 = (4, Point(8, 3))
      val v = (5, Point(10, 6))
      val other1 = (6, Point(5, 7))
      val other2 = (7, Point(5, 5))
      val coordinates = Map(n1, n2, n3, n4, v, other1, other2)
      val drawing = buildDrawing(Vector(1, 2, 3, 4, 5, 6, 7), Vector((1,5), (2,5), (3,5), (4,5), (6,7)), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("v should only reach (7,6)")
      centered should be ('defined)
      centered.get.coordinateFor(v._1) should be (Point(7, 6))

    }

    it ("should not move a single vertex with no neighbors") {

      Given("a vertex v")
      val v = (1, Point(8, 5))
      val coordinates = Map(v)
      val drawing = buildDrawing(Vector(1), Vector(), coordinates)
      val cPreProcess = CenterPreProcessing(drawing)

      When("centering v")
      val centered = cPreProcess.adjustVertex(v._1)

      Then("no modification should be made")
      centered should be (None)

    }

  }

  private def buildDrawing(vertices: Vector[Int],
                           edges: Vector[(Int, Int)],
                           coordinates: Map[Int, Point]): StraightLineDrawing[Int] = {
    val width = coordinates.maxBy(_._2.x)._2.x
    val height = coordinates.maxBy(_._2.y)._2.y
    new StraightLineDrawing(vertices, edges, coordinates, width, height) // Width is irrelevant
  }

}
