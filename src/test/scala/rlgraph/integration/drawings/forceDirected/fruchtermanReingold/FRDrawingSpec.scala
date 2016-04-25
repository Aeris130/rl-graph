package rlgraph.integration.drawings.forceDirected.fruchtermanReingold

import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.rlgraph.drawings.RVertex
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.{FRDrawing, FRSettings, SizeDisplacement}
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class FRDrawingSpec extends SpecImports {
  private val eps = 1e-3
  private val defaultSettings = FRSettings.defaultSettings
  private val defaultConstant = 2
  private val defaultMaxIterations = 100

  private def withOptimalDistance(d: Int): FRSettings = new FRSettings(defaultMaxIterations, new SizeDisplacement(d, defaultConstant))

  describe("FRDrawing") {

    it ("should process a drawing with width and height 1 containing a single rectangle without affecting it") {

      Given("a 1x1 rectangle and a 1x1 drawing")
      val r = (0, Rectangle(Point(0, 0), 1, 1))
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0), 1, 1, Vector(r), defaultSettings)

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll

      Then("the drawing should contain a single 1x1 rectangle at (0, 0)")
      finalDrawing.computeDrawing.vertices should be (Vector(RVertex(r._1, r._2)))

    }

    it ("should process a drawing where two rectangles overlap and can't be moved") {

      Given("two 1x1 rectangles and a 1x1 drawing")
      val r0 = (0, Rectangle(Point(0, 0), 1, 1))
      val r1 = (1, Rectangle(Point(0, 0), 1, 1))
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0, 1), 1, 1, Vector(r0, r1), defaultSettings)

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll

      Then("the drawing should contain both rectangles at (0, 0)")
      finalDrawing.computeDrawing.vertices should be (Vector(RVertex(r0._1, r0._2), RVertex(r1._1, r1._2)))

    }

    it ("should process a drawing where two overlapping rectangles are neighbors and can't be moved") {

      Given("two 1x1 rectangles and a 1x1 drawing")
      val r0 = (0, Rectangle(Point(0, 0), 1, 1))
      val r1 = (1, Rectangle(Point(0, 0), 1, 1))
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0~1), 1, 1, Vector(r0, r1), defaultSettings)

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll

      Then("the drawing should contain both rectangles at (0, 0)")
      finalDrawing.computeDrawing.vertices should be (Vector(RVertex(r0._1, r0._2), RVertex(r1._1, r1._2)))

    }

    it ("should push two non-connected vertices apart two the drawings border if the optimal distance cannot be achieved") {

      Given("a 11x11 drawing, two 1x1 vertices next to the horizontal middle at (5,0) and an optimal distance of 999")
      val r0 = (0, Rectangle(Point(4, 0), 1, 1))
      val r1 = (1, Rectangle(Point(6, 0), 1, 1))
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0, 1), 11, 11, Vector(r0, r1), withOptimalDistance(999))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("the left rectangle should be pushed to position (0, 0)")
      val left = rectangles.find(r => r.vertex == 0).get.rectangle
      left.start should be (Point(0, 0))

      And("the right rectangle should be pushed to position (10, 10)")
      val right = rectangles.find(r => r.vertex == 1).get.rectangle
      right.start should be (Point(10, 0))

    }

    it ("should push two non-connected vertices apart if they overlap") {

      Given("an 11x11 drawing, two 1x1 vertices in the horizontal middle at (5,0) and an optimal distance of 999")
      val r0 = (0, Rectangle(Point(5, 0), 1, 1))
      val r1 = (1, Rectangle(Point(5, 0), 1, 1))
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0, 1), 11, 11, Vector(r0, r1), withOptimalDistance(999))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("one rectangle should be pushed to position (0, 0)")
      assert(rectangles.exists(r => r.rectangle.start == Point(0, 0)), "No rectangle found at (0, 0) in " + rectangles)

      And("the other should be pushed diagonally to position (10, 10) due to its coordinate being adjusted by (1,1)")
      assert(rectangles.exists(r => r.rectangle.start == Point(10, 10)), "No rectangle found at (10, 10) in " + rectangles)

    }

    it ("should attract two vertices towards the optimal distance") {

      Given("a 100x100 grid and two 1x1 vertices in opposite corners connected by an edge, using optimal distance 10")
      val r0Start = Point(0, 0)
      val r1Start = Point(99, 99)
      val r0 = (0, Rectangle(r0Start, 1, 1))
      val r1 = (1, Rectangle(r1Start, 1, 1))
      val dim = 100
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0~1), dim, dim, Vector(r0, r1), withOptimalDistance(10))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("the distance between both rectangles should be less than the starting value")
      val r0R = rectangles.find(r => r.vertex == 0).get.rectangle
      val r1R = rectangles.find(r => r.vertex == 1).get.rectangle
      val originalDistance = Math.sqrt(99 * 99 + 99 * 99)
      val dx = r0R.start.x - r1R.start.x
      val dy = r0R.start.y - r1R.start.y
      val updatedDistance = Math.sqrt(dx * dx + dy * dy)
      assert(originalDistance >= updatedDistance, "No reduction is distance found between vertices.")
      assert(originalDistance - updatedDistance > eps, "Distance between the connected neighbor pair was not decreased.")

      And("neither vertex should be at its original position")
      r0R.start should not be r0Start
      r1R.start should not be r1Start

    }

    /* This test is a continuation of the one above. The vertices appear in the same order in the edge, but
     * their coordinates are reversed.
     */
    it ("should attract two vertices towards the optimal distance when their coordinates are reversed") {

      Given("a 100x100 grid and two 1x1 vertices in opposite corners connected by an edge, where the positions for each vertex on the edge is reversed from the previous test")
      val r0Start = Point(99, 99)
      val r1Start = Point(0, 0)
      val r0 = (0, Rectangle(r0Start, 1, 1))
      val r1 = (1, Rectangle(r1Start, 1, 1))
      val dim = 100
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0~1), dim, dim, Vector(r0, r1), withOptimalDistance(10))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("the distance between both rectangles should be less than the starting value")
      val r0R = rectangles.find(r => r.vertex == 0).get.rectangle
      val r1R = rectangles.find(r => r.vertex == 1).get.rectangle
      val originalDistance = Math.sqrt(99 * 99 + 99 * 99)
      val dx = r0R.start.x - r1R.start.x
      val dy = r0R.start.y - r1R.start.y
      val updatedDistance = Math.sqrt(dx * dx + dy * dy)
      assert(originalDistance >= updatedDistance, "No reduction is distance found between vertices.")
      assert(originalDistance - updatedDistance > eps, "Distance between the connected neighbor pair was not decreased.")

      And("neither vertex should be at its original position")
      r0R.start should not be r0Start
      r1R.start should not be r1Start

    }

    it ("should not attract two vertices if doing so cause their rectangles to overlap") {

      Given("a 100x100 drawing with two vertices of size 50x100 filling up half the canvas")
      val r0Start = Point(0, 0)
      val r1Start = Point(50, 0)
      val r0 = (0, Rectangle(r0Start, 50, 100))
      val r1 = (1, Rectangle(r1Start, 50, 100))
      val dim = 100
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0~1), dim, dim, Vector(r0, r1), withOptimalDistance(10))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("neither vertex should move")
      val r0R = rectangles.find(r => r.vertex == 0).get.rectangle
      val r1R = rectangles.find(r => r.vertex == 1).get.rectangle
      r0R.start should be (r0Start)
      r1R.start should be (r1Start)

    }

    it ("should repel rectangles of size > 1") {

      Given("a 100x100 drawing with two vertices of size 20x25 / 25x20")
      val r0Start = Point(20, 20)
      val r1Start = Point(60, 61)
      val r0 = (0, Rectangle(r0Start, 20, 25))
      val r1 = (1, Rectangle(r1Start, 25, 20))
      val dim = 100
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0~1), dim, dim, Vector(r0, r1), withOptimalDistance(999))

      When("processing the drawing")
      val finalDrawing = drawing.iterateAll
      val rectangles = finalDrawing.computeDrawing.vertices

      Then("both rectangles should be pushed to the opposite ends of the drawing")
      val r0R = rectangles.find(r => r.vertex == 0).get.rectangle
      val r1R = rectangles.find(r => r.vertex == 1).get.rectangle
      r0R.start should be (Point(0, 0))
      r1R.start + (r1R.width - 1, r1R.height - 1) should be (Point(99, 99))

    }

    it ("should decrease the temperature") {

      Given("a drawing with an initial temperature")
      val r = (0, Rectangle(Point(0, 0), 1, 1))

      // 100x100 in size so that the temperature starts high enough to actually matter.
      val drawing = FRDrawing(Graph[Int, UnDiEdge](0), 100, 100, Vector(r), defaultSettings)

      When("iterating the algorithm")
      val it1 = drawing.iterate(3) // Iterate a number of times to ensure that the temp change is large enough to measure
      val it2 = it1.iterate(3)
      val it3 = it2.iterate(3)

      Then("the temperature should decrease after each iteration")
      assert(it1.temperature < drawing.temperature)
      assert(it2.temperature < it1.temperature)
      assert(it3.temperature < it2.temperature)

    }

  }

}
