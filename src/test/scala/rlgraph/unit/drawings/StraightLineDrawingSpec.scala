package rlgraph.unit.drawings

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point}
import net.cyndeline.rlgraph.drawings.StraightLineDrawing
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class StraightLineDrawingSpec extends SpecImports {

  describe("StraightLineDrawing") {

    it ("should scale coordinates") {

      Given("a drawing with coordinates (1,1) and (2,3)")
      val drawing = StraightLineDrawing(Graph[Int, UnDiEdge](1, 2), Map(1 -> Point(1, 1), 2 -> Point(2, 3)))

      When("scaling the drawing by 3")
      val scaled = drawing.scale(3)

      Then("vertex 1 should be at (3,3)")
      scaled.coordinates(1).asTuple should be ((3, 3))

      And("vertex 2 should be at (6,9)")
      scaled.coordinates(2).asTuple should be ((6, 9))

    }

    it ("should scale a coordinate based on the center of a rectangle") {

      Given("a drawing with a vertex having coordinate (2,3) and a rectangle of size 3x5")
      val r = Map(1 -> Dimensions(3, 5))
      val drawing = StraightLineDrawing(Graph[Int, UnDiEdge](1), Map(1 -> Point(2, 3)))

      When("scaling the coordinate based on its rectangle")
      val scaled = StraightLineDrawing.rectangleScale(drawing, r)

      Then("the coordinate should be scaled by 5 and adjusted to the center of a rectangle beginning at the scaled coordinate")
      scaled.coordinates(1).asTuple should be ((12, 17))

      And("the width should be 3")
      scaled.width should be (3)

      And("the height should be 5")
      scaled.height should be (5)

    }

    it ("should scale multiple coordinates based on the centre of their rectangles") {

      Given("three vertices at (1,0), (2,1) and (3,3) with rectangles of size 3x3")
      val rd = Dimensions(3, 3)
      val coordinates = Map(1 -> Point(1, 0), 2 -> Point(2, 1), 3 -> Point(3, 3))
      val drawing = StraightLineDrawing(Graph[Int, UnDiEdge](1, 2, 3), coordinates)

      When("scaling the coordinates based on their rectangles")
      val scaled = StraightLineDrawing.rectangleScale(drawing, Map(1 -> rd, 2 -> rd, 3 -> rd))

      Then("vertex 1 should scale to (4,1)")
      scaled.coordinates(1).asTuple should be ((4, 1))

      And("vertex 2 should scale to (7,4)")
      scaled.coordinates(2).asTuple should be ((7, 4))

      Then("vertex 3 should scale to (10,10)")
      scaled.coordinates(3).asTuple should be ((10, 10))

    }

  }

}
