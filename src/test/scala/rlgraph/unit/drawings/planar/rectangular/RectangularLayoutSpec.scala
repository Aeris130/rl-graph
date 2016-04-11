package rlgraph.unit.rectangular

import net.cyndeline.rlgraph.drawings.planar.rectangular.RectangularLayout
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RectangularLayoutSpec extends SpecImports {

  describe("RectangularLayout") {

    it ("should report two areas as adjacent if they were adjacent in the original graph") {

      Given("two areas")
      val area1 = (1, (0, 0), (2, 2))
      val area2 = (2, (2, 0), (3, 2))
      val graph = Graph(1~2)

      When("constructing a rectangular layout")
      val layout = new RectangularLayout(Vector(area1, area2), graph)

      Then("area 1 should have area 2 mapped as an adjacent area")
      layout.adjacentAreas(layout.rectangles(1)) should be (Vector(layout.rectangles(2)))

      And("area 2 should have area 1 mapped as an adjacent area")
      layout.adjacentAreas(layout.rectangles(2)) should be (Vector(layout.rectangles(1)))

    }

    it ("should not report two areas as adjacent if they are separated by a gate, despite being adjacent in the original graph") {

      Given("two areas separated by a gate")
      val area1 = (1, (0, 0), (2, 2))
      val gateArea = ((1, 2), (2, 0), (3, 2))
      val area2 = (2, (3, 0), (4, 2))
      val graph = Graph(1~2)

      When("constructing a rectangular layout")
      val layout = new RectangularLayout(Vector(area1, area2), Vector(gateArea), graph)
      val gate = layout.gates.head

      Then("area 1 and 2 should have the gate as its adjacent neighbor")
      layout.adjacentAreas(layout.rectangles(1)) should be (Vector(gate))
      layout.adjacentAreas(layout.rectangles(2)) should be (Vector(gate))

      And("the gate should have area 1 and 2 as its adjacent neighbors")
      layout.adjacentAreas(gate).toSet should be (Set(layout.rectangles(1), layout.rectangles(2)))

    }

    it ("should not report two adjacent areas as adjacent if they're not adjacent in the original graph") {

      Given("two areas adjacent to each other and a graph where area 1 and 2 are only adjacent to area 3")
      val area1 = (1, (0, 0), (2, 2))
      val area2 = (2, (2, 0), (3, 2))
      val area3 = (3, (0, 2), (3, 4))
      val graph = Graph(1~3, 2~3)

      When("constructing a rectangular layout")
      val layout = new RectangularLayout(Vector(area1, area2, area3), graph)

      Then("area 1 and 2 should only have area 3 as a neighbor")
      layout.adjacentAreas(layout.rectangles(1)) should be (Vector(layout.rectangles(3)))
      layout.adjacentAreas(layout.rectangles(2)) should be (Vector(layout.rectangles(3)))

    }

    it ("should adjust the coordinates of dummy gates") {

      Given("a layout with a dummy at position (3, 3), (5, 5)")
      val dummy = ((3, 3), (5, 5))
      val layout = new RectangularLayout(Vector(), Vector(), Vector(dummy), Graph[Int, UnDiEdge]())

      When("adjusting the layouts coordinates by 3, 4")
      val adjusted = layout.adjustCoordinates(3, 4)

      Then("the new coordinates of the dummy should be (6, 7), (8, 9)")
      val r = adjusted.dummies.head
      r.startX should be (6)
      r.startY should be (7)
      r.stopX should be (8)
      r.stopY should be (9)

    }

    it ("should compute min and max coordinates from gates") {

      Given("two gates with minimum and maximum coordinates in the layout")
      val minimumGate = ((1, 2), (0, 0), (1, 1))
      val regularArea1 = (1, (1, 1), (2, 2))
      val regularArea2 = (2, (2, 1), (3, 2))
      val maximumGate = ((1, 2), (2, 2), (4, 4))

      When("constructing a rectangular layout")
      val layout = new RectangularLayout(Vector(regularArea1, regularArea2), Vector(minimumGate, maximumGate), Graph(1~2))

      Then("the minimum coordinates should be the ones of the min gate")
      layout.minX should be (0)
      layout.minY should be (0)

      And("the maximum coordinates should be the ones of the max gate")
      layout.maxX should be (4)
      layout.maxY should be (4)

    }

    it ("should compute min and max coordinates from dummies") {

      Given("two dummies with minimum and maximum coordinates in the layout")
      val minimumDummy = ((0, 0), (1, 1))
      val regularArea1 = (1, (1, 1), (2, 2))
      val regularArea2 = (2, (2, 1), (3, 2))
      val maximumDummy = ((2, 2), (4, 4))

      When("constructing a rectangular layout")
      val layout = new RectangularLayout(Vector(regularArea1, regularArea2), Vector(), Vector(minimumDummy, maximumDummy), Graph(1~2))

      Then("the minimum coordinates should be the ones of the min dummy")
      layout.minX should be (0)
      layout.minY should be (0)

      And("the maximum coordinates should be the ones of the max dummy")
      layout.maxX should be (4)
      layout.maxY should be (4)

    }

    /*
     * Merging dummies into regular vertex areas
     */

    it ("should merge a dummy into a regular rectangle on the x axis") {

      Given("a rectangle with a dummy to its right")
      val rectangleArea = (1, (0, 0), (1, 1))
      val dummy = ((1, 0), (2, 1))
      val layout = new RectangularLayout(Vector(rectangleArea), Vector(), Vector(dummy), Graph[Int, UnDiEdge](1))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("the regular rectangle should occupy both coordinate spaces")
      val r1 = merged.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (2)
      r1.stopY should be (1)

    }

    it ("should merge a dummy into a regular rectangle on the y axis") {

      Given("a rectangle with a dummy to its right")
      val rectangleArea = (1, (0, 0), (1, 1))
      val dummy = ((0, 1), (1, 2))
      val layout = new RectangularLayout(Vector(rectangleArea), Vector(), Vector(dummy), Graph[Int, UnDiEdge](1))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("the regular rectangle should occupy both coordinate spaces")
      val r1 = merged.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (1)
      r1.stopY should be (2)

    }

    it ("should remove dummy areas merged by the layout") {

      Given("a rectangle and a dummy that can merge into it")
      val rectangleArea = (1, (0, 0), (1, 1))
      val dummy = ((1, 0), (2, 1))
      val layout = new RectangularLayout(Vector(rectangleArea), Vector(), Vector(dummy), Graph[Int, UnDiEdge](1))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("only 1 rectangular area should remain")
      merged.allAreas should be (Vector(merged.rectangles(1)))

    }

    it ("should prioritize the x axis over the y axis") {

      Given("a dummy with regular rectangles to its right and below")
      val rectangleRight = (1, (1, 0), (2, 1))
      val rectangleBelow = (2, (0, 1), (1, 2))
      val dummy = ((0, 0), (1, 1))
      val layout = new RectangularLayout(Vector(rectangleRight, rectangleBelow), Vector(), Vector(dummy), Graph[Int, UnDiEdge](1))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("the dummy should share coordinate space with the rectangle to its right")
      val r1 = merged.rectangles(1)
      r1.startX should be (0)
      r1.startY should be (0)
      r1.stopX should be (2)
      r1.stopY should be (1)

      And("the rectangle below should still be in the layout")
      val r2 = merged.rectangles(2)
      r2.startX should be (0)
      r2.startY should be (1)
      r2.stopX should be (1)
      r2.stopY should be (2)

    }

    it ("should prioritize the smaller rectangle when two possible merge candidates exist") {

      Given("a layout with a dummy between two rectangles, with the right rectangle having a smaller area")
      val rectangleLeft = (1, (0, 0), (4, 1))
      val dummy = ((4, 0), (5, 1))
      val rectangleRight = (2, (5, 0), (6, 1))
      val layout = new RectangularLayout(Vector(rectangleLeft, rectangleRight), Vector(), Vector(dummy), Graph[Int, UnDiEdge](1, 2))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("the left rectangle should retain its coordinates")
      val lr = merged.rectangles(1)
      lr.startX should be (0)
      lr.startY should be (0)
      lr.stopX should be (4)
      lr.stopY should be (1)

      And("the right rectangle should cover the dummy's coordinate space")
      val rr = merged.rectangles(2)
      rr.startX should be (4)
      rr.startY should be (0)
      rr.stopX should be (6)
      rr.stopY should be (1)

    }

    it ("should only merge rectangles with one dummy when many are available") {

      Given("a rectangle of size 2 surrounded by four dummies of size 2")
      val north = ((1, 0), (2, 1))
      val west = ((0, 1), (1, 2))
      val south = ((1, 2), (2, 3))
      val east = ((2, 1), (3, 2))
      val middle = (1, (1, 1), (2, 2))
      val layout = new RectangularLayout(Vector(middle), Vector(), Vector(north, west, south, east), Graph[Int, UnDiEdge](1))

      When("merging dummies")
      val merged = layout.mergeDummyRectangles

      Then("three dummies should remain")
      merged.dummies should have size 3

      And("the area of the middle rectangle should be 2")
      val r = merged.rectangles(1)
      (r.stopX - r.startX) * (r.stopY - r.startY) should be (2) // 1*2 or 2*1

      And("the area of the remaining dummies should remain 1")
      for (d <- merged.dummies) {
        (d.stopX - d.startX) * (d.stopY - d.startY) should be (1)
      }
    }

  }
}
