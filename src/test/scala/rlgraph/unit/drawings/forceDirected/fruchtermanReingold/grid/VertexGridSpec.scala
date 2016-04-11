package rlgraph.unit.drawings.forceDirected.fruchtermanReingold.grid

import net.cyndeline.rlcommon.math.geom.{DPoint, Point}
import net.cyndeline.rlgraph.drawings.forceDirected.fruchtermanReingold.grid.{GridRectangle, VertexGrid}
import rlgraph.SpecImports

class VertexGridSpec extends SpecImports {

  private def singleCoordGrid = VertexGrid(1, 1, 1)

  describe("VertexGrid") {

    it ("should confirm that it is empty") {

      Given("an empty grid")
      val grid = VertexGrid(10, 10, 3)

      When("checking if the grid is empty")
      val emptyStatus = grid.isEmpty

      Then("the grid should be reported as such")
      emptyStatus should be (true)

    }

    it ("should not return that it is empty while a rectangle is in it") {

      Given("a grid with a rectangle")
      val rAndGrid = VertexGrid(30, 30, 3).create(0, Point(19, 19), 11, 5)
      val grid = rAndGrid._2

      When("checking if the grid is empty")
      val emptyStatus = grid.isEmpty

      Then("the grid should be not be empty")
      emptyStatus should be (false)

    }

    it ("should store a single coordinate") {

      Given("a grid of size 1")
      val grid = singleCoordGrid

      When("examining grid dimensions")
      Then("the grid should have width and height 1")
      grid.width should be (1)
      grid.height should be (1)

    }

    it ("should create a rectangle occupying a single coordinate") {

      Given("a grid of size 1")
      val grid = singleCoordGrid

      When("creating a rectangle of size 1")
      val id = 0
      val rAndGrid = grid.create(id, Point(0, 0), 1, 1)
      val r: GridRectangle = rAndGrid._1
      val g: VertexGrid = rAndGrid._2

      Then("the grid should contain a rectangle of size 1x1")
      r.center should be (DPoint(0, 0))
      g.vertices(id) should be (r)

    }

    it ("should store multiple coordinates per cell") {

      Given("a grid of size 264 * 798")
      val grid = VertexGrid(264, 798, 61)

      When("examining grid dimensions")
      Then("the grid should have width 264 and height 798")
      grid.width should be (264)
      grid.height should be (798)

    }

    it ("should create a rectangle occupying a subset of all coordinates") {

      Given("a grid of size 10")
      val grid = VertexGrid(10, 10, 1)

      When("creating a rectangle of size 3 at point (2, 4)")
      val id = 0
      val rAndGrid = grid.create(id, Point(2, 4), 3, 3)

      Then("the rectangles should be stored in the grid")
      rAndGrid._2.vertices(id) should be (rAndGrid._1)

    }

    it ("should create multiple rectangles") {

      Given("an empty grid")
      val grid = VertexGrid(10, 10, 4)

      When("creating multiple rectangles")
      val rag1 = grid.create(0, Point(0, 0), 3, 5)
      val r1 = rag1._1
      val grid1 = rag1._2
      val rag2 = grid1.create(1, Point(2, 6), 7, 3)
      val r2 = rag2._1
      val finalGrid = rag2._2

      Then("the final grid should contain both rectangles")
      finalGrid.vertices(0) should be (r1)
      finalGrid.vertices(1) should be (r2)
      finalGrid.allVertices(Point(0, 0), Point(10, 10)) should be (Vector(r1, r2))

    }

    it ("should move a rectangle") {

      Given("a grid of size 30x30 with cells of size 3 and a rectangle spanning multiple cells at the furthest corner of the grid")
      val id = 0
      val rAndGrid = VertexGrid(30, 30, 3).create(id, Point(19, 19), 11, 5)
      val grid = rAndGrid._2
      val r = rAndGrid._1

      When("moving the rectangles center to the beginning")
      val gridAfterMove = grid.move(r, -19, -19) // as close to (0,0) as possible

      Then("no rectangles should be found after (12,6)")
      val rectanglesInLowerGrid = gridAfterMove.allVertices(Point(12, 6), Point(30, 30))
      rectanglesInLowerGrid should be ('empty)

      And("the created rectangle should be in the upper corner of the grid")
      val upperGrid = gridAfterMove.allVertices(Point(0, 0), Point(11, 5))
      upperGrid should have size 1
      val movedRect = upperGrid.head
      movedRect.id should be (0)
      movedRect.start should be (Point(0, 0))

    }

    it ("should update the vertex map when moving a rectangle") {

      Given("a grid with a rectangle")
      val id = 0
      val rAndGrid = VertexGrid(10, 10, 1).create(id, Point(5, 5), 1, 1)
      val grid = rAndGrid._2
      val r = rAndGrid._1

      When("moving the rectangle to a new coordinate")
      val gridAfterMoving = grid.move(r, -5, -5)

      Then("the rectangle in the vertex map should use the new coordinate")
      gridAfterMoving.vertices(id).start should be (Point(0, 0))
      gridAfterMoving.vertices(id).center should be (DPoint(0, 0))

    }

  }
}
