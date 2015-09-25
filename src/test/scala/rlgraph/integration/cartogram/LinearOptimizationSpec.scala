package rlgraph.integration.cartogram

import net.cyndeline.rlgraph.cartogram.rectangular.linearProgramming.LinearOptimization
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import rlgraph.SpecImports
import rlgraph.help.ConstraintRoom

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class LinearOptimizationSpec extends SpecImports {
  private val optimizer = new LinearOptimization[ConstraintRoom, UnDiEdge]()
  private val defaultMaxAspectRatio = 3

  describe("LinearOptimization") {

    it ("should shrink a single room to match is targeted area") {

      Given("a single room with size 2x2, drawn using a rectangle with size 3x3")
      val room = ConstraintRoom(2, 2)
      val areaEntry = (room, (0, 0), (2, 2))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(areaEntry), graph)

      When("optimizing the layout")
      val optimizedDrawing = optimizer.optimizeLayout(layout, defaultMaxAspectRatio)

      Then("the rectangle should occupy coordinates between (0,0) and (1, 1)")
      val r = optimizedDrawing.rectangles(room)
      r.startX should be (0)
      r.startY should be (0)
      r.stopX should be (1)
      r.stopY should be (1)

    }

    it ("should shrink a single room only until the point that it meets its target area") {

      Given("a single room with size 3x3, drawn using a rectangle with size 5x5")
      val room = ConstraintRoom(3, 3)
      val areaEntry = (room, (0, 0), (4, 4))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(areaEntry), graph)

      When("optimizing the layout")
      val optimizedDrawing = optimizer.optimizeLayout(layout, defaultMaxAspectRatio)

      Then("the rectangle should occupy coordinates between (0,0) and (2, 2)")
      val r = optimizedDrawing.rectangles(room)
      r.startX should be (0)
      r.startY should be (0)
      r.stopX should be (2)
      r.stopY should be (2)

    }

    it ("should expand a single room to match its targeted area") {

      Given("a single room with size 4x4, drawn using a rectangle with size 2x2")
      val room = ConstraintRoom(4, 4)
      val areaEntry = (room, (0, 0), (1, 1))
      val graph = Graph[ConstraintRoom, UnDiEdge](room)
      val layout = new RectangularLayout(Vector(areaEntry), graph)

      When("optimizing the layout")
      val optimizedDrawing = optimizer.optimizeLayout(layout, defaultMaxAspectRatio)

      Then("the rectangle should occupy coordinates between (0,0) and (3, 3)")
      val r = optimizedDrawing.rectangles(room)
      r.startX should be (0)
      r.startY should be (0)
      r.stopX should be (3)
      r.stopY should be (3)

    }

    it ("should ignore sea rectangles") {

      Given("a layout with two adjacent rooms, with the right being a sea rectangle with target size 100")
      val room = ConstraintRoom(4, 4)
      val seaRectangle = ConstraintRoom(10, 10)
      val areaEntry1 = (room, (0, 0), (1, 1))
      val areaEntry2 = (seaRectangle, (1, 0), (2, 1))
      val graph = Graph[ConstraintRoom, UnDiEdge](room~seaRectangle)
      val layout = new RectangularLayout(Vector(areaEntry1, areaEntry2), graph)

      When("optimizing the layout with aspect ratio 50 (something large)")
      val optimizedDrawing = optimizer.optimizeLayoutWithExceptions(layout, 50, Set(seaRectangle))

      Then("the sea rectangle should only have its have with 1 and height equal to the room")
      val r = optimizedDrawing.rectangles(room)
      val sr = optimizedDrawing.rectangles(seaRectangle)
      sr.startX should be (r.stopX)
      sr.startY should be (0)
      sr.stopX should be (sr.startX + 1)
      sr.stopY should be (r.stopY)

    }

  }

}
