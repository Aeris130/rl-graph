package rlgraph.integration.cartogram

import net.cyndeline.rlgraph.cartogram.rectangular.evolution.EvolutionaryOptimization
import net.cyndeline.rlgraph.cartogram.rectangular.evolution.metrics.{AspectRatio, CartographicError}
import net.cyndeline.rlgraph.cartogram.rectangular.segmentHeuristic.SegmentHeuristic
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularDualAlgorithm
import rlgraph.SpecImports
import rlgraph.help.{ConstraintIdRoom, RLayoutValidation}

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class EvolutionaryOptimizationSpec extends SpecImports {

  private def optimizer = new {
    val cartoErrorScore = new CartographicError(1.0)
    val ratioScore = new AspectRatio(1.0, 12)
    val heuristic = new SegmentHeuristic[ConstraintIdRoom, UnDiEdge](5)
    val evoOptimizer = new EvolutionaryOptimization[ConstraintIdRoom, UnDiEdge](20, Vector(cartoErrorScore, ratioScore), heuristic, Set())
  }

  describe("EvolutionaryOptimization") {

    it ("should process a layout") {

      Given("a REL produced from a graph")
      val a = ConstraintIdRoom(3, 3, 0)
      val b = ConstraintIdRoom(3, 3, 1)
      val c = ConstraintIdRoom(3, 3, 2)
      val d = ConstraintIdRoom(3, 3, 3)
      val e = ConstraintIdRoom(3, 3, 4)
      val f = ConstraintIdRoom(3, 3, 5)
      val g = ConstraintIdRoom(3, 3, 6)
      val graph = Graph(a~b, b~c, c~d, d~e, e~f, f~g, g~a)
      val algAndRel = RectangularDualAlgorithm.edgeLabelAlgorithm(graph)

      When("optimizing the layout")
      val fix = optimizer
      import fix._
      val optimizedLayout = evoOptimizer.optimizeLayout(algAndRel._2, algAndRel._1)

      Then("the layout should be valid")
      RLayoutValidation.layoutIsValidIgnoreCorners(optimizedLayout)

    }

  }

}
