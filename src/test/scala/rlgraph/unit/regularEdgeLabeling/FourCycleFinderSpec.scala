package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.FourCycleFinder
import rlgraph.SpecImports
import rlgraph.help.RegularEdgeLabelingData

class FourCycleFinderSpec extends SpecImports {
  private val cycleFinder = new FourCycleFinder[Int]()

  describe("FourCycleFinder") {

    it ("should find a single four-cycle with no internal vertices") {

      Given("a REL with a face cycle having no internal vertices")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val rel = labeling

      When("parsing 4-cycles")
      val cycles = cycleFinder.findAlternatingFourCycles(rel)

      Then("the cycle 1, 2, 3, 4 should be found")
      val expected = Cycle.undirected(1, 2, 3, 4)
      cycles should have size 1
      cycles.head should be (expected)

    }

    it ("should find a single four-cycle with internal vertices") {

      Given("a REL with a face cycle having internal vertices")
      val f = RegularEdgeLabelingData.essentialEightCycleClockwise
      import f._
      val rel = labeling

      When("parsing 4-cycles")
      val cycles = cycleFinder.findAlternatingFourCycles(rel)

      Then("the cycle 1, 2, 3, 4 should be found")
      val expected = Cycle.undirected(1, 2, 3, 4)
      cycles should have size 1
      cycles.head should be (expected)

    }

    it ("should find four-cycles sharing edges") {

      Given("a REL with two four-cycles 1, 2, 3, 4 and 2, 7, 8, 3 sharing the edge 2, 3")
      val f = RegularEdgeLabelingData.adjacentFourCycles
      import f._
      val rel = labeling

      When("parsing 4-cycles")
      val cycles = cycleFinder.findAlternatingFourCycles(rel)

      Then("the cycle 1, 2, 3, 4 should be found")
      val expected1 = Cycle.undirected(1, 2, 3, 4)
      val expected2 = Cycle.undirected(2, 7, 8, 3)
      cycles should have size 2
      cycles.toSet should be (Set(expected1, expected2))

    }

  }

}
