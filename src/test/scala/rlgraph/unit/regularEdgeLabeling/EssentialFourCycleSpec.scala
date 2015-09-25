package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.EssentialFourCycle
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.AngularMap
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, LabelEdge}
import rlgraph.SpecImports
import rlgraph.help.RegularEdgeLabelingData

class EssentialFourCycleSpec extends SpecImports {

  /**
   * The cycle 2, 7, 8, 3, with an inner edge 3->7 in T2.
   */
  private def singleT2EdgeInFaceCycle = new {
    val f = RegularEdgeLabelingData.adjacentFourCycles
    import f._
    val edge = findEdge(labeling) _
    val e2_7 = edge(2, 7)
    val e7_8 = edge(8, 7)
    val e8_3 = edge(3, 8)
    val e3_2 = edge(3, 2)
    val e3_7 = edge(3, 7)
    val cycle = EssentialFourCycle(Vector(e2_7, e7_8, e8_3, e3_2), Vector(e3_7), 0, labeling, angularMap)
  }

  private def eightCycleClockwise = new {
    val f = RegularEdgeLabelingData.adjacentFourCycles
    import f._
    val edge = findEdge(labeling) _
    // Outer
    val e1_2 = edge(1, 2)
    val e3_2 = edge(3, 2)
    val e4_3 = edge(4, 3)
    val e4_1 = edge(4, 1)
    // Inner
    val e3_5 = edge(3, 5)
    val e3_6 = edge(3, 6)
    val e5_1 = edge(5, 1)
    val e6_1 = edge(6, 1)
    val e4_5 = edge(4, 5)
    val e5_6 = edge(5, 6)
    val e6_2 = edge(6, 2)

    val cycle = EssentialFourCycle(Vector(e1_2, e3_2, e4_3, e4_1), Vector(e3_5, e3_6, e5_1, e6_1, e4_5, e5_6, e6_2), 0, labeling, angularMap)
  }

  // This cycle is originally clockwise, so it must be reversed before constructing the map
  private def eightCycleCounterClockwise = new {
    val f = RegularEdgeLabelingData.adjacentFourCycles
    import f._
    val edge = findEdge(labeling) _
    // Outer
    val e1_2 = edge(1, 2)
    val e3_2 = edge(3, 2)
    val e4_3 = edge(4, 3)
    val e4_1 = edge(4, 1)
    // Inner
    val e3_5 = edge(3, 5)
    val e3_6 = edge(3, 6)
    val e5_1 = edge(5, 1)
    val e6_1 = edge(6, 1)
    val e4_5 = edge(4, 5)
    val e5_6 = edge(5, 6)
    val e6_2 = edge(6, 2)
    val innerPreFlip = Vector(e3_5, e3_6, e5_1, e6_1, e4_5, e5_6, e6_2)
    val reverse = Vector(true, true, true, true, false, false, false)

    // Flip all inner edges and reverse only the ones in T2
    val flipped = labeling.flipEdges(innerPreFlip, reverse)
    val flip = findEdge(flipped) _
    // Outer
    val f_e1_2 = flip(1, 2)
    val f_e3_2 = flip(3, 2)
    val f_e4_3 = flip(4, 3)
    val f_e4_1 = flip(4, 1)
    // Inner
    val f_e3_5 = flip(5, 3)
    val f_e3_6 = flip(6, 3)
    val f_e5_1 = flip(1, 5)
    val f_e6_1 = flip(1, 6)
    val f_e4_5 = flip(4, 5)
    val f_e5_6 = flip(5, 6)
    val f_e6_2 = flip(6, 2)

    val map = new AngularMap(flipped)
    val cycle = EssentialFourCycle(Vector(f_e1_2, f_e3_2, f_e4_3, f_e4_1), Vector(f_e3_5, f_e3_6, f_e5_1, f_e6_1, f_e4_5, f_e5_6, f_e6_2), 0, flipped, map)
  }

  describe("EssentialFourCycle") {

    it ("it should construct a left-alternating cycle") {

      Given("a REL with the outer cycle 2, 7, 8, 3, and the inner edge 3, 7 sharing the T-set with its left outer edge 3, 2")
      val f = RegularEdgeLabelingData.adjacentFourCycles
      import f._

      When("constructing a 4-cycle with the outer and inner edges of the cycle")
      val e2_7 = labeling.edges.find(e => e.from == 2 && e.to == 7).get
      val e7_8 = labeling.edges.find(e => e.from == 8 && e.to == 7).get
      val e8_3 = labeling.edges.find(e => e.from == 3 && e.to == 8).get
      val e3_2 = labeling.edges.find(e => e.from == 3 && e.to == 2).get
      val e3_7 = labeling.edges.find(e => e.from == 3 && e.to == 7).get
      val cycle = EssentialFourCycle(Vector(e2_7, e7_8, e8_3, e3_2), Vector(e3_7), 0, labeling, angularMap)

      Then("the cycle should be left-alternating")
      cycle.isLeftAlternating should be (true)
      cycle.isRightAlternating should be (false)

    }

    it ("it should construct a right-alternating cycle") {

      Given("a REL with the outer cycle 1, 2, 3, 4, and the inner edges (3, 5), (3, 6), (5, 1), (6, 1), (4, 5), " +
            "(5, 6) and (6, 2) sharing the T-set with its right outer edge neighbors")
      val f = eightCycleClockwise

      When("constructing a 4-cycle with the outer and inner edges of the cycle")
      val cycle = f.cycle

      Then("the cycle should be right-alternating")
      cycle.isLeftAlternating should be (false)
      cycle.isRightAlternating should be (true)

    }

    it ("should flip itself") {

      Given("a 4-cycle")
      val f = singleT2EdgeInFaceCycle
      import f._

      When("flipping the cycle")
      val flipped = cycle.flip

      Then("the alternating direction should change")
      cycle.isLeftAlternating should not be flipped.isLeftAlternating
      cycle.isRightAlternating should not be flipped.isRightAlternating

    }

  }

  private def findEdge[V](labeling: EdgeLabeling[V])(from: V, to: V): LabelEdge[V] = {
    labeling.edges.find(e => e.from == from && e.to == to).getOrElse(throw new NoSuchElementException("No edge from " + from + " to " + to + "."))
  }
}
