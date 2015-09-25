package rlgraph.integration.regularEdgeLabeling

import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.{AlternatingFourCycles, EssentialFourCycle}
import net.cyndeline.rlgraph.regularEdgeLabeling.{EdgeLabeling, LabelEdge}
import rlgraph.SpecImports
import rlgraph.help.RegularEdgeLabelingData

class AlternatingFourCyclesSpec extends SpecImports {

  describe("AlternatingFourCycles") {

    it ("should detect a 4-cycle with a single internal edge") {

      Given("a REL with an alternating 4-cycle 1, 2, 3, 4 and an internal edge 1, 3")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("one cycle should be detected")
      fourCycles.allCycles should have size 1

      And("that cycle should have outer edges (1, 2), (2, 3), (3, 4), (4, 1)")
      val rel = fourCycles.regularEdgeLabeling
      val cycle = fourCycles.allCycles.head
      val outerEdges = cycle.outerEdges.map(rel.edges(_))
      cycle.outerEdges should have size 4
      assert(outerEdges.exists(e => e.from == 1 && e.to == 2))
      assert(outerEdges.exists(e => e.from == 3 && e.to == 2))
      assert(outerEdges.exists(e => e.from == 4 && e.to == 3))
      assert(outerEdges.exists(e => e.from == 4 && e.to == 1))

      And("the cycle should have the inner edge 1, 3")
      val innerEdges = cycle.innerEdges.map(rel.edges(_))
      innerEdges should have size 1
      assert(innerEdges.exists(e => e.from == 1 && e.to == 3))

    }

    it ("should detect a 4-cycle with multiple internal edges") {

      Given("a REL with an alternating 4-cycle 1, 2, 3, 4 and internal edges (1, 5), (1, 6), (3, 5), (3, 6), (4, 5), (5, 6), (6, 2)")
      val f = RegularEdgeLabelingData.essentialEightCycleCounterClockwise
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("one cycle should be detected")
      fourCycles.allCycles should have size 1

      And("that cycle should have outer edges (1, 2), (2, 3), (3, 4), (4, 1)")
      val rel = fourCycles.regularEdgeLabeling
      val cycle = fourCycles.allCycles.head
      val outerEdges = cycle.outerEdges.map(rel.edges(_))
      cycle.outerEdges should have size 4
      assert(outerEdges.exists(e => e.from == 1 && e.to == 2))
      assert(outerEdges.exists(e => e.from == 3 && e.to == 2))
      assert(outerEdges.exists(e => e.from == 4 && e.to == 3))
      assert(outerEdges.exists(e => e.from == 4 && e.to == 1))

      And("the cycle should have the inner edges (1, 5), (1, 6), (5, 3), (6, 3), (4, 5), (5, 6), (6, 2)")
      val innerEdges = cycle.innerEdges.map(rel.edges(_))
      innerEdges should have size 7
      assert(innerEdges.exists(e => e.from == 1 && e.to == 5))
      assert(innerEdges.exists(e => e.from == 1 && e.to == 6))
      assert(innerEdges.exists(e => e.from == 5 && e.to == 3))
      assert(innerEdges.exists(e => e.from == 6 && e.to == 3))
      assert(innerEdges.exists(e => e.from == 4 && e.to == 5))
      assert(innerEdges.exists(e => e.from == 5 && e.to == 6))
      assert(innerEdges.exists(e => e.from == 6 && e.to == 2))

    }

    it ("should compute inner edges for a counter-clockwise 8-cycle") {

      Given("a counter-clockwise 8-cycle with inner edges (1, 5), (1, 6), (5, 3), (6, 3), (4, 5), (5, 6), (6, 2)")
      val f = RegularEdgeLabelingData.essentialEightCycleCounterClockwise
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("the inner edges should be (1, 5), (1, 6), (5, 3), (6, 3), (4, 5), (5, 6) and (6, 2)")
      val edge = findEdge(labeling) _
      val e1 = edge(1, 5)
      val e2 = edge(1, 6)
      val e3 = edge(5, 3)
      val e4 = edge(6, 3)
      val e5 = edge(4, 5)
      val e6 = edge(5, 6)
      val e7 = edge(6, 2)
      val expected = Set(e1, e2, e3, e4, e5, e6, e7)
      val cycleInner = fourCycles.allCycles.head.innerEdges.map(labeling.edges(_)).toSet
      cycleInner should equal (expected)

    }

    it ("should compute multiple cycles") {

      Given("a REL with two alternating 4-cycles")
      val f = RegularEdgeLabelingData.adjacentFourCycles
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("the inner edge sets of each cycle should be distinct")
      val cycleA = fourCycles.allCycles(0)
      val cycleB = fourCycles.allCycles(1)
      (cycleA.innerEdges intersect cycleB.innerEdges) should be ('empty)

      And("the outer edge 3, 2 should be present in both cycles")
      val c32 = labeling.edges.find(e => e.from == 3 && e.to == 2).get
      (cycleA.outerEdges intersect cycleB.outerEdges) should be (Vector(c32.index))

    }

    it ("should only keep 1 cycle when a clockwise face-cycle and a cc 8-cycle shares edges") {

      Given("a REL with a clockwise face-cycle and a counter-clockwise 8-cycle sharing an edge")
      val f = RegularEdgeLabelingData.nonDisjointOppositeCycles
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("a single cycle should be found")
      fourCycles.allCycles should have size 1

    }

    it ("should assign different indices to each cycle") {

      Given("a REL with two alternating 4-cycles")
      val f = RegularEdgeLabelingData.adjacentFourCycles
      import f._

      When("parsing the essential alternating 4-cycles of the REL")
      val fourCycles = AlternatingFourCycles(labeling)

      Then("two cycles should be present")
      fourCycles.allCycles should have size 2

      And("one cycle should have index 0")
      val castCycles = fourCycles.allCycles.map {
        case ec: EssentialFourCycle[Int] => ec
      }
      assert(castCycles.exists(_.index == 0))
      assert(castCycles.exists(_.index == 1))

    }

    it ("should flip a cycles edges between T1 and T2") {

      Given("a REL with some alternating 4-cycle and its 4-cycle collection")
      val f = RegularEdgeLabelingData.essentialEightCycleClockwise
      import f._
      val fourCycles = AlternatingFourCycles(labeling)
      val rel = fourCycles.regularEdgeLabeling

      When("flipping a cycle")
      val cycle = fourCycles.allCycles.head
      val flipped = fourCycles.flipCycle(cycle)
      val flippedCycle = flipped.allCycles.head

      Then("every inner edge from T1 in the original edge labeling should be in T2")
      for (edge <- cycle.innerEdges.map(rel.edges(_)).filter(fourCycles.regularEdgeLabeling.isMemberOfT1)) {
        val e = flipped.regularEdgeLabeling.edges.find(e => Set(e.to, e.from) == Set(edge.to, edge.from)).get
        flipped.regularEdgeLabeling.isMemberOfT2(e) should be (true)
      }

      And("every inner edge from T2 in the original edge labeling should be in T1")
      for (edge <- cycle.innerEdges.map(rel.edges(_)).filter(fourCycles.regularEdgeLabeling.isMemberOfT2)) {
        val e = flipped.regularEdgeLabeling.edges.find(e => Set(e.to, e.from) == Set(edge.to, edge.from)).get
        flipped.regularEdgeLabeling.isMemberOfT1(e) should be (true)
      }

      And("the flipped cycle corresponding to the old cycle should have its alternating direction changed")
      cycle.isLeftAlternating should not be flippedCycle.isLeftAlternating
      cycle.isRightAlternating should not be flippedCycle.isRightAlternating

    }

    it ("should reverse the edges previously belonging to T2 of a cycle when flipping it if the cycle was clockwise") {

      Given("a REL with an clockwise 8-cycle and edges 4->5, 5->6, 6->2 in T1, and 3->5, 3->6, 5->1 and 6->1 in T2")
      val f = RegularEdgeLabelingData.essentialEightCycleClockwise
      import f._
      val cycles = AlternatingFourCycles(labeling)

      When("flipping the cycle")
      val flipped = cycles.flipCycle(cycles.allCycles.head)

      Then("the edges 4->5, 5->6, 6->2 should retain their orientation")
      val rel = flipped.regularEdgeLabeling
      assert(hasEdge(rel)(4, 5))
      assert(hasEdge(rel)(5, 6))
      assert(hasEdge(rel)(6, 2))

      And("the edges 3->5, 3->6, 5->1 and 6->1 should be reversed")
      assert(hasEdge(rel)(5, 3))
      assert(hasEdge(rel)(6, 3))
      assert(hasEdge(rel)(1, 5))
      assert(hasEdge(rel)(1, 6))

    }

    it ("should reverse the edges previously belonging to T1 of a cycle when flipping it if the cycle was counter-clockwise") {

      Given("a REL with an counter-clockwise 8-cycle and edges 4->5, 5->6, 6->2 in T2, and 5->3, 6->3, 1->5 and 1->6 in T1")
      val f = RegularEdgeLabelingData.essentialEightCycleClockwise
      import f._
      val cycles = AlternatingFourCycles(labeling)
      val cc = cycles.flipCycle(cycles.allCycles.head) // This produces a cc cycle, per the test above.

      When("flipping the cycle")
      val flipped = cc.flipCycle(cc.allCycles.head)

      Then("the edges 4->5, 5->6, 6->2 should retain their orientation")
      val rel = flipped.regularEdgeLabeling
      assert(hasEdge(rel)(4, 5))
      assert(hasEdge(rel)(5, 6))
      assert(hasEdge(rel)(6, 2))

      And("the edges 5->3, 6->3, 1->5 and 1->6 should be reversed")
      assert(hasEdge(rel)(3, 5))
      assert(hasEdge(rel)(3, 6))
      assert(hasEdge(rel)(5, 1))
      assert(hasEdge(rel)(6, 1))

    }

    it ("should not reverse the direction of an inner edge in a face-cycle if the edge already travels in the direction " +
        "of the two outer edges belonging to the new T-set") {

      Given("a REL with a face-cycle having an edge 4,2 in T1 that doesn't need to be reversed when flipping it")
      val f = RegularEdgeLabelingData.essentialFaceCycle2
      import f._
      val cycles = AlternatingFourCycles(labeling)

      When("flipping the cycle")
      val flipped = cycles.flipCycle(cycles.allCycles.head)

      Then("the edge 4, 2 should keep its orientation from 4 to 2")
      val rel = flipped.regularEdgeLabeling
      assert(hasEdge(rel)(4, 2))

    }

    it ("should reverse the direction of an inner edge in a face-cycle if the edge travels opposite of the direction " +
       "of the T-set it is being flipped to") {

      Given("a REL with an edge 1, 3 in T1 that is directed opposite of what it needs to be if in T2")
      val f = RegularEdgeLabelingData.essentialFaceCycle
      import f._
      val cycles = AlternatingFourCycles(labeling)

      When("flipping the cycle")
      val flipped = cycles.flipCycle(cycles.allCycles.head)

      Then("the edge 1,3 should have the orientation 3, 1")
      val rel = flipped.regularEdgeLabeling
      assert(hasEdge(rel)(3, 1))

    }

  }

  private def findEdge[V](labeling: EdgeLabeling[V])(from: V, to: V): LabelEdge[V] = {
    labeling.edges.find(e => e.from == from && e.to == to).getOrElse(throw new NoSuchElementException("No edge from " + from + " to " + to + "."))
  }

  private def hasEdge[V](labeling: EdgeLabeling[V])(from: V, to: V): Boolean = {
    labeling.edges.exists(e => e.from == from && e.to == to)
  }

}
