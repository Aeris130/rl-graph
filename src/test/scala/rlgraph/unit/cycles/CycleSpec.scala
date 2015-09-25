package rlgraph.unit.cycles

import net.cyndeline.rlgraph.cycles.Cycle
import rlgraph.SpecImports

class CycleSpec extends SpecImports {

  describe("Cycle") {

    it ("should consider two cycles equal as long as they contain the same edges") {

      Given("a cycle 1, 2, 3 and a cycle 3, 1, 2")
      val c1 = Cycle(1, 2, 3)
      val c2 = Cycle(3, 1, 2)

      When("comparing them")
      Then("they should be equal")
      c1 should equal (c2)

    }

    it ("should consider two reversed cycles equal if the cycles are undirected") {

      Given("two undirected cycles with the same vertices, where one of them is reversed")
      val vs = Vector(1, 2, 3, 4)
      val c1 = Cycle.undirected(vs.reverse)
      val c2 = Cycle.undirected(vs)

      When("comparing them")
      Then("they should be equal")
      c1 should equal (c2)

    }

    it ("should not consider two reversed cycles as equal if only one of them are undirected") {

      Given("two cycles with the same vertices, where one of them is reversed and only one is undirected")
      val vs = Vector(1, 2, 3, 4)
      val c1 = Cycle(vs.reverse)
      val c2 = Cycle.undirected(vs)

      When("comparing them")
      Then("they should not be equal")
      c1 should not equal c2

    }

    it ("should not consider two cycles with the same vertices to be equal if only one of them are undirected") {

      Given("two cycles with the same vertices, where only one of them are undirected")
      val vs = Vector(1, 2, 3, 4)
      val c1 = Cycle(vs)
      val c2 = Cycle.undirected(vs)

      When("comparing them")
      Then("they should not be equal")
      c1 should not equal c2

    }
  }

}
