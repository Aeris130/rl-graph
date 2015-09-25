package rlgraph.unit.regularEdgeLabeling

import net.cyndeline.rlgraph.regularEdgeLabeling.LabelEdge
import net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles.{DisjointCycles, EdgeData}
import rlgraph.SpecImports

class DisjointCyclesSpec extends SpecImports {
  private val disjointCycleComp = new DisjointCycles[String]()

  private def vertices = new {
    val i1 = "inner1"
    val i2 = "inner2"
    val i3 = "inner3"
    val i4 = "inner4"
    val i5 = "inner5"
    val i6 = "inner6"
    val i7 = "inner7"
    val i8 = "inner8"
    val o1 = "outer1"
    val o2 = "outer2"
    val o3 = "outer3"
    val o4 = "outer4"
    val o5 = "outer5"
    val o6 = "outer6"
    val o7 = "outer7"
    val o8 = "outer8"
  }

  private def singleCycle1 = new {
    val v = vertices
    import v._
    val e1 = LabelEdge(o1, o2, 0)
    val e2 = LabelEdge(o2, o3, 1)
    val e3 = LabelEdge(o3, o4, 2)
    val e4 = LabelEdge(o4, o1, 3)
    val innerEdge = LabelEdge(o4, o2, 4)
    val cycle = EdgeData(Vector(e1, e2, e3, e4), Vector(innerEdge))
  }

  // same as 1, but with other edges
  private def singleCycle2 = new {
    val v = vertices
    import v._
    val e1 = LabelEdge(o5, o6, 5)
    val e2 = LabelEdge(o6, o7, 6)
    val e3 = LabelEdge(o7, o8, 7)
    val e4 = LabelEdge(o8, o5, 8)
    val innerEdge = LabelEdge(o5, o7, 9)
    val cycle = EdgeData(Vector(e1, e2, e3, e4), Vector(innerEdge))
  }

  describe("DisjointCycles") {

    it ("should return a single disjoint cycle") {

      Given("a single cycle")
      val f = singleCycle1
      import f._

      When("computing disjoint cycles")
      val disjoint = disjointCycleComp.disjointInnerEdges(Vector(cycle))

      Then("the cycle should be returned")
      disjoint should be (Vector(cycle))

    }

    it ("should return two disjoint cycles") {

      Given("two disjoint cycles")
      val f1 = singleCycle1
      val f2 = singleCycle2

      When("computing disjoint cycles")
      val disjoint = disjointCycleComp.disjointInnerEdges(Vector(f1.cycle, f2.cycle))

      Then("both cycles should be returned")
      disjoint should have size 2
      disjoint.toSet should be (Set(f1.cycle, f2.cycle))

    }

    it ("should return two cycles that only share an outer edge") {

      Given("the cycles 1, 2, 3, 4 and 2, 3, 5, 6 sharing the edge 2, 3")
      val v = vertices
      import v._
      val e1 = LabelEdge(o1, o2, 0)
      val e2 = LabelEdge(o2, o3, 1)
      val e3 = LabelEdge(o3, o4, 2)
      val e4 = LabelEdge(o4, o1, 3)
      val e5 = LabelEdge(o2, o5, 4)
      val e6 = LabelEdge(o5, o6, 5)
      val e7 = LabelEdge(o6, o3, 6)
      val innerEdge1 = LabelEdge(o4, o2, 7)
      val innerEdge2 = LabelEdge(o3, o5, 8)
      val cycle1 = EdgeData(Vector(e1, e2, e3, e4), Vector(innerEdge1))
      val cycle2 = EdgeData(Vector(e2, e5, e6, e7), Vector(innerEdge2))

      When("computing disjoint cycles")
      val disjoint = disjointCycleComp.disjointInnerEdges(Vector(cycle1, cycle2))

      Then("both cycles should be returned")
      disjoint should have size 2
      disjoint.toSet should be (Set(cycle1, cycle2))

    }

    // This test doesn't use a valid REL setup by any means, but it doesn't have to.
    it ("should only return one cycle when two cycles share an edge marked as inner in either of them") {

      Given("a cycle 1, 2, 3, 4 with an inner edge 2, 4 that is used as an outer edge in the cycle 2, 4, 5, 6")
      val f = singleCycle1
      import f._
      val v = vertices
      import v._
      val e6 = LabelEdge(o2, o6, 5)
      val e7 = LabelEdge(o6, o5, 6)
      val e8 = LabelEdge(o5, o4, 7)
      val inner2 = LabelEdge(o5, o3, 8)
      val inner3 = LabelEdge(o3, o6, 9)
      val cycle2 = EdgeData(Vector(innerEdge, e6, e7, e8), Vector(e3, e2, inner2, inner3))

      When("computing disjoint cycles")
      val disjoint = disjointCycleComp.disjointInnerEdges(Vector(cycle, cycle2))

      Then("only one cycle should be returned")
      disjoint should have size 1
      assert(disjoint.head == cycle || disjoint.head == cycle2)

    }


  }

}
