package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.DartSimplifier
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.DefaultDart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{VertexWrapper, VertexWrapperFactory}
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

class DartSimplifierSpec extends SpecImports {
  private val wrapperFactory = new VertexWrapperFactory[String]()
  private val simplifier = new DartSimplifier(wrapperFactory)

  def vWrappers = new {
    val A = new VertexWrapper(1, "A")
    val B = new VertexWrapper(2, "B")
    val C = new VertexWrapper(3, "C")
    val D = new VertexWrapper(4, "D")
    val E = new VertexWrapper(5, "E")
    val F = new VertexWrapper(6, "F")
    val G = new VertexWrapper(7, "G")
  }

  val dummyOriginalEdge: Option[UnDiEdge[String]] = Option("A"~"B")

  describe("DartSimplifier") {

    /**
     *
     * Tests simplifying a dart from scratch.
     *
     */
    it ("should create darts equal to the number of bends + 1") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("the result should be 4 new darts")
      result.size should be (4)

    }

    it ("should add the original to/from vertices as beginning and end to the simplified dart chain") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("the first vertex of the first dart should be the first vertex in the original dart")
      result.head.from should equal (dart.from)

      And("the last vertex of the last dart should be the last vertex in the original dart")
      result.reverse.head.to should be (dart.to)

    }

    it ("should give the last dart the same bend as the original") {

      Given("a dart vertex-bend 4")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 1, 0)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("the bend value of the last dart should be 4")
      result.reverse.head.nextDegree should be (4)

    }

    it ("should return Nil if neither dart has bends") {

      Given("a dart and its opposite, both without bends")
      val dart = new DefaultDart[String]("A", "B", 4, 0, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 1, 0)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("no darts should be created")
      result should be (Vector())

    }

    it ("should apply the bends of the regular dart even if the opposite doesn't contain any") {

      Given("a dart pair where only the regular dart has bends")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 1, 0)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("3 darts should be created")
      result.size should be (3)

      And("the fist two darts should bend 90 degrees inwards")
      result(0).nextDegree should be (1)
      result(1).nextDegree should be (1)

    }

    it ("should apply the bends of the opposite dart even if the regular one doesn't contain any") {

      Given("a dart pair where only the opposite dart has bends")
      val dart = new DefaultDart[String]("A", "B", 4, 0, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 1, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("two darts should be created")
      result.size should be (2)

      And("the first dart should have a 270 degree bend outward")
      result(0).nextDegree should be (3)

    }

    it ("should add vertex-bends to the dart chain based on the edge-bend") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("the first dart should bend 90 degrees inward")
      result(0).nextDegree should be (1)

      And("the next dart should also bend 90 degrees inwards")
      result(1).nextDegree should be (1)

      And("the next dart should bend 270 degrees outward")
      result(2).nextDegree should be (3)

    }

    it ("should set the bend value of all darts created to 0") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("every created dart should have edge-bends == 0")
      result(0).bends should be (0)
      result(1).bends should be (0)
      result(2).bends should be (0)
      result(3).bends should be (0)

    }

    it ("should use the end-dummy vertex of one dart as the start-dummy of the next") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("the last vertex of the one dart should be the first vertex of the next")
      result(0).to should equal (result(1).from)
      result(1).to should equal (result(2).from)
      result(2).to should equal (result(3).from)
    }

    it ("should add the original darts from/to to every simplified dart when simplifying a dart from scratch") {

      Given("a dart with 2 bends, and an opposite dart with 1 bend")
      val dart = new DefaultDart[String]("A", "B", 4, 2, wrapperFactory)
      val oppositeDart = new DefaultDart[String](dart.to, dart.from, 4, 1)

      When("simplifying the dart")
      val result = simplifier.simplifyDart(dart, oppositeDart)

      Then("every dart should have A -> B as its original")
      result(0).original should equal (dart.from, dart.to)
      result(1).original should equal (dart.from, dart.to)
      result(2).original should equal (dart.from, dart.to)
      result(3).original should equal (dart.from, dart.to)

    }

    /**
     *
     * Tests simplifying a dart based on the simplification of its opposite.
     *
     */

    it ("should apply inside bends based on the opposite bends of a simplified dart list") {

      Given("a dart with 2 internal bends and a list of 3 opposite dart simplifications with 270 degree bends from its pov")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 3, 0)
      val chain2 = new DefaultDart[String](C, D, 3, 0)
      val chain3 = new DefaultDart[String](D, A, 4, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("the result should have 3 darts")
      result.size should be (3)

      And("the darts should form a chain A -> D -> C -> B")
      result(0).from should be (A)
      result(0).to should be (D)
      result(1).from should be (D)
      result(1).to should be (C)
      result(2).from should be (C)
      result(2).to should be (B)

      And("every dart should have a 90 degree bend to the next, except the last")
      result(0).nextDegree should be (1)
      result(1).nextDegree should be (1)

    }

    it ("should apply opposite bends based on the inside bends of a simplified dart list") {

      Given("a dart with no internal bends and a list of 3 opposite dart simplifications with 90 degree bends from its pov")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 1, 0)
      val chain2 = new DefaultDart[String](C, D, 1, 0)
      val chain3 = new DefaultDart[String](D, A, 4, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("the result should have 3 darts")
      result.size should be (3)

      And("every dart should have a 270 degree bend to the next, except the last")
      result(0).nextDegree should be (3)
      result(1).nextDegree should be (3)

    }

    it ("should apply both inside and outside bends based on the bends of its opposite simplification") {

      Given("a dart with 2 internal bends and an opposite simplification with two 90 degree bends and two 270 degree bends from its pov")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 1, 0)
      val chain2 = new DefaultDart[String](C, D, 1, 0)
      val chain3 = new DefaultDart[String](D, E, 3, 0)
      val chain4 = new DefaultDart[String](E, F, 3, 0)
      val chain5 = new DefaultDart[String](F, A, 3, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3, chain4, chain5)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("the result should have 5 darts")
      result.size should be (5)

      And("the darts should have two 90 degree bends followed by two 270 degree bends")
      result(0).nextDegree should be (1)
      result(1).nextDegree should be (1)
      result(2).nextDegree should be (3)
      result(3).nextDegree should be (3)

    }

    it ("should set the original darts outgoing vertex bend as the last bend in the simplification") {

      Given("a dart with a 360 degree vertex-bend and its opposite simplification")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 3, 0)
      val chain2 = new DefaultDart[String](C, D, 3, 0)
      val chain3 = new DefaultDart[String](D, A, 2, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("the last dart should have vertex-bend 4")
      result(2).nextDegree should be (4)

    }

    it ("should chain the same wrapper instances as the supplied opposite, and in the reversed order") {

      Given("a dart with 2 internal bends and an opposite simplification with two 90 degree bends and two 270 degree bends from its pov")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 1, 0)
      val chain2 = new DefaultDart[String](C, D, 1, 0)
      val chain3 = new DefaultDart[String](D, E, 3, 0)
      val chain4 = new DefaultDart[String](E, F, 3, 0)
      val chain5 = new DefaultDart[String](F, A, 3, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3, chain4, chain5)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("the traversed vertices should be in the order A -> F -> E -> D -> C -> B")
      result(0).from should be (A)
      result(0).to should be (F)
      result(1).from should be (F)
      result(1).to should be (E)
      result(2).from should be (E)
      result(2).to should be (D)
      result(3).from should be (D)
      result(3).to should be (C)
      result(4).from should be (C)
      result(4).to should be (B)

    }

    it ("should add the original darts from/to to every simplified dart when simplifying a dart from another simplification") {

      Given("a dart with 2 internal bends and an opposite simplification with two 90 degree bends and two 270 degree bends from its pov")
      val f = vWrappers
      import f._
      val dart = new DefaultDart[String](A, B, 4, 2)
      val chain1 = new DefaultDart[String](B, C, 1, 0)
      val chain2 = new DefaultDart[String](C, D, 1, 0)
      val chain3 = new DefaultDart[String](D, E, 3, 0)
      val chain4 = new DefaultDart[String](E, F, 3, 0)
      val chain5 = new DefaultDart[String](F, A, 3, 0)
      val oppositeDartList = Vector(chain1, chain2, chain3, chain4, chain5)

      When("simplifying the dart")
      val result = simplifier.simplifySimpleDart(dart, oppositeDartList)

      Then("every simplified dart should have A -> B as its original")
      result(0).original should equal (dart.from, dart.to)
      result(1).original should equal (dart.from, dart.to)
      result(2).original should equal (dart.from, dart.to)
      result(3).original should equal (dart.from, dart.to)
      result(4).original should equal (dart.from, dart.to)

    }

  }
}
