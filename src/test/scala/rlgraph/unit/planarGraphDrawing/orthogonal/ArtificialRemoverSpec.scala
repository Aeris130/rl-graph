package rlgraph.unit.planarGraphDrawing.orthogonal

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.ArtificialRemover
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.{ArtificialDart, DefaultDart, SimpleDart, SplitDart}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapper}
import rlgraph.SpecImports

class ArtificialRemoverSpec extends SpecImports {
  private val remover = new ArtificialRemover()
  private implicit def dartToSet(dart: Dart[String]) = Set(dart)

  def darts = new {
    val v1 = new VertexWrapper[String](1, "v1")
    val v2 = new VertexWrapper[String](2, "v2")
    val v3 = new VertexWrapper[String](3, "v3")
    val v4 = new VertexWrapper[String](4, "v4")
    val v5 = new VertexWrapper[String](5, "v5")
    val v6 = new VertexWrapper[String](6, "v6")

    val default = new DefaultDart[String](v1, v2, 1, 0)

    val artificial = new ArtificialDart[String](v2, v3, 1)

    val splitWithDefault = new SplitDart[String](v3, v4, 1, default)
    val splitWithArtificial= new SplitDart[String](v4, v5, 1, artificial)

    val simpleWithDefault = new SimpleDart[String](v5, v6, 1, default)

    val multipleNestledDefault = new SplitDart[String](v6, v5, 1, simpleWithDefault)
    val multipleNestledArtificial = new SplitDart[String](v6, v5, 1, splitWithArtificial)

    val splitWithSimpleWithDefault = new SplitDart[String](v4, v5, 1, simpleWithDefault)
    val splitWithSimpleWithDefault2 = new SplitDart[String](v1, v2, 1, simpleWithDefault)
  }

  describe("ArtificialDartRemover") {

    /*
     *
     * Edge removal tests
     *
     */

    it ("should keep default darts") {

      Given("a default dart")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](default)

      Then("the remaining darts should include the default dart")
      nonArtificialDarts should equal (Set(default))

    }

    it ("should remove artificial darts") {

      Given("an artificial dart")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](artificial)

      Then("the no darts should remain")
      nonArtificialDarts should equal (Set())

    }

    it ("should keep split darts that splits a default dart") {

      Given("a split dart with a default dart set as its original")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](splitWithDefault)

      Then("the split dart should be kept")
      nonArtificialDarts should equal (Set(splitWithDefault))

    }

    it ("should discard darts that split an artificial dart") {

      Given("a split dart with an artificial dart set as its original")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](splitWithArtificial)

      Then("the split dart should be discarded")
      nonArtificialDarts should equal (Set())

    }

    it ("should keep simple darts that simplifies a default dart") {

      Given("a simple dart with a default dart set as its original")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](simpleWithDefault)

      Then("the simple dart should be kept")
      nonArtificialDarts should equal (Set(simpleWithDefault))

    }

    it ("should keep a dart with a default dart nestled multiple layers down the original") {

      Given("a simple dart with a simple dart based on a default dart as original")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](multipleNestledDefault)

      Then("the simple dart should be kept")
      nonArtificialDarts should equal (Set(multipleNestledDefault))

    }

    it ("should discard a dart with an artificial dart nestled multiple layers down the original") {

      Given("a split dart with a split dart based on an artificial dart as original")
      val f = darts
      import f._

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts[String](multipleNestledArtificial)

      Then("the split dart should be discarded")
      nonArtificialDarts should equal (Set())

    }

    it ("should keep all darts related to default darts when filtering a collection of multiple darts") {

      Given("a collection of default and artificial darts")
      val f = darts
      import f._
      val dartSet = Set(default, artificial, splitWithDefault, splitWithArtificial, simpleWithDefault, multipleNestledDefault, multipleNestledArtificial)

      When("removing artificial darts")
      val nonArtificialDarts = remover.removeArtificialDarts(dartSet)

      Then("only the default-based darts should be kept")
      nonArtificialDarts should equal (Set(default, splitWithDefault, simpleWithDefault, multipleNestledDefault))

    }

    /*
     *
     * Vertex removal tests
     *
     */

    it ("should ignore non-split darts when removing artificial vertices") {

      Given("a dart-set without split darts")
      val f = darts
      import f._

      When("removing artificial vertices")
      val dartsWithoutArtificialVertices = remover.removeArtificialVertices[String](default)

      Then("the resulting set should contain the dart")
      dartsWithoutArtificialVertices should equal (Set(default))

    }

    it ("should stop at the simple dart if a split dart has a simple dart with a default original") {

      Given("a split dart with a simple original with a default original")
      val f = darts
      import f._

      When("removing artificial vertices")
      val dartsWithoutArtificialVertices = remover.removeArtificialVertices[String](splitWithSimpleWithDefault)

      Then("the resulting set should contain the simple dart")
      dartsWithoutArtificialVertices should equal (Set(simpleWithDefault))

    }

    it ("should only return one copy if multiple splits based on the same original exists") {

      Given("two split dart with the same simple original with a default original")
      val f = darts
      import f._

      When("removing artificial vertices")
      val dartSet = Set[Dart[String]](splitWithSimpleWithDefault, splitWithSimpleWithDefault2)
      val dartsWithoutArtificialVertices = remover.removeArtificialVertices[String](dartSet)

      Then("the resulting set should contain the simple dart")
      assert(dartsWithoutArtificialVertices.size == 1, "Too many darts returned (" + dartsWithoutArtificialVertices.size + "), should have been 1.")
      dartsWithoutArtificialVertices should equal (Set(simpleWithDefault))

    }

  }

}
