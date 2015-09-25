package rlgraph.unit.spqr.jbpt

import net.cyndeline.rlgraph.spqr.Series
import net.cyndeline.rlgraph.spqr.jbpt.{HashNode, OrderedEdge}
import rlgraph.SpecImports

class HashNodeSpec extends SpecImports {

  it ("should add vertices from solid edges to its vertex set") {

    Given("a node with no edges or vertices")
    val n = HashNode[Int](0, Series)

    When("adding a solid edge")
    n.addSolidEdge(OrderedEdge(0, 1))

    Then("vertices 0 and 1 should be in the vertex set")
    n.vertices.toSet should be (Set(0, 1))

  }

  it ("should add vertices from a virtual edge to its vertex set") {

    Given("a node with no edges or vertices")
    val n = HashNode[Int](0, Series)

    When("adding a virtual edge")
    n.addVirtualEdge(OrderedEdge(0, 1))

    Then("vertices 0 and 1 should be in the vertex set")
    n.vertices.toSet should be (Set(0, 1))

  }

}
