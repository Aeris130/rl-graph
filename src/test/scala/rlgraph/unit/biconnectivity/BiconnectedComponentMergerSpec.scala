package rlgraph.unit.biconnectivity

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.help.BiconnectedComponentMerger
import rlgraph.SpecImports

import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class BiconnectedComponentMergerSpec extends SpecImports {

  describe("BiconnectedComponentMerger") {

    it ("should return all cut points") {

      Given("a merger for a graph with cut points 2 and 3")
      val componentMerger = new BiconnectedComponentMerger(Graph(1~2, 2~3, 3~4))

      When("retrieving the cut points")
      val cutPoints = componentMerger.cutPoints

      Then("the list should contain vertices 2 and 3")
      cutPoints should have size 2
      cutPoints should contain (2)
      cutPoints should contain (3)

    }

    it ("should mark two vertices belong to different biconnected components as such") {

      Given("a merger for a graph with vertices 1 and 3 belonging to different biconnected components")
      val componentMerger = new BiconnectedComponentMerger(Graph(1~2, 2~3))

      When("checking if the vertices 1 and 3 belongs to the same biconnected component")
      Then("the result should be false")
      componentMerger.sharesBiconnectedComponent(1, 3) should be (false)

    }

    it ("should mark two vertices belong to the same biconnected component as such") {

      Given("a merger for a graph with vertices 1 and 2 belonging to the same biconnected component")
      val componentMerger = new BiconnectedComponentMerger(Graph(1~2, 2~3))

      When("checking if the vertices 1 and 2 belongs to the same biconnected component")
      Then("the result should be true")
      componentMerger.sharesBiconnectedComponent(1, 2) should be (true)

    }

    it ("should merge two vertices from different components to the same component") {

      Given("a merger based on a graph with the biconnected components and the cut vertex 3")
      val componentMerger = new BiconnectedComponentMerger(Graph(1~2, 2~3, 3~1, 3~4, 4~5, 5~3))

      When("merging vertices 1 and 4 at the cut point 3")
      componentMerger.sharesBiconnectedComponent(1, 4) should be (false)
      componentMerger.merge(1, 4, 3)

      Then("1 and 4 should belong to a common component")
      componentMerger.sharesBiconnectedComponent(1, 4) should be (true)

    }

    it ("should not merge components connected to a component that merges") {

      Given("a graph with two biconnected components (1,2,3) and (3,4,5) connected to each other, and two components " +
            "(1,6,7) and (4,8,9) connected to the first two")
      val component1 = Graph(1~2, 2~3, 3~1)
      val component2 = Graph(3~4, 4~5, 5~3)
      val connectedToComponent1 = Graph(1~6, 6~7, 7~1)
      val connectedToComponent2 = Graph(4~8, 8~9, 9~4)
      val graph = component1 ++ component2 ++ connectedToComponent1 ++ connectedToComponent2

      val componentMerger = new BiconnectedComponentMerger(graph)

      When("merging vertices 2 and 5 around cut point 3")
      componentMerger.merge(2, 5, 3)

      Then("vertices 6/7 should not share components with any other vertex in the graph except 1")
      componentMerger.sharesBiconnectedComponent(6, 1) should be (true)
      componentMerger.sharesBiconnectedComponent(6, 2) should be (false)
      componentMerger.sharesBiconnectedComponent(6, 3) should be (false)
      componentMerger.sharesBiconnectedComponent(6, 4) should be (false)
      componentMerger.sharesBiconnectedComponent(6, 5) should be (false)
      componentMerger.sharesBiconnectedComponent(6, 8) should be (false)
      componentMerger.sharesBiconnectedComponent(6, 9) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 1) should be (true)
      componentMerger.sharesBiconnectedComponent(7, 2) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 3) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 4) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 5) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 8) should be (false)
      componentMerger.sharesBiconnectedComponent(7, 9) should be (false)

      And("vertices 8/9 should not share components with any other vertex in the graph except 4")
      componentMerger.sharesBiconnectedComponent(8, 1) should be (false)
      componentMerger.sharesBiconnectedComponent(8, 2) should be (false)
      componentMerger.sharesBiconnectedComponent(8, 3) should be (false)
      componentMerger.sharesBiconnectedComponent(8, 4) should be (true)
      componentMerger.sharesBiconnectedComponent(8, 5) should be (false)
      componentMerger.sharesBiconnectedComponent(8, 6) should be (false)
      componentMerger.sharesBiconnectedComponent(8, 7) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 1) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 2) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 3) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 4) should be (true)
      componentMerger.sharesBiconnectedComponent(9, 5) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 6) should be (false)
      componentMerger.sharesBiconnectedComponent(9, 7) should be (false)

    }

  }
}
