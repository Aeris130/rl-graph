package rlgraph.unit.rectangular.triangleBreak

import net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak.help.{Bubble, BubbleGraph}
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import rlgraph.SpecImports

class BubbleGraphSpec extends SpecImports {

  // Three connected bubbles
  private def tripleGraph = new {
    val t1 = Triangle(1, 2, 3)
    val t2 = Triangle(2, 3, 4) // Shares 2~3 with t1
    val t3 = Triangle(3, 4, 5) // Shares 3~4 with t2
    val bubbleGraph = BubbleGraph(Vector(t1, t2, t3)).head

    val bv1 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(1, 2)).get
    val bv2 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(2, 3)).get
    val bv3 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(3, 1)).get
    val bv4 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(3, 4)).get
    val bv5 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(4, 2)).get
    val bv6 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(4, 5)).get
    val bv7 = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(5, 3)).get


  }

  describe("BubbleGraph") {

    it ("should create multiple graphs for disconnected bubbles") {

      Given("two disconnected triangles")
      val t1 = Triangle(1, 2, 3)
      val t2 = Triangle(4, 5, 6)

      When("constructing bubble graphs")
      val bubbleGraphs = BubbleGraph(Vector(t1, t2))

      Then("two graphs should be created")
      bubbleGraphs should have size 2

      And("both graphs should have size 1")
      bubbleGraphs(0).size should be (1)
      bubbleGraphs(1).size should be (1)

      And("one of the graphs should have a bubble for vertices 1, 2, 3")
      assert(bubbleGraphs.exists(graph => verticesOfBubble(graph.bubbles.head) == Set(1, 2, 3)))

      And("one of the graphs should have a bubble for vertices 4, 5, 6")
      assert(bubbleGraphs.exists(graph => verticesOfBubble(graph.bubbles.head) == Set(4, 5, 6)))

    }

    it ("should create a single graph for two adjacent bubbles") {

      Given("two triangles sharing an edge 2~3")
      val t1 = Triangle(1, 2, 3)
      val t2 = Triangle(2, 3, 4)

      When("constructing bubble graphs")
      val bubbleGraphs = BubbleGraph(Vector(t1, t2))

      Then("a single graph should be created")
      bubbleGraphs should have size 1

      And("the graph should have size 2")
      bubbleGraphs.head.size should be (2)

      And("the graph should have a bubble for vertices 1, 2, 3")
      assert(bubbleGraphs.head.bubbles.exists(verticesOfBubble(_) == Set(1, 2, 3)))

      And("the graph should have a bubble for vertices 2, 3, 4")
      assert(bubbleGraphs.head.bubbles.exists(verticesOfBubble(_) == Set(2, 3, 4)))

      And("both bubbles should share a single vertex representing the edge 2~3")
      val bubble1 = bubbleGraphs.head.bubbles(0)
      val bubble2 = bubbleGraphs.head.bubbles(1)
      bubble1.vertices.toSet.intersect(bubble2.vertices.toSet) should have size 1

    }

    it ("should not add bubbles to the same graph if they only share a single vertex") {

      Given("two triangles sharing a vertex 3")
      val t1 = Triangle(1, 2, 3)
      val t2 = Triangle(3, 4, 5)

      When("constructing bubble graphs")
      val bubbleGraphs = BubbleGraph(Vector(t1, t2))

      Then("two graphs should be created")
      bubbleGraphs should have size 2

      And("the bubble set should be disjoint")
      bubbleGraphs(0).bubbles.toSet intersect bubbleGraphs(1).bubbles.toSet should be ('empty)

    }

    it ("should pop a single bubble") {

      Given("a graph with a single bubble representing the triangle 1, 2, 3")
      val t1 = Triangle(1, 2, 3)
      val bubbleGraph = BubbleGraph(Vector(t1)).head
      val bubbleVertex1 = bubbleGraph.bubbles.head.vertices(0)
      val bubbleVertex2 = bubbleGraph.bubbles.head.vertices(1)
      val bubbleVertex3 = bubbleGraph.bubbles.head.vertices(2)

      When("popping a bubble containing a vertex representing a triangle edge")
      bubbleGraph.popBubbles(bubbleVertex1)

      Then("no bubbles should exist in the graph")
      bubbleGraph.size should be (0)

      And("the weight of every bubble vertex should be 0")
      bubbleVertex1.weight should be (0)
      bubbleVertex2.weight should be (0)
      bubbleVertex3.weight should be (0)

    }

    it ("should pop two bubbles sharing a bubble-vertex") {

      Given("a graph with two bubbles sharing the vertex representing the common edge 2~3")
      val t1 = Triangle(1, 2, 3)
      val t2 = Triangle(2, 3, 4)
      val bubbleGraph = BubbleGraph(Vector(t1, t2)).head
      val bubbles = bubbleGraph.bubbles
      val sharedVertex = (bubbles(0).vertices intersect bubbles(1).vertices).head

      When("popping bubbles containing the shared vertex")
      bubbleGraph.popBubbles(sharedVertex)

      Then("no bubbles should exist in the graph")
      bubbleGraph.size should be (0)

    }

    it ("should not pop bubbles that doesn't share the vertex being used to pop") {

      Given("three adjacent bubbles, with one bubble vertex being exclusive to one of the bubbles")
      val f = tripleGraph
      import f._
      val edge4_5Vertex = bubbleGraph.bubbles
        .find(verticesOfBubble(_) == Set(3, 4, 5))
        .get.vertices.find(bv => Set(bv._1, bv._2) == Set(4, 5)).get

      When("popping bubbles containing the exclusive vertex")
      bubbleGraph.popBubbles(edge4_5Vertex)

      Then("2 bubbles should exist in the graph")
      bubbleGraph.size should be (2)

      And("that bubble should represent triangles <1, 2, 3> and <2, 3, 4>")
      assert(bubbleGraph.bubbles.exists(verticesOfBubble(_) == Set(1, 2, 3)))
      assert(bubbleGraph.bubbles.exists(verticesOfBubble(_) == Set(2, 3, 4)))

    }

    it ("should initiate vertex weights") {

      Given("three adjacent bubbles with edges 2,3 and 3,4 connecting two bubbles")
      val f = tripleGraph
      import f._

      Then("vertices (1,2), (3,1), (4,2), (4,5), (5,3) should have weight 1")
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(1, 2)).get.weight should be (1)
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(3, 1)).get.weight should be (1)
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(4, 2)).get.weight should be (1)
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(4, 5)).get.weight should be (1)
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(5, 3)).get.weight should be (1)

      And("vertices (2,3) and (3,4) should have weight 2")
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(2, 3)).get.weight should be (2)
      bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(3, 4)).get.weight should be (2)

    }

    it ("should remove bubble membership of the remaining two vertices when a bubble is popped") {

      Given("three vertices each representing an edge in a triangle")
      val t1 = Triangle(1, 2, 3)
      val bubbleGraph = BubbleGraph(Vector(t1)).head
      val bubbleVertex1 = bubbleGraph.bubbles.head.vertices(0)
      val bubbleVertex2 = bubbleGraph.bubbles.head.vertices(1)
      val bubbleVertex3 = bubbleGraph.bubbles.head.vertices(2)

      When("popping a bubble using one of the vertices")
      bubbleGraph.popBubbles(bubbleVertex1)

      Then("the none of the vertices should have a bubble associated with them")
      bubbleGraph.bubblesOfVertex(bubbleVertex1) should be ('empty)
      bubbleGraph.bubblesOfVertex(bubbleVertex2) should be ('empty)
      bubbleGraph.bubblesOfVertex(bubbleVertex3) should be ('empty)

      And("the vertex set should be empty")
      bubbleGraph.bubbleVertices should be ('empty)

    }

    it ("should return the original graph when popping the only bubble in it") {

      Given("a graph with a bubble")
      val t1 = Triangle(1, 2, 3)
      val bubbleGraph = BubbleGraph(Vector(t1)).head
      val someVertex = bubbleGraph.bubbles.head.vertices(0)

      When("popping the bubble")
      val resultingGraphs = bubbleGraph.popBubbles(someVertex)

      Then("a single graph should be returned")
      resultingGraphs should have size 1

      And("it should be the original graph instance")
      assert(resultingGraphs.head eq bubbleGraph)

    }

    it ("should return the original graph when popping one of many bubbles doesn't disconnect it") {

      Given("a graph with multiple bubbles, and the edge (1,2) being present at an outer 1-bubble.")
      val f = tripleGraph
      import f._
      val outerVertex = bubbleGraph.bubbleVertices.find(v => Set(v._1, v._2) == Set(1, 2)).get

      When("popping all bubbles using the outer vertex")
      val resultingGraphs = bubbleGraph.popBubbles(outerVertex)

      Then("a single graph should be returned")
      resultingGraphs should have size 1

      And("it should be the original graph instance")
      assert(resultingGraphs.head eq bubbleGraph)

    }

    it ("should return two new graph instances when popping a bubble in the old one results in a disconnected graph") {

      Given("a graph with multiple bubbles, and the edge (4,2) being present at a bubble that connects two other bubbles, but not in the other two bubbles")
      val f = tripleGraph
      import f._
      val vertex = bv5

      When("popping all bubbles using the vertex")
      val resultingGraphs = bubbleGraph.popBubbles(vertex)

      Then("a two graphs should be returned")
      resultingGraphs should have size 2

      And("one of them should contain vertices (1,2), (2,3) and (3,1)")
      resultingGraphs.exists(graph => graph.bubbleVertices.contains(bv1) && graph.bubbleVertices.contains(bv2) && graph.bubbleVertices.contains(bv3))

      And("one of them should contain vertices (3,4), (4,5) and (5,3)")
      resultingGraphs.exists(graph => graph.bubbleVertices.contains(bv4) && graph.bubbleVertices.contains(bv6) && graph.bubbleVertices.contains(bv7))

    }

    it ("should maintain bubble membership in bubble vertices when popping a graph into subgraphs") {

      Given("a graph with multiple bubbles, and the edge (4,2) being present at a bubble that connects two other bubbles, but not in the other two bubbles")
      val f = tripleGraph
      import f._
      val vertex = bv5
      val bubble1 = bubbleGraph.bubbles.find(_.vertices.toSet == Set(bv1, bv2, bv3)).get
      val bubble3 = bubbleGraph.bubbles.find(_.vertices.toSet == Set(bv4, bv6, bv7)).get

      When("popping all bubbles using the vertex")
      val resultingGraphs = bubbleGraph.popBubbles(vertex)
      val g1 = resultingGraphs.find(_.bubbles.contains(bubble1)).get
      val g2 = resultingGraphs.find(_.bubbles.contains(bubble3)).get

      Then("bubble vertices (1,2), (2,3) and (3,1) should belong to bubble 1")
      g1.bubblesOfVertex(bv1) should be (Vector(bubble1))
      g1.bubblesOfVertex(bv2) should be (Vector(bubble1))
      g1.bubblesOfVertex(bv3) should be (Vector(bubble1))

      And("bubble vertices (3,4), (4,5) and (5,3) should belong to bubble 3")
      g2.bubblesOfVertex(bv4) should be (Vector(bubble3))
      g2.bubblesOfVertex(bv6) should be (Vector(bubble3))
      g2.bubblesOfVertex(bv7) should be (Vector(bubble3))

    }


  }

  private def verticesOfBubble(b: Bubble[Int]): Set[Int] = {
    b.vertices.map(bv => Set(bv._1, bv._2)).flatten.toSet
  }

}
