package rlgraph.unit.drawings.planar.orthogonal

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.FlowNetworkConstructor
import net.cyndeline.rlgraph.drawings.planar.orthogonal.network.util.{FlowEdge, FlowVertex, NetworkRepresentation}
import rlgraph.SpecImports

import scalax.collection.immutable.Graph

class FlowNetworkConstructorSpec extends SpecImports {
  private val networkConstructor = new FlowNetworkConstructor()

  def triangular = new {
    val faces = Vector(new Face(Vector(1, 2, 3)), new Face(Vector(3, 2, 1)))
    val vertices = Vector(1, 2, 3)
    val externalFace = 0
    val internalFace = new Face(Vector(3, 2, 1))
  }

  def triangularWithBridgeOnBothFaces = new {
    val extFace = new Face(Vector(1, 2, 4, 2, 3))
    val intFace = new Face(Vector(3, 2, 5, 2, 1))
    val faces = Vector(extFace, intFace)
    val vertices = Vector(1, 2, 3, 4, 5)
    val externalFace = 0
  }

  describe("FlowNetworkConstructor") {

    it ("should assign a flow vertex for every vertex in the original graph") {

      Given("a triangular graph with 3 vertices")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the set of original vertices mapped in the representation should equal the original set")
      networkRep.vertexMapping.values.toSet should equal (vertices.toSet)

      And("the set of flow vertices added should equal the original set in size")
      val newSize = networkRep.vertexMapping.keySet.size
      val oldSize = networkRep.vertexMapping.values.size
      assert(newSize == oldSize, "There was a " + Math.abs(oldSize - newSize) + " size difference between the original vertex set and the new set of flow vertices. " + Math.abs(oldSize - newSize) + " new flow-v's are missing.")

    }

    it ("should assign a face vertex for every face") {

      Given("a triangular graph with 2 faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the set of original faces mapped in the representation should equal the original set")
      networkRep.faceMapping.values.toSet should equal (faces.toSet)

      And("the set of face-vertices added should equal the original set in size")
      val newSize = networkRep.faceMapping.keySet.size
      val oldSize = networkRep.faceMapping.values.size
      assert(newSize == oldSize, "There was a " + Math.abs(oldSize - newSize) + " size difference between the original vertex set and the new set of face vertices. " + Math.abs(oldSize - newSize) + " new face-v's are missing.")

    }

    it ("should insert edges between sources (original vertices in faces) and new face-vertices") {

      Given("a triangular graph with 2 faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the flow vertices belonging to faces in the graph should have outgoing edges to the two face-vertices")
      val faceVertices: Iterator[FlowVertex] = networkRep.faceMapping.keysIterator

        networkRep.faceMapping.keySet.size should be (2)

      val f1 = networkRep.graph.get(faceVertices.next())
      val f2 = networkRep.graph.get(faceVertices.next())
      val faceSet: Set[Graph[FlowVertex, FlowEdge]#NodeT] = Set(f1, f2)

      val someVertexEntry: (FlowVertex, Int) = networkRep.vertexMapping.head // Some arbitrary vertex
      val vertex: Graph[FlowVertex, FlowEdge]#NodeT = networkRep.graph.get(someVertexEntry._1)

        vertex.outDegree should be (2)

      val outgoingEdges: Vector[Graph[FlowVertex, FlowEdge]#EdgeT] = vertex.outgoing.toVector
      val firstOutgoing: Graph[FlowVertex, FlowEdge]#NodeT = outgoingEdges(0).to
      val secondOutgoing: Graph[FlowVertex, FlowEdge]#NodeT = outgoingEdges(1).to

        faceSet should contain (firstOutgoing)
        faceSet should contain (secondOutgoing)

    }

    it ("should only insert 1 edge between each vertex and face-vertex, even if the vertex occurs more than once in the face") {

      Given("a graph with a single external face")
      val face = new Face(Vector(1, 2, 3, 2))
      val vertices = Vector(1, 2, 3)

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, Vector(face), 0)
      val vertex2 = networkRep.vertexMapping.map(_.swap).get(2).get

      Then("vertex 2 should only have a single outgoing edge")
      networkRep.graph.get(vertex2).outgoing.size should be (1)

    }

    it ("should insert 1 edge between face vertices adjacent to each other for each edge they share") {

      Given("a triangular graph with 2 faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the two face vertices in the graph should have each other a neighbors once for all three edges they share")
      val faceToVertex = networkRep.faceMapping.map(_.swap)
      val faceVertex1 = faceToVertex.get(faces(0)).get
      val faceVertex2 = faceToVertex.get(faces(1)).get
      val innerNode1 = networkRep.graph.get(faceVertex1)
      val innerNode2 = networkRep.graph.get(faceVertex2)
      val graph: Graph[FlowVertex, FlowEdge] = networkRep.graph

      innerNode1.outDegree should be (3)
      innerNode2.outDegree should be (3)

      // innerNode1 is inside the edges 1-2, 2-3, 3-1 and innerNode2 is inside the reverse.
      var edgePresent = flowsAcrossEdge((1, 2), innerNode2)(_)
      graph.edges.count(edgePresent) should be (1)

      edgePresent = flowsAcrossEdge((2, 3), innerNode2)(_)
      graph.edges.count(edgePresent) should be (1)

      edgePresent = flowsAcrossEdge((3, 1), innerNode2)(_)
      graph.edges.count(edgePresent) should be (1)

      edgePresent = flowsAcrossEdge((3, 2), innerNode1)(_)
      graph.edges.count(edgePresent) should be (1)

      edgePresent = flowsAcrossEdge((2, 1), innerNode1)(_)
      graph.edges.count(edgePresent) should be (1)

      edgePresent = flowsAcrossEdge((1, 3), innerNode1)(_)
      graph.edges.count(edgePresent) should be (1)
    }

    it ("should insert two edges from and to the same face if an edge occurs twice in a face") {

      Given("a triangular graph with a bridge inside one of its faces")
      val faces = Vector(new Face(Vector(1, 2, 4, 2, 3)), new Face(Vector(3, 2, 1))) // 2~4 and 4~2 will be traversed in that order
      val vertices = Vector(1, 2, 3, 4)
      val externalFace = 0 // The external face is the one with the bridge. Saves loc by having the face stored directly in the representation.

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the external face vertex should have two outgoing edges that points to itself")
      val graph = networkRep.graph

      val external = graph.get(networkRep.externalFace)
      graph.edges.count(toAndFromExists(external, external)) should be (2)

      And("there should be one edge-crossing edge with edge 2~>4 as its crossing")
      var edgePresent = flowsAcrossEdge((2, 4), graph.get(networkRep.externalFace))(_)
      graph.edges.count(edgePresent) should be (1)

      And("there should be one edge-crossing edge with edge 4~>2 as its crossing")
      edgePresent = flowsAcrossEdge((4, 2), graph.get(networkRep.externalFace))(_)
      graph.edges.count(edgePresent) should be (1)

    }

    it ("shouldn't insert edges between face vertices that aren't adjacent") {

      Given("a graph with two faces separate from each other")
      val oneFace = new Face(Vector(1, 3, 2))
      val notAdjacentFace = new Face(Vector(3, 4, 5))
      val faces = Vector(oneFace, new Face(Vector(2, 3, 5)), notAdjacentFace, new Face(Vector(1, 2, 5, 4, 3)))
      val vertices = Vector(1, 2, 3, 4, 5)
      val externalFace = 3

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("there shouldn't be any vertices between face 1, 3, 2 and 3, 4, 5")
      val graph = networkRep.graph
      val faceToVertex = networkRep.faceMapping.map(_.swap)
      val v1 = graph.get(faceToVertex.get(oneFace).get)
      val v2 = graph.get(faceToVertex.get(notAdjacentFace).get)

      graph.edges.count(toAndFromExists(v1, v2)) should be (0)
      graph.edges.count(toAndFromExists(v2, v1)) should be (0)

    }

    it ("shouldn't insert edges between regular vertices") {

      Given("a triangular graph with 2 faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("there shouldn't be any edges that start and stop at a regular vertex")
      val graph = networkRep.graph
      val allRegularVertices: collection.Set[FlowVertex] = for {
        node <- graph.nodes
        regular <- networkRep.vertexMapping.keySet
        if graph.get(regular) == node
      } yield regular

      graph.edges.filter(toAndFromInSet(allRegularVertices)(_)) should be ('empty)

    }

    it ("should assign every regular vertex the production 4") {

      Given("a graph")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("every flow vertex based on a regular vertex should have production 4")
      val flowVertices = networkRep.vertexMapping.keySet.toVector

      // There should be 3 of them in a triangular graph
      flowVertices(0).production should be (4)
      flowVertices(1).production should be (4)
      flowVertices(2).production should be (4)

    }

    it ("should assign every edge going from a regular vertex the minimum bound equal to the sum of vertex angles inside the face of the edge") {

      Given("a graph with two faces")
      val tri = triangularWithBridgeOnBothFaces
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)
      val vertexMap = networkRep.vertexMapping.map(_.swap)

      Then("an edge going out from a regular vertex that occurs once in the face should have lower bound 1")
      val flowVertex1 = vertexMap(1)
      val outgoingFrom1 = networkRep.graph.get(flowVertex1).outgoing.toVector
      outgoingFrom1(0).toOuter.lowerBound should be (1)
      outgoingFrom1(1).toOuter.lowerBound should be (1)

      And("an edge going out from a regular vertex that occurs twice in the face should have lower bound 2")
      val flowVertex2 = vertexMap(2)
      val outgoingFrom2 = networkRep.graph.get(flowVertex2).outgoing.toVector
      outgoingFrom2(0).toOuter.lowerBound should be (2)
      outgoingFrom2(1).toOuter.lowerBound should be (2)

    }

    it ("should assign every edge going from a regular vertex the capacity 4") {

      Given("a graph with two faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("an edge going out from a regular vertex should have capacity 4")
      val flowVertices = networkRep.vertexMapping.keySet.toVector

      // There should be 2 of them per vertex in a triangular graph
      val outgoingEdges = networkRep.graph.get(flowVertices(0)).outgoing.toVector
      outgoingEdges(0).toOuter.capacity should be (4)
      outgoingEdges(1).toOuter.capacity should be (4)

    }

    it ("should assign edges going from face vertices lower bound 0") {

      Given("a graph with two faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("an edge going out from a face vertex should have lower bound 0")
      val flowVertices = networkRep.faceMapping.keySet.toVector

      // There should be 2 of them per vertex in a triangular graph
      val outgoingEdges = networkRep.graph.get(flowVertices(0)).outgoing.toVector
      outgoingEdges(0).toOuter.lowerBound should be (0)
      outgoingEdges(1).toOuter.lowerBound should be (0)

    }

    it ("should assign edges going from face vertices infinite capacity") {

      Given("a graph with two faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("an edge going out from a face vertex should have capacity -1")
      val flowVertices = networkRep.faceMapping.keySet.toVector

      // There should be 2 of them per vertex in a triangular graph
      val outgoingEdges = networkRep.graph.get(flowVertices(0)).outgoing.toVector
      outgoingEdges(0).toOuter.capacity should be (-1)
      outgoingEdges(1).toOuter.capacity should be (-1)

    }

    it ("should assign the face vertex belonging to the external face the consumption (2 * # of vertex angles)  + 4") {

      Given("a graph with 4 vertices, one of which is a pendant, on the outer face")
      val tri = triangularWithBridgeOnBothFaces
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the external face should consume (2 * 5) + 4 units of flow")
      /*
       * Connecting a pendant to a node induces 3 vertex angles: 1 for the 360 degree angle at the top of the pendant,
       * and 2 (1 on each side) where the pendant connects to the rest of the graph.
       */
      networkRep.externalFace.production should be (-14)

    }

    it ("should assign face vertices belonging to internal faces the consumption (2 * # of vertex angles) - 4") {

      Given("a graph with 4 vertices, one of which is a pendant, on the inner face")
      val tri = triangularWithBridgeOnBothFaces
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)

      Then("the internal face should consume (2 * 5) - 4 units of flow")
      /*
       * Connecting a pendant to a node induces 3 vertex angles: 1 for the 360 degree angle at the top of the pendant,
       * and 2 (1 on each side) where the pendant connects to the rest of the graph.
       */
      val internalFlowVertex = networkRep.faceMapping.map(_.swap).get(intFace).get
      internalFlowVertex.production should be (-6)

    }

    it ("should assign every edge going from one face-vertex to another the cost 1") {

      Given("a triangular graph with two faces")
      val tri = triangular
      import tri._

      When("computing the flow network")
      val networkRep: NetworkRepresentation[Int] = networkConstructor.construct(vertices, faces, externalFace)
      val faceMap = networkRep.faceMapping.map(_.swap)
      val internalFlowFace = faceMap.get(internalFace).get
      val externalFlowFace = networkRep.externalFace

      Then("the edges between the internal and external face's flow verteices should have cost 1")

      // Each vertex only has one outgoing edge.
      val internalToExternal = networkRep.graph.get(internalFlowFace).outgoing.head
      val externalToInternal = networkRep.graph.get(externalFlowFace).outgoing.head

      internalToExternal.toOuter.cost should be (1)
      externalToInternal.toOuter.cost should be (1)

    }

  }

  /*
   * Filter function that finds edges that passes through a specific edge in the original graph, going
   * to a specified face-vertex.
   */
  private def flowsAcrossEdge(eTuple: (Int, Int), to: Graph[FlowVertex, FlowEdge]#NodeT)(x: Graph[FlowVertex, FlowEdge]#EdgeT) = {
    val edge: FlowEdge[FlowVertex] = x.toOuter
    if (edge.crosses.isDefined) {
      edge.crosses.get == eTuple && x.to == to
    } else false
  }

  /* Filter function that checks that an edge with the specified start- and- stop vertices exist. */
  private def toAndFromExists(to: Graph[FlowVertex, FlowEdge]#NodeT, from: Graph[FlowVertex, FlowEdge]#NodeT)(edge: Graph[FlowVertex, FlowEdge]#EdgeT) = {
    val inner = edge.toOuter
    inner.from == inner.to
  }

  /* Filter function that checks for edges where both to and from is contained in the specified set. */
  private def toAndFromInSet(vertices: collection.Set[FlowVertex])(e: Graph[FlowVertex, FlowEdge]#EdgeT) = {
    val edge: FlowEdge[FlowVertex] = e.toOuter
    vertices.contains(edge.from) && vertices.contains(edge.to)
  }
}
