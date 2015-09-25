package net.cyndeline.rlgraph.planarGraphDrawing.rectangular.triangleBreak.help

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import net.cyndeline.rlgraph.util.GraphCommons

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Graph structure described by Gianluca Quercini in "Optimizing and Visualizing Planar Graphs via Rectangular
 * Dualization".
 *
 * Every vertex b in a bubble-graph represents an edge in a triangle. An edge from b1 to b2 exists only if the edges
 * corresponding to b1 and b2 are members of the same triangle in the original graph. This means that every
 * bubble-vertex always has an even degree. If two triangles share an edge, the b-vertex representing them will
 * have 4 edges connected to it. Six if three triangles share the edge etc. Two bubbles will never share two edges
 * with each other, as that would imply multi-edges.
 *
 * As every triangle shares at most one edge with any given neighbor, 2 distinct bubbles share at most one vertex.
 *
 * New graphs are created using the companion factory.
 */
class BubbleGraph[V : TypeTag] private (initialBubbles: Vector[Bubble[V]], initialBubbleMembership: Map[BubbleVertex[V], Vector[Bubble[V]]]) {
  private val bubbleMembership = new mutable.HashMap[BubbleVertex[V], Vector[Bubble[V]]]()
  private val allBubbles = new ListBuffer[Bubble[V]]()
  bubbleMembership ++= initialBubbleMembership
  allBubbles ++= initialBubbles

  /** @return Every bubble in the graph. */
  def bubbles: Vector[Bubble[V]] = allBubbles.toVector

  /** @return Every bubble-vertex in the graph, current present in one or more bubbles. */
  def bubbleVertices: Vector[BubbleVertex[V]] = bubbleMembership.keys.toVector

  /** @return Every bubble a vertex is currently a member of in the graph. */
  def bubblesOfVertex(v: BubbleVertex[V]): Vector[Bubble[V]] = bubbleMembership.get(v).getOrElse(Vector())

  /**
   * Removes every bubble incident to a bubble-vertex, and updates the weights of the remaining vertices of the bubble
   * that still are connected to other bubbles.
   * @param v Vertex to remove, thereby "popping" the bubble.
   * @return Every connected bubble graph that results from popping one or more vertices (thus disconnecting the
   *         current graph). If this graph does not become disconnected by removing the specified vertex's bubble,
   *         this graph instance is returned as the sole element in the list.
   */
  def popBubbles(v: BubbleVertex[V]): Vector[BubbleGraph[V]] = {
    require(bubbleMembership.contains(v), "Vertex " + v + " is not a member of any bubble.")
    val vertexBubbles = bubbleMembership(v)
    for (b <- vertexBubbles) {
      b.pop() // Updates vertex weights

      for (v <- b.vertices) {
        val bubbleList = bubbleMembership(v)
        val bubbleRemoved = bubbleList diff Vector(b)
        if (bubbleRemoved.isEmpty) {
          bubbleMembership -= v
        } else {
          bubbleMembership += v -> bubbleRemoved
        }
      }
    }

    allBubbles --= vertexBubbles

    /* Check for disconnects: Add edges between every vertex in every remaining bubble and check for disconnected
     * components. For each such component, put all bubbles in its own graph.
     */
    val allEdges: Vector[UnDiEdge[BubbleVertex[V]]] = (for (b <- bubbles) yield Vector(b.v1~b.v2, b.v2~b.v3, b.v3~b.v1)).flatten
    val totalGraph = Graph.from(Nil, allEdges)
    val splitGraphs = GraphCommons.allConnectedComponents(totalGraph)

    if (splitGraphs.size < 2) {
      Vector(this)

    } else {
      val allBubbleGraphs = new ListBuffer[BubbleGraph[V]]()

      for (subgraph <- splitGraphs) {
        val bubblesInGraph = new ListBuffer[Bubble[V]]()
        val bubbleMembership = new mutable.HashMap[BubbleVertex[V], Vector[Bubble[V]]]()
        val addedBubbles = new mutable.HashSet[Bubble[V]]()

        for (n <- subgraph.nodes; bubble <- bubblesOfVertex(n)) {
          bubbleMembership += ((n, bubblesOfVertex(n)))
          addedBubbles += bubble
          bubblesInGraph += bubble
        }

        allBubbleGraphs += new BubbleGraph[V](bubblesInGraph.toVector.distinct, bubbleMembership.toMap)
      }

      allBubbleGraphs.toVector
    }
  }

  /** @return The number of bubbles left in the graph. */
  def size: Int = allBubbles.size

  /** @return True if no bubbles exist in the graph, otherwise false. */
  def isEmpty: Boolean = size < 1

  override def toString: String = {
    val builder = new StringBuilder()
    val nl = System.getProperty("line.separator")

    builder ++= "BubbleGraph: " + nl

    for (bubble <- allBubbles) {
      builder ++= bubble.toString + nl

      for (bv <- bubble.vertices) {
        builder ++= bv.toString + " :: Membership: " + (if (!bubbleMembership.contains(bv)) "" else bubbleMembership(bv)) + nl
      }

      builder ++= nl
    }

    builder.toString()
  }
}

/**
 * Constructs all bubble-graphs from a list of separating triangles, where each graph contains a single group of
 * connected bubbles.
 */
object BubbleGraph {

  /**
   * @param separatingTriangles The list of every separating triangle in a graph. Can contain disconnected sets
   *                            of triangles.
   * @tparam V Vertex type in the graph.
   * @return A list of all bubble graphs derived from the triangle set, with each graph only containing connected
   *         triangles.
   */
  def apply[V : TypeTag](separatingTriangles: Vector[Triangle[V]]): Vector[BubbleGraph[V]] = {

    /* Keeps track of which edges has already had a bubble-vertex constructed, such that an edge present in
     * multiple bubbles is assigned the same bubble-vertex.
     */
    val vertexToBubbleVertex = new mutable.HashMap[UnorderedPair[V], BubbleVertex[V]]()
    val bubbleMembership = new mutable.HashMap[BubbleVertex[V], Vector[Bubble[V]]]()
    val bubbleEdges = new ListBuffer[UnDiEdge[BubbleVertex[V]]]()

    // Start by creating all bubbles and vertices in them
    for (triangle <- separatingTriangles) {
      val bv1 = createBubbleVertex(triangle._1, triangle._2, vertexToBubbleVertex)
      val bv2 = createBubbleVertex(triangle._2, triangle._3, vertexToBubbleVertex)
      val bv3 = createBubbleVertex(triangle._3, triangle._1, vertexToBubbleVertex)
      val bubble = new Bubble(bv1, bv2, bv3)
      addBubbleMembership(bv1, bubble, bubbleMembership)
      addBubbleMembership(bv2, bubble, bubbleMembership)
      addBubbleMembership(bv3, bubble, bubbleMembership)

      bubbleEdges ++= Vector(bv1~bv2, bv2~bv3, bv1~bv3)
    }

    // Splits the bubbles into disconnected components. For each vertex in each set, assign its bubble to a graph
    // representing that component.
    val connectedComponents = GraphCommons.allConnectedComponents(Graph.from(Nil, bubbleEdges.toVector))

    val bubbleGraphs = new ListBuffer[BubbleGraph[V]]()
    for (dc <- connectedComponents) {
      val bubblesInGraph = new ListBuffer[Bubble[V]]()
      val addedBubbles = new mutable.HashSet[Bubble[V]]()

      for (n <- dc.nodes) {
        val bubbles = bubbleMembership(n)
        for (b <- bubbles if !addedBubbles.contains(b)) {
          bubblesInGraph += b
          addedBubbles += b
        }
      }

      bubbleGraphs += new BubbleGraph(bubblesInGraph.toVector, bubbleMembership.toMap[BubbleVertex[V], Vector[Bubble[V]]])
    }

    bubbleGraphs.toVector
  }

  private def createBubbleVertex[V](a: V, b: V, current: mutable.HashMap[UnorderedPair[V], BubbleVertex[V]]): BubbleVertex[V] = {
    val edge = UnorderedPair(a, b)
    current.get(edge).getOrElse {
      val newBubbleV = BubbleVertex(a, b)
      current += edge -> newBubbleV
      newBubbleV
    }
  }

  private def addBubbleMembership[V](bv: BubbleVertex[V], b: Bubble[V], membership: mutable.HashMap[BubbleVertex[V], Vector[Bubble[V]]]) {
    if (membership.contains(bv)) {
      val currentBubbles = membership(bv)
      membership += bv -> (b +: currentBubbles)
    } else {
      membership += bv -> Vector(b)
    }
  }
}
