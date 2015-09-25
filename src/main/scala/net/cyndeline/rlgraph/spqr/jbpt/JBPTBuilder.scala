package net.cyndeline.rlgraph.spqr.jbpt

import net.cyndeline.rlgraph.spqr._
import net.cyndeline.rlgraph.util.graphConverters.ConverterData
import net.cyndeline.rlgraph.util.graphConverters.jbpt.JBPTConverter
import net.cyndeline.rlgraph.util.graphConverters.jbpt.hashStructure.JBPTHashGraph
import org.jbpt.algo.tree.tctree.{TCTree, TCTreeNode, TCType}
import org.jbpt.graph.Edge
import org.jbpt.graph.abs.AbstractEdge
import org.jbpt.hypergraph.abs.Vertex

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Wraps the jbpt library's tree class. Uses custom hash structures and node content sorting to eliminate the random
 * order that nodes appear in when building the tree using jbpt.
 *
 * @tparam VType Vertex type in the graph.
 * @tparam EType Edge type in the graph.
 */
class JBPTBuilder[VType, EType[X] <: UnDiEdge[X] : ({type l[M[_]] = TypeTag[M[VType]]})#l] extends TreeBuilder[VType, EType] {

  /**
   * @param graph A planar biconnected graph.
   * @return An SPQR tree based on the input graph.
   */
  def buildTree(graph: Graph[VType, EType]): SPQRTree[VType] = {
    def defaultRootIndex(a: Array[Set[VType]]): Int = 0
    buildTreeWithRoot(graph, defaultRootIndex)
  }

  /**
   * Builds an SPQR tree rooted in a node selected by the user.
   * @param graph A planar biconnected graph.
   * @param rootSelection A function that takes a set of node vertices, and returns the index of the node that should be
   *                      used as root.
   * @return An SPQR tree based on the input graph.
   */
  def buildTreeWithRoot(graph: Graph[VType, EType], rootSelection: Array[Set[VType]] => Int): SPQRTree[VType] = {
    val jbptConverter = new JBPTConverter[VType, EType]()
    val jbptGraphData: ConverterData[VType, EType, JBPTHashGraph, Vertex, Edge] = jbptConverter.convertTo(graph)
    val newToOldVertices: Map[Vertex, VType] = jbptGraphData.vertexMap.map(_.swap)
    val spqrTree = new TCTree(jbptGraphData.convertedGraph)

    /* Special case: If the input graph consists of a single triconnected component, the SPQR tree will be empty rather
     * than containing a single node. Build the single-vertex tree manually.
     */
    if (spqrTree.getRoot == null) { // Root is only null if the tree is empty
    val rootNode = new HashNode[VType](0, Rigid)
      for (edge <- graph.edges)
        rootNode.addSolidEdge(new OrderedEdge[VType](edge._1, edge._2))

      return new JBPTTree[VType](rootNode)
    }

    trimTrivialNodes(spqrTree)
    val rootCandidates = sortTreeNodes(spqrTree.getTCTreeNodes.toVector)
      .filter(n => n.getType == TCType.RIGID || n.getType == TCType.POLYGON)
      .toArray
    val verticesInNodes = rootCandidates
      .map(n => n.getSkeleton.getVertices.map(v => newToOldVertices(v)).toSet)
      .toArray
    val selectedRoot = rootCandidates(rootSelection(verticesInNodes))
    spqrTree.reRoot(selectedRoot)

    constructTree(jbptGraphData, newToOldVertices, spqrTree)
  }

  /**
   * @return An SPQR tree based on the input graph.
   */
  private def constructTree(jbptGraphData: ConverterData[VType, EType, JBPTHashGraph, Vertex, Edge],
                            newToOldVertices: Map[Vertex, VType],
                            spqrTree: TCTree[Edge, Vertex]): SPQRTree[VType] = {

    val root = spqrTree.getRoot
    trimTrivialNodes(spqrTree)
    var jbptToNode = Map[TCTreeNode[Edge, Vertex], HashNode[VType]]()
    var nodeId = 0

    val rootNode = new HashNode[VType](nodeId, spqrType(root))
    nodeId += 1
    jbptToNode += (root -> rootNode)
    val finalTree = new JBPTTree[VType](rootNode)

    /* Stores each level of children as they are visited using a breadth first traversal. */
    var nodesToBeProcessed: Vector[TCTreeNode[Edge, Vertex]] = spqrTree.getDirectSuccessors(root).toVector

    /* Step 1: Create a new node entry for each TCTreeNode and set its parent. */
    while (!nodesToBeProcessed.isEmpty) {
      val sortedNodes = sortTreeNodes(nodesToBeProcessed)
      val childrenOfNodes = new ArrayBuffer[TCTreeNode[Edge, Vertex]]()
      val it = sortedNodes.iterator

      while (it.hasNext) {
        val n = it.next()
        val originalParent = spqrTree.getParent(n)
        val sharedVertices = originalParent.getSkeleton.getVertices.toVector intersect n.getSkeleton.getVertices.toVector
        if (sharedVertices.size > 2)
          throw new Error("Parent and child cannot share more than 2 vertices.")

        val cutPair = (sharedVertices(0), sharedVertices(1))
        val parentCutPair = (newToOldVertices(cutPair._1), newToOldVertices(cutPair._2))
        val parent: HashNode[VType] = jbptToNode(spqrTree.getParent(n))
        val newNode = new HashNode[VType](nodeId, parent, parentCutPair, spqrType(n))
        jbptToNode += (n -> newNode)
        nodeId += 1
        finalTree.addChild(newNode)

        childrenOfNodes ++= spqrTree.getDirectSuccessors(n).toVector
      }

      nodesToBeProcessed = childrenOfNodes.toVector
    }

    /* Step 2: For each node / TCTreeNode pair, add all edges from the TCTreeNode to the new node.
     * Process order is based on the id of the new nodes, so as to prevent the ordering of tctree nodes
     * from affecting the result.
     */
    for ((tctNode, newNode) <- jbptToNode.toVector.sortBy(_._2.id)) {
      addEdgesToNode(newNode, tctNode, newToOldVertices)
    }

    finalTree
  }

  private def addEdgesToNode(node: HashNode[VType], original: TCTreeNode[Edge, Vertex], vertexMap: Map[Vertex, VType]) {
    val virtualEdges = original.getSkeleton.getVirtualEdges
      .toVector
      .map((e: AbstractEdge[Vertex]) => new OrderedEdge(vertexMap(e.getV1), vertexMap(e.getV2)))
      .sortBy(_.##)

    val solidEdges = original.getSkeleton.getOriginalEdges
      .toVector
      .map(e => new OrderedEdge(vertexMap(e.getV1), vertexMap(e.getV2)))
      .sortBy(_.##)

    for (se <- solidEdges)
      node.addSolidEdge(se)

    for (ve <- virtualEdges)
      node.addVirtualEdge(ve)
  }

  /** The jbpt tree uses trivial edges to represent every edge in the graph. Those aren't needed here. */
  private def trimTrivialNodes(tree: TCTree[Edge, Vertex]) {
    val trivialNodes = tree.getTCTreeNodes.filter(_.getType == TCType.TRIVIAL).toIterator

    for (node <- trivialNodes) {
      tree.removeVertex(node)
    }
  }

  private def spqrType(node: TCTreeNode[Edge, Vertex]): NodeType = {
    node.getType match {
      case TCType.BOND => Parallel
      case TCType.RIGID => Rigid
      case TCType.POLYGON => Series
      case TCType.TRIVIAL => throw new Error("Trivial nodes should not be processed.")
      case TCType.UNDEFINED => throw new Error("Undefined nodes should not be processed.")
    }
  }

  /**
   * Sorts a list of TCTree nodes based on their skeleton edges and vertices. The following orderings are
   * disregarded when sorting:
   *
   *  Edge ordering (1-3, 3-2 should be the same as 3-2, 1-3)
   *  Vertex ordering in a single edge (1-3 should equal 3-1)
   */
  private def sortTreeNodes(nodes: Vector[TCTreeNode[Edge, Vertex]]): Vector[TCTreeNode[Edge, Vertex]] = {
    nodes
      .map(v => (v, tcTreeNodeHash(v))) // Compute the hash once for each node
      .sortBy(_._2) // Sort using the new hash
      .map(_._1)
  }

  /** Computes hash codes for TCTreeNodes based on their original and virtual edges. */
  private def tcTreeNodeHash(node: TCTreeNode[Edge, Vertex]): Int = {
    val originalEdgeHash: Int = node.getSkeleton.getOriginalEdges
      .toVector
      .sortWith((a, b) => abstractEdgeHash(a) < abstractEdgeHash(b))
      .map((edge: AbstractEdge[Vertex]) => abstractEdgeHash(edge))
      .hashCode()

    val virtualEdgeHash: Int = node.getSkeleton.getVirtualEdges
      .toVector
      .sortWith((a: AbstractEdge[Vertex], b: AbstractEdge[Vertex]) => abstractEdgeHash(a) < abstractEdgeHash(b))
      .map((edge: AbstractEdge[Vertex]) => abstractEdgeHash(edge))
      .hashCode()

    originalEdgeHash ^ virtualEdgeHash
  }

  /** Computes a hash irrespective of its vertex positions. */
  private def abstractEdgeHash(edge: AbstractEdge[Vertex]): Int = {
    edge.getV1.## ^ edge.getV2.##
  }

}
