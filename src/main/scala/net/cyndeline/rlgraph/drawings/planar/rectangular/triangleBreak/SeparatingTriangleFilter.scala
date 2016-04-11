package net.cyndeline.rlgraph.drawings.planar.rectangular.triangleBreak

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.subgraph.triangles.Triangle
import net.cyndeline.rlgraph.util.IndexVertex

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Takes a set of triangles and a set of triangular faces and filters the triangles that aren't also a face. To do this
 * in O(n) time requires sorting the vertices in both faces and triangles. Due to this, index wrappers must be used,
 * giving each vertex its unique value.
 *
 * Algorithm description:
 *
 * First the vertices of each face and triangle are sorted according to their index, then ordered lexicographically such
 * that if two triangles/faces have different vertices in the same position in their vertex-array, the lowest differing
 * value is used (based on the array index it appears at) and the element whose vertex has the lowest value at that
 * index is ordered before the other.
 *
 * This difference is guaranteed to hold between every face/triangle in both lists, as they all differ in at least one
 * vertex. The only exception is if the total graph only has two faces and they're both triangular, but in this case
 * the triangle list will also only contain a single triangle, and the result is trivial to compute.
 *
 * @param allTriangles every triangle found in the graph, such that no triangle has all three of its edges as members in
 *                     other triangles.
 * @param triangularFaces Every face of length 3 in a planar biconnected graph.
 */
class SeparatingTriangleFilter[V : TypeTag : ClassTag](allTriangles: Vector[Triangle[V]], triangularFaces: Vector[Face[V]]) {
  require(!triangularFaces.exists(_.vertexSize != 3))

  /** All triangles that didn't share every vertex with one of the faces. */
  val triangles: Vector[Triangle[V]] = {
    val indexConversion = indexedVertices
    val triangles: Vector[Triangle[IndexVertex[V]]] = indexConversion._1
    val faces: Vector[Face[IndexVertex[V]]] = indexConversion._2

    // Start by handling the special case
    if (triangles.size == 1 && faces.size == 2) {
      trivialCase
    } else if (faces.isEmpty) {
      allTriangles

    } else {
      val facesWithOrderedVertices = faces.map(f => new Element(f, f.vertices.toArray))
      val trianglesWithOrderedVertices = triangles.map(t => new Element(t, t.toArray))
      lexicographicalArraySort(facesWithOrderedVertices)
      lexicographicalArraySort(trianglesWithOrderedVertices)

      // Order elements based on their array triples
      var sortedFaces = facesWithOrderedVertices.sortWith(_ < _)
      var sortedTriangles = trianglesWithOrderedVertices.sortWith(_ < _)

      // For every face, remove it until a face is found at the head of the list that matches the current triangle.
      // Triangles left when the face list is empty does not correspond to a face.
      val nonMatchingTriangles = new ListBuffer[Triangle[IndexVertex[V]]]()

      while (!sortedFaces.isEmpty && !sortedTriangles.isEmpty) {
        val nextFace = sortedFaces.head
        val nextTriangle = sortedTriangles.head

        if (nextFace.compare(nextTriangle) == 0) { // Equal
          sortedTriangles = sortedTriangles.drop(1)
          sortedFaces = sortedFaces.drop(1)

        } else if (nextTriangle.compare(nextFace) < 0) { // Triangle less than face
          nonMatchingTriangles += nextTriangle.value.asInstanceOf[Triangle[IndexVertex[V]]]
          sortedTriangles = sortedTriangles.drop(1)

        } else {
          sortedFaces = sortedFaces.drop(1)
        }
      }

      // If the face list runs out before the triangles, every triangle left is separating.
      removeIndex(nonMatchingTriangles.toVector ++ sortedTriangles.map(_.value.asInstanceOf[Triangle[IndexVertex[V]]]))
    }
  }

  /**
   * Sorts the vertices of a face or triangle according to their index, not their position in the face/triangle.
   * @param elements A list of faces/triangles tupled with their vertices in an array.
   */
  private def lexicographicalArraySort[E](elements: Vector[Element]) {
    for (e <- elements) {
      val sortedArray = e.array.sortWith(_.index < _.index) // RadixSort(e.array.map(a => (a, a.index)))
      e.array = sortedArray
    }
  }

  private def trivialCase: Vector[Triangle[V]] = {
    val f0 = triangularFaces(0).vertices.toSet
    val f1 = triangularFaces(1).vertices.toSet
    val t0 = allTriangles.head.toVector.toSet

    if (t0 == f0 || t0 == f1)
      Vector()
    else
      allTriangles
  }

  /* Converts triangles and faces into using index vertices. */
  private def indexedVertices: (Vector[Triangle[IndexVertex[V]]], Vector[Face[IndexVertex[V]]]) = {
    val vToIndex = new mutable.HashMap[V, IndexVertex[V]]()
    var nextIndex = 0
    val convertedTriangles = allTriangles.map(t => {
      val aIndex = vToIndex.get(t._1).getOrElse {
        val iv = IndexVertex(t._1, nextIndex)
        vToIndex += t._1 -> iv
        nextIndex += 1
        iv
      }
      val bIndex = vToIndex.get(t._2).getOrElse {
        val iv = IndexVertex(t._2, nextIndex)
        vToIndex += t._2 -> iv
        nextIndex += 1
        iv
      }
      val cIndex = vToIndex.get(t._3).getOrElse {
        val iv = IndexVertex(t._3, nextIndex)
        vToIndex += t._3 -> iv
        nextIndex += 1
        iv
      }
      Triangle(aIndex, bIndex, cIndex)
    })

    // Since the faces are a subset of the triangles, there's no need to create additional vertices
    val convertedFaces = triangularFaces.map(f => {
      val ivs = f.vertices.map(v => vToIndex.get(v).getOrElse {
        val iv = IndexVertex(v, nextIndex)
        nextIndex += 1
        iv
      })
      new Face(ivs)
    })

    (convertedTriangles, convertedFaces)
  }

  /* Maps index vertices to their values. */
  private def removeIndex(triangles: Vector[Triangle[IndexVertex[V]]]): Vector[Triangle[V]] = triangles.map(t => Triangle(t._1.valueOfIndex, t._2.valueOfIndex, t._3.valueOfIndex))

  /**
   * Generic container for both faces and triangles. Defines the ordering based on the current vertex indices such that
   * a lexicographical order is achieved when sorting elements.
   * @param value Either a face or a triangle.
   * @param a The initial array containing the vertices of the element.
   */
  private class Element(val value: Any, a: Array[IndexVertex[V]]) extends Ordered[Element] {

    /** Holds the updated order of the vertices. Refer to this and not the constructor parameter when checking
      * vertex order/values.
      */
    var array: Array[IndexVertex[V]] = a

    /**
     * Lexicographical compare. Indices are prioritized 0 < 1 < 2.
     */
    def compare(that: Element): Int = {
      if (array(0).index < that.array(0).index ||
          (array(0).index == that.array(0).index && array(1).index < that.array(1).index) ||
          (array(0).index == that.array(0).index && array(1).index == that.array(1).index && array(2).index < that.array(2).index)) {
        -1
      } else if (array(0).index == that.array(0).index && array(1).index == that.array(1).index && array(2).index == that.array(2).index)  {
        0
      } else {
        1
      }
    }

    override def toString: String = "[/" + value + " | " + array.mkString(", ") + "/]"
  }
}
