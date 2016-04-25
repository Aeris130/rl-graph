package net.cyndeline.rlgraph.drawings.planar.grid.binaryTree

/**
  * Stores the vertices in a grid drawing as the drawing is being constructed using a canonical ordering, along with
  * the orderings outer boundary C(k). The left child (if one exists) of a vertex v in the tree is the leftmost vertex
  * that v ended up covering when v was added o C(k). The right child (if any) is the right neighbor of v on C(k).
  *
  * @param left A vector of length n, where 0 to n-1 are the vertices in the tree. A value v >= 0 at index i means that
  *             v is the left child of the vertex having index i.
  * @param right Analogous to left.
  */
class LeftRightTree private (left: Vector[Int], right: Vector[Int]) {

  /**
    * @param nrOfVertices The number of vertices that the tree should support.
    * @return An empty tree capable of containing vertices of value 0 to the highest vertex index.
    */
  def this(nrOfVertices: Int) = {
    this(Vector.fill(nrOfVertices)(-1), Vector.fill(nrOfVertices)(-1))
    require(nrOfVertices > 0, "The number of vertices in the tree must be specified, cannot be less than 1.")
  }

  def leftChild(v: Int): Int = {
    require(left(v) >= 0, "No left child found for vertex " + v)
    left(v)
  }

  def rightChild(v: Int): Int = {
    require(right(v) >= 0, "No right child found for vertex " + v)
    right(v)
  }

  def hasLeft(v: Int): Boolean = left(v) >= 0
  def hasRight(v: Int): Boolean = right(v) >= 0

  def addLeft(v: Int, leftChild: Int): LeftRightTree = {
    require(left(v) < 0, "Cannot add a left child to vertex " + v + " as one is already present in the tree.")
    new LeftRightTree(left.updated(v, leftChild), right)
  }

  def addRight(v: Int, rightChild: Int): LeftRightTree = {
    require(right(v) < 0, "Cannot add a right child to vertex " + v + " as one is already present in the tree.")
    new LeftRightTree(left, right.updated(v, rightChild))
  }

  def removeLeft(v: Int): LeftRightTree = {
    require(left(v) >= 0, "Cannot remove the left child from vertex " + v + " as no child is presently in the tree.")
    new LeftRightTree(left.updated(v, -1), right)
  }

  def removeRight(v: Int): LeftRightTree = {
    require(right(v) >= 0, "Cannot remove the right child from vertex " + v + " as no child is presently in the tree.")
    new LeftRightTree(left, right.updated(v, -1))
  }

}
