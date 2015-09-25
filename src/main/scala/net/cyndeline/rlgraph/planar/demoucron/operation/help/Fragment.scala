package net.cyndeline.rlgraph.planar.demoucron.operation.help

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.pathfinding.{BFSPathfinder, Path}

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * A connected subset of a graph with two or more vertices (contact-vertices) belonging to the
 * so-far embedded section of the graph. If there's a face in the current embedding that
 * contains every contact-vertex of this fragment, it can be embedded into the face.
 *
 * @constructor Creates a new fragment.
 * @param component edges and vertices of the fragment.
 * @param contact every contact-vertex of the fragment.
 */
class Fragment[VType, EType[X] <: UnDiEdge[X]](val component: Graph[VType, EType], val contact: Set[VType]) {
  if (contact.size < 2) throw new IllegalArgumentException("Fragments must contain a minimum of two contact vertices to embed alpha path between (currently " + contact.size + ": " + contact + ")")
  if (!component.isConnected) throw new IllegalArgumentException("The fragment " + component + " is not connected")
  for (vertex <- contact) if (!component.contains(vertex)) throw new IllegalArgumentException("Contact vertex " + vertex + " was not found in the component " + component)

  for (node <- component.nodes.toOuter.toSet[VType]) if (component.get(node).edges.size == 1 && !contact.contains(node)) {
    throw new IllegalArgumentException("The node " + node + " only has one outgoing edge, but isn't present in the contact set.")
  }

  /**
   * Checks if a face is considered admissible, that is, if every contact vertex lies in the face.
   * @param face Face to check.
   * @return True if every contact vertex is in the face, otherwise false.
   */
  def admissible(face: Face[VType]): Boolean = contact.subsetOf(face.vertices.toSet)

  /**
   * Computes an alpha path through the fragment (i.e a path from one contact vertex to
   * another, with no other contact vertices along the path.
   * @param from the initial contact vertex in the path.
   * @param to the end contact vertex in the path, must differ from the start vertex.
   * @return A path going from the start vertex to the stop without crossing additional contact vertices.
   */
  def alphaPath(from: VType, to: VType): Path[VType, EType] = {
    if (from == to) throw new IllegalArgumentException("Alpha paths cannot start and stop in the same vertex (" + from + ")")
    if (!contact.contains(from)) throw new IllegalArgumentException("The start vertex " + from + " was not a contact vertex")
    if (!contact.contains(to)) throw new IllegalArgumentException("The stop vertex " + to + " was not a contact vertex")

    /* Alpha path can't contain any contact nodes other than start and stop. */
    val filteredContacts = contact - from - to
    val componentsWithoutContacts = component -- filteredContacts.toList
    val path = new BFSPathfinder().computePath[VType, EType](component.get(from), component.get(to), componentsWithoutContacts)
     .getOrElse(throw new IllegalArgumentException("The vertices " + from + " and " + to + " did not have a path between them in the fragment " + component))

    path
  }

  /**
   * Computes an alpha path through the fragment between two arbitrary contact vertices.
   * @return A path going from one contact vertex to another without crossing additional contact vertices.
   */
  def alphaPath: Path[VType, EType] = {
    val contactVertices = contact.toVector
    alphaPath(contactVertices(0), contactVertices(1))
  }

  override def toString: String = "Component: " + component + ", contacts: " + contact

  override def equals(other: Any): Boolean = other match {
    case f: Fragment[VType, EType] => f.component == component && f.contact == contact
    case _ => false
  }

  override def hashCode: Int = component.## ^ contact.##

}
