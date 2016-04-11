package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help

import scala.collection.mutable.ArrayBuffer

/**
 * A face of darts in an orthogonal representation.
 *
 * @constructor Constructs a new face of darts.
 * @param darts A list of darts as they are traversed in this face. The last dart connects to the first.
 * @tparam VType Vertex type represented in the face.
 */
class OFace[VType](val darts: Vector[Dart[VType]]) {
  if (darts.size < 2) throw new IllegalArgumentException("A face must contain at least two darts, currently: " + darts)
  brokenChainException()
  private val lookup = darts.toSet

  /**
   * Creates a face of darts from a dart sequence.
   * @param darts A list of darts as they are traversed in this face. The last dart connects to the first.
   * @return A new face of darts.
   */
  def this(darts: Dart[VType]*) = this(darts.toVector)

  /**
   * Checks if the face contains a specific dart.
   * @param dart Dart to check existence for.
   * @return true if the dart is a member of this face, otherwise false.
   */
  def contains(dart: Dart[VType]): Boolean = lookup.contains(dart)

  /**
   * Looks up membership of dart vertices based on wrappers.
   * @param from Start wrapper.
   * @param to Stop wrapper.
   * @return True if a dart with from/to vertices exists, otherwise false.
   */
  def contains(from: VertexWrapper[VType], to: VertexWrapper[VType]): Boolean = getDart(from, to).isDefined

  /**
   * Retrieves a dart from the face.
   * @param from Start wrapper.
   * @param to Stop wrapper.
   * @return A dart containing the start/stop vertices if one exists, otherwise None.
   */
  def getDart(from: VertexWrapper[VType], to: VertexWrapper[VType]): Option[Dart[VType]] = {
    for (dart <- darts)
      if (dart.from == from && dart.to == to) return Option(dart)

    None
  }

  /**
   * Iterates over every vertex in the darts of the face. The last dart contains a reference to the first vertex
   * in the first dart, but this iterator only contains a single instance of each vertex.
   * @return Iterator over every vertex in the face.
   */
  def vertices: Iterator[VertexWrapper[VType]] = {
    val vs = new ArrayBuffer[VertexWrapper[VType]]()
    val ds = darts.iterator

    while (ds.hasNext) {
      val dart = ds.next()
      vs += dart.from
    }

    vs.iterator
  }

  /**
   * Creates an iterator around the faces vertices, starting at a specified dart.
   *
   * @param from Start wrapper.
   * @param to Stop wrapper.
   * @return An iterator over all darts in the face, starting at the dart having the specified to/from values.
   */
  def iteratorFrom(from: VertexWrapper[VType], to: VertexWrapper[VType]): Iterator[Dart[VType]] = {
    val dart = getDart(from, to)
    if (dart.isEmpty) return Vector().iterator

    val result = new ArrayBuffer[Dart[VType]]()
    val buffer = new ArrayBuffer[Dart[VType]]()
    val dartIt = darts.iterator
    var found = false
    while (dartIt.hasNext) {
      val d = dartIt.next()
      if (!found && d == dart.get)
        found = true

      if (found) result += d
      else buffer += d
    }

    (result ++ buffer).iterator
  }

  /**
   * Creates an iterator around the faces vertices, starting at a specified dart.
   * @param dart Dart to start iterating from.
   * @return An iterator over all darts in the face, starting at the specified dart.
   */
  def iteratorFrom(dart: Dart[VType]): Iterator[Dart[VType]] = iteratorFrom(dart.from, dart.to)


  /**
   * Returns the number of darts in this face.
   * @return the number of darts in this face.
   */
  def size = darts.size

  /**
   * Checks if a face is adjacent to this face.
   * @param face Face to check adjacency for.
   * @return The list of edges the two faces share.
   */
  def edgesAdjacentTo(face: OFace[VType]): Vector[Dart[VType]] = {
    val result = new ArrayBuffer[Dart[VType]]()

    /* Store every dart that runs opposite of a dart in this face. */
    for (otherDart <- face.darts; dart <- darts)
      if (otherDart.to == dart.from && otherDart.from == dart.to)
        result += otherDart

    result.toVector
  }

  /**
   * Replaces all darts in a range with a new list of darts.
   * @param from Dart to start replacing at (inclusive)
   * @param to Dart to stop replacing at (inclusive)
   * @param replacement Darts to replace the range with.
   * @return a new face with darts replaced.
   */
  def replaceDartSubset(from: Dart[VType], to: Dart[VType], replacement: Vector[Dart[VType]]): OFace[VType] = {

    /* Find the last dart, then keep only the ones that comes after it up until the first. */
    val dartsToKeep = iteratorFrom(to)
    dartsToKeep.next() // Get rid of 'to
    val newFace = new ArrayBuffer[Dart[VType]]()
    var stop = false

    while (dartsToKeep.hasNext && !stop) {
      val dart = dartsToKeep.next()

      if (dart == from) {
        stop = true
      } else {
        newFace += dart
      }
    }

    new OFace(newFace.toVector ++ replacement)
  }

  /**
   * Checks if the faces vertex-degrees are rectangular, assuming it is an internal face.
   * @return true if the face only contains internal 90 degree or 180 degree bends, otherwise false.
   */
  def isInternallyRectangular: Boolean = {
    for (dart <- darts)
      if (dart.nextDegree != 1 && dart.nextDegree != 2) return false

    true
  }

  /**
   * Checks if the faces vertex-degrees are rectangular, assuming it is an external face.
   * @return true if the face only contains internal 90 degree or 180 degree bends, otherwise false.
   */
  def isExternallyRectangular: Boolean = {
    for (dart <- darts)
      if (dart.nextDegree != 3 && dart.nextDegree != 2) return false

    true
  }

  /**
   * Returns the subsequent dart that comes after another dart in the face.
   * @param dart Dart to retrieve dart after.
   * @return the dart that comes after the specified dart.
   */
  def getDartAfter(dart: Dart[VType]): Option[Dart[VType]] = {
    val dartIt = iteratorFrom(dart)
    if (dartIt.isEmpty) {
      None
    } else {
      dartIt.next()
      Option(dartIt.next())
    }
  }

  /**
   * Returns a dart with both vertices specified.
   * @param from First vertex in the dart.
   * @param to Second vertex in the dart.
   * @return a dart with both vertices specified.
   */
  def getDart(from: VType, to: VType): Option[Dart[VType]] = {
    for (dart <- darts)
      if (dart.from.isDefined && dart.to.isDefined && dart.from.value.get == from && dart.to.value.get == to)
        return Option(dart)

    None
  }

  /**
   * Creates a new face by replacing a simple dart with multiple darts.
   *
   * @param dartToReplace Dart that should be replaced. Can't have bends.
   * @param replaceWith A chain of darts to replace it with. Note that since the replaced dart doesn't have bends,
   *                    each dart must have a 180 degree bend to the next and no edge-bends.
   * @param respectBend True if the final outgoing bend should match the bend of the last element in the inserted
   *                    darts, otherwise false.
   * @return a new face with the new dart.
   */
  def replaceDart(dartToReplace: Dart[VType], replaceWith: Vector[Dart[VType]], respectBend: Boolean = true): OFace[VType] = {
    if (dartToReplace.bends != 0)
      throw new IllegalArgumentException("Dart " + dartToReplace + " has bends, can't be replaced with " + replaceWith)

    for (dart <- replaceWith)
      if (dart.bends != 0)
        throw new IllegalArgumentException("Dart " + dart + " has bends, not allowed when replacing a face-dart.")

    if (!contains(dartToReplace))
      throw new IllegalArgumentException("Cannot replace " + dartToReplace + " in face " + this + " since the dart isn't present.")

    if (replaceWith.head.from != dartToReplace.from)
      throw new IllegalArgumentException("Replacing " + dartToReplace + " with " + replaceWith + " is not allowed since the replacement doesn't begin on the replaced darts .from value")

    if (replaceWith.reverse.head.to != dartToReplace.to)
      throw new IllegalArgumentException("Replacing " + dartToReplace + " with " + replaceWith + " is not allowed since the replacement doesn't end on the replaced darts .to value")

    val replaceIterator = replaceWith.iterator
    while (replaceIterator.hasNext) {
      val n = replaceIterator.next()

      if (!replaceIterator.isEmpty && n.nextDegree != 2)
        throw new IllegalArgumentException("All intermediate replacing darts must have vertex-bend 2")
    }

    if (respectBend && replaceWith.reverse.head.nextDegree != dartToReplace.nextDegree)
      throw new IllegalArgumentException("The final replacement bend must match the replaced darts.")

    new OFace(replace(darts.toList, Nil, dartToReplace, replaceWith.toList).toVector)

  }

  override def toString: String = "OFace[" + darts.toString + "]"

  override def equals(other: Any) = other match {
    case o: OFace[VType] => o.darts == darts
    case _ => false
  }

  override def hashCode: Int = darts.##

  /**
   * Finds a dart, and replaces it with a list of darts.
   * @param allDarts All darts left to search through.
   * @param previous Previously searched darts.
   * @param lookingFor Dart to look for.
   * @param replacement List of darts to replace the dart with when found.
   * @return A new list with the replacements inserted at the same spot as the previous dart.
   */
  private def replace(allDarts: List[Dart[VType]],
                       previous: List[Dart[VType]],
                       lookingFor: Dart[VType],
                       replacement: List[Dart[VType]]): List[Dart[VType]] = allDarts match {
    case head :: tail => if (head == lookingFor) previous ::: replacement ::: tail
                          else replace(tail, previous :+ head, lookingFor, replacement)

    case Nil => previous // Dart to look for not found
  }

  /**
   * Throws an exception unless every darts .to value equals the next darts .from.
   */
  private def brokenChainException() {
    var n = 0

    // -1 since the alg. should stop after there's only two darts left
    while (n < darts.size - 1) {
      val d1 = darts(n)
      val d2 = darts(n + 1)

      if (d1.to != d2.from) chainException(d1, d2)
      n += 1
    }

    if (darts(0).from != darts(darts.size - 1).to)
      chainException(darts(0), darts(darts.size - 1))
  }

  private def chainException(d1: Dart[VType], d2: Dart[VType]) =
    throw new IllegalArgumentException("The chain of darts in face " +
      darts + " is broken, since dart " + d1 + " doesn't end in the same vertex that " + d2 + " starts at.")

}
