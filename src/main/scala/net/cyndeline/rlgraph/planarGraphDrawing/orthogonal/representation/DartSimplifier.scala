package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.darts.SimpleDart
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.representation.help.{Dart, VertexWrapperFactory}

import scala.collection.mutable.ArrayBuffer

/**
 * Simplifies a single dart by splitting it into multiple sub-darts for every bend
 * along its edge, removing the bends.
 *
 * @constructor Creates a new dart simplifier.
 * @tparam VType Vertex type in the graph the darts represent.
 */
class DartSimplifier[VType](wrapperFactory: VertexWrapperFactory[VType]) {

  /**
   * Removes every bend in a dart by creating a new dart leading up to every bend, with the same degree value
   * towards the next dart as the bend would've had.
   *
   * Note that it is assumed that each dart runs clockwise along its face, and that an internal 90 degree bend
   * refers to the right side of the dart.
   *
   * @param dart Dart to simplify.
   * @param oppositeDart Dart that runs opposite of the dart to simplify (x->y => y->x). This is needed since
   *                     each dart only contains the inward (90 degree) bends it provides to its face. 270 degree
   *                     bends are supplied by the opposite dart.
   * @return A list of all sub-darts induced by the edge bends, in the order they should occur on the face of
   *         the original dart. The first Dart in this list contains the original .from value as its .from, and
   *         the last dart contains the .to equivalent.
   */
  def simplifyDart(dart: Dart[VType], oppositeDart: Dart[VType]): Vector[Dart[VType]] = {
    if (dart.bends < 1 && oppositeDart.bends < 1) return Vector()

    if (dart.to != oppositeDart.from || dart.from != oppositeDart.to)
      throw new IllegalArgumentException("The start/stop vertices of " + dart + " did not match stop/start of the simplified dart chain " + oppositeDart)

    val dartResult = new ArrayBuffer[Dart[VType]]()

    // The first dart begins at the original start value.
    var current = dart.from

    for (i <- 0 until dart.bends) {
      val nextDummy = wrapperFactory.createEmptyVertexWrapper
      dartResult += new SimpleDart[VType](current, nextDummy, 1, dart)
      current = nextDummy
    }

    for (i <- 0 until oppositeDart.bends) {
      val nextDummy = wrapperFactory.createEmptyVertexWrapper
      dartResult += new SimpleDart[VType](current, nextDummy, 3, dart)
      current = nextDummy
    }

    /* Finally, the last edge bends in the original vertex-bend direction. */
    dartResult += new SimpleDart[VType](current, dart.to, dart.nextDegree, dart)
    dartResult.toVector
  }

  /**
   * Simplifies a dart based on an already simplified instance of its opposite, by creating new darts using
   * the simplifications from/to values, but in the opposite order. Bends between darts are copied and inversed
   * from the opposite dart list.
   *
   * @param dart Dart to simplify.
   * @param simplifiedDart List of sub-darts making up the darts simplified opposite.
   * @return A list of all sub-darts induced by the edge bends, in the order they should occur on the face of
   *         the original dart. The first Dart in this list contains the original .from value as its .from, and
   *         the last dart contains the .to equivalent.
   */
  def simplifySimpleDart(dart: Dart[VType], simplifiedDart: Vector[Dart[VType]]): Vector[Dart[VType]] = {
    if (dart.to != simplifiedDart.head.from || dart.from != simplifiedDart(simplifiedDart.size - 1).to)
      throw new IllegalArgumentException("The start/stop vertices of " + dart + " did not match stop/start of the simplified dart chain " + simplifiedDart)

    val dartResult = new ArrayBuffer[Dart[VType]]()
    val reversedDarts = simplifiedDart.reverse

    for (i <- 0 until reversedDarts.size) {
      val d = reversedDarts(i)
      val nextDegree = if (i < reversedDarts.size - 1)
                        reverseDegree(reversedDarts(i + 1).nextDegree)
                      else
                        dart.nextDegree

      dartResult += new SimpleDart[VType](d.to, d.from, nextDegree, dart)
    }
    dartResult.toVector
  }

  private def reverseDegree(degree: Int) =
    if (degree == 1) 3
    else if (degree == 3) 1
    else throw new IllegalArgumentException("Edge bends can only be 90 (1) or 270 (3) degrees, not " + degree)

}
