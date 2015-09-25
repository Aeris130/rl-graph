package net.cyndeline.rlgraph.triangulation.fourConnectivity.help

import net.cyndeline.rlgraph.face.Face
import net.cyndeline.rlgraph.util.GraphCommons

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
 * Splits a face from a biconnected graph into two, based on two separate vertex intervals. Example: Splitting the
 * face [1, 2, 3, 4, 5, 6] at vertices 2 and 4 gives the faces [2, 3, 4] and [4, 5, 6, 1, 2].
 *
 * Since this class is used to triangulate faces, an error is thrown if a resulting face split has size < 3.
 */
class FaceSplit[V : TypeTag : ClassTag](face: Face[V], from: V, to: V) {
  private val fromFirst = GraphCommons.placeElementFirst(from, face.vertices)
  private val span: (Vector[V], Vector[V]) = fromFirst.span(_ != to)
  private val split1 = span._1 :+ to // from -> to
  private val split2 = span._2 :+ from // to -> from

  /** The face containing every vertex in the interval from -> to in the original face. */
  val face1 = new Face(split1)

  /** The face containing every vertex in the interval to -> from in the original face. */
  val face2 = new Face(split2)

  /** Both face splits. */
  val faces = Vector(face1, face2)

}
