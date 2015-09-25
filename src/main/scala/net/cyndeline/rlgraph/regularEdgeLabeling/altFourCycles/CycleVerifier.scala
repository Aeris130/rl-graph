package net.cyndeline.rlgraph.regularEdgeLabeling.altFourCycles

import net.cyndeline.rlgraph.cycles.Cycle
import net.cyndeline.rlgraph.regularEdgeLabeling.angularMap.{AngularMap, AngularVertex}

/**
 * Compares a cycle of vertices to an angular map to see if it constitutes an alternating essential 4-cycle.
 */
class CycleVerifier {

  /**
   * @param c A cycle of length 4 corresponding to a face in an angular map.
   * @param map The angular map of the cycle.
   * @return True if the cycle marks an essential alternating 4-cycle: This is only the case when the cycle corresponds
   *         to a circuit in the angular maps alpha4-orientation. Thus, make sure to test a cycle AND its reverse to
   *         see which one of them (if any) yields true.
   */
  def verifyFaceCycle[V](c: Cycle[AngularVertex[V]], map: AngularMap[V]): Boolean = {
    require(c.length == 4, "Faces in an angular map should all have size 4.")

    // If the cycle isn't a circuit, return false
    for (e <- c.edges) {
      if (map.orientationOfEdge(e._1, e._2) != e._2)
        return false
    }

    true
  }

}
