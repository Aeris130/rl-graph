package net.cyndeline.rlgraph.canonicalOrder.planarBiconnected

/**
  * Contains the data related to the last canonical modification of C(k-1).
  *
  * @param newContour C(k) after C(k-1) has been modified by adding v(k).
  * @param support If v(k) only had a single neighbor on C(k-1), this value is set to Left (Right) if v(k) was inserted
  *                as if it had an edge to a neighbor left (right) of it on C(k-1).
  * @param vertexCover A sequence containing the vertices on C(k-1) that ended up covered by v(k) and no longer is on
  *                    the contour.
  * @param leftNeighbor The leftmost neighbor of v(k) on C(k). May not be connected to v(k) in the original embedding,
  *                     check support to see if this is the case.
  * @param rightNeighbor The rightmost neighbor of v(k) on C(k). May not be connected to v(k) in the original embedding,
  *                      check support to see if this is the case.
  */
case class CKMod[V](newContour: Contour[V],
                    support: Option[LegalSupport],
                    vertexCover: Vector[V],
                    leftNeighbor: V,
                    rightNeighbor: V) {

  /**
    * This method allows for nicer syntax by letting the user chain addVertex calls instead of retrieving the new
    * contour each time.
    * @param v Vertex to add.
    * @return Modification of the contour in this modification.
    */
  def addVertex(v: V): CKMod[V] = newContour.addVertex(v)

}

trait LegalSupport
case object Left extends LegalSupport
case object Right extends LegalSupport
