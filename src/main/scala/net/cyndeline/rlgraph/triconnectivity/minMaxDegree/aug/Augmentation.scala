package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug

/**
 * Given a graph G, an SPQR node b and a pertinent pertinent graph B, this object represents the number
 * of edges (x and y) that must be connected between B and (G - B) in the faces F1 and F2 in parent(b),
 * in order to triconnect the B. F1 and F2 are the faces sharing the edge e(B) in parent(b), connected
 * between the cutpair that connects parent(b) to b.
 *
 * x >= 0, y <= 2, x + y > 0.
 *
 * @param x The number of edges that must connect from B to (G - B) in F1.
 * @param y The number of edges that must connect from B to (G - B) in F2.
 */
case class Augmentation(x: Int, y: Int) {
  if ((x + y) < 1)
    throw new Error("Augmentation x (" + x + ") + y (" + y + ") must be > 0.")
  else if (y > 2)
    throw new Error("y augmentation cannot be higher than 2 (currently " + y + ").")
  else if (x < 0)
    throw new Error("The x augmentation cannot be negative (currently " + x + ").")
  else if (y < 0)
    throw new Error("The y augmentation cannot be negative (currently " + y + ").")
  else if (x == 0) {
    throw new IllegalArgumentException("An augmentation (n, 0) should have the 0 set as y, not x.")
  }

}
