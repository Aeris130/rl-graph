package net.cyndeline.rlgraph.cartogram.rectangular.common

/**
 * Defines the optimal area a rectangle should have in a rectangular cartogram.
 */
trait MapArea {

  /**
   * @return A positive value of 2 or more (stop coordinate included).
   */
  def targetArea: Int
}
