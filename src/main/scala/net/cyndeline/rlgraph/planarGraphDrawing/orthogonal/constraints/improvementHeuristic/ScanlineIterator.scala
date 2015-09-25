package net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic

import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.help.Segment
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.constraints.improvementHeuristic.CornerType._

import scala.collection.immutable.TreeSet

/**
 * Simulates a scan line moving contrary to a set of aligned segments, reacting when it enters a segment from the
 * top, or leaves one from the bottom.
 *
 * @constructor Creates a new scan line iterator.
 * @param segments All segment corners being iterated over. Must all refer to segments of a single orientation.
 * @tparam VType Vertex type in the segment corners being iterated over.
 */
class ScanlineIterator[VType](segments: Set[SegmentCorner[VType]]) extends Iterator[(Int, Set[SegmentCorner[VType]])] {
  checkOrientation()
  private val iterator = groupIdenticalSecondaryCoordinates

  /**
   * Creates a new scan line iterator from a sequence of segment corners.
   * @param segments All segment corners to iterate over.
   * @return A new scanline iterator.
   */
  def this(segments: SegmentCorner[VType]*) = this(segments.toSet)

  def hasNext: Boolean = iterator.hasNext

  def next(): (Int, Set[SegmentCorner[VType]]) = iterator.next()

  /**
   * Groups every segment corner with the same secondary coordinate into a set, and adds it to the iterator paired
   * with the coordinate.
   */
  private def groupIdenticalSecondaryCoordinates: Iterator[(Int, Set[SegmentCorner[VType]])] = {
    val order = Ordering.fromLessThan[(Int, Set[SegmentCorner[VType]])](_._1 > _._1)
    var orderedSegments = TreeSet.empty(order)
    var remainingSegments = segments

    var processedBottomCorners = Set[Segment[VType]]()
    var processedTopCorners = Set[Segment[VType]]()

    while (!remainingSegments.isEmpty) {
      val segment = remainingSegments.head
      val coordinate = segment.secondaryCoordinate

      // Get every segment with the same secondary coordinate, top or bottom doesn't matter.
      val segmentsWithCoordinate: Set[SegmentCorner[VType]] = remainingSegments.filter(_.secondaryCoordinate == coordinate)
      remainingSegments = remainingSegments -- segmentsWithCoordinate
      orderedSegments += ((coordinate, segmentsWithCoordinate))

      /* For every added segment corner, check if its opposite has been processed. If so, remove it from the set.
       * If not, add this segment corner.
       */
      val addedSegments: Iterator[SegmentCorner[VType]] = segmentsWithCoordinate.iterator
      while (addedSegments.hasNext) {
        val s = addedSegments.next()
        if (s.cornerType == Top) {
          if (processedBottomCorners.contains(s.segment)) {
            processedBottomCorners -= s.segment
          } else {
            processedTopCorners += s.segment
          }

        } else {
          if (processedTopCorners.contains(s.segment)) {
            processedTopCorners -= s.segment
          } else {
            processedBottomCorners += s.segment
          }
        }
      }
    }

    if (!processedTopCorners.isEmpty)
      throw new IllegalArgumentException("The following segment top corners did not have a matching opposite: " + processedTopCorners)

    if (!processedBottomCorners.isEmpty)
      throw new IllegalArgumentException("The following segment bottom corners did not have a matching opposite: " + processedBottomCorners)

    orderedSegments.iterator
  }

  /**
   * Throws an exception if every segment doesn't share the same orientation.
   */
  private def checkOrientation() {
    if (segments.isEmpty)
      return

    val orientation = segments.head.segment.orientation
    val seg = segments.iterator
    while (seg.hasNext) {
      val s = seg.next()
      if (s.segment.orientation != orientation)
        throw new IllegalArgumentException("Segments " + seg + " and " + s.segment + " has different orientations.")
    }
  }
}
