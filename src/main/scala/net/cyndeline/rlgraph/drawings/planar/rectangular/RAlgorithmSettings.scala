package net.cyndeline.rlgraph.drawings.planar.rectangular

import net.cyndeline.rlgraph.regularEdgeLabeling.RELFactory
import net.cyndeline.rlgraph.regularEdgeLabeling.factories.MinimalLabelingFactory

import scalax.collection.GraphEdge.UnDiEdge

/**
 * This object specifies helper objects that the use may supply to modify how the rectangular dual is computed.
 */
class RAlgorithmSettings[VType, EType[X] <: UnDiEdge[X]] private (val outerFaceSelect: Option[OuterFaceSelection[VType, EType]],
                                                                  val regularEdgeLabelFactory: RELFactory) {

  def withOuterFaceSelect(fs: OuterFaceSelection[VType, EType]) = new RAlgorithmSettings(Some(fs), regularEdgeLabelFactory)
  def withRELFactory(rf: RELFactory) = new RAlgorithmSettings(outerFaceSelect, rf)

}

/**
 * Creates an initial settings object with default values for the mandatory data.
 */
object RAlgorithmSettings {
  def apply[VType, EType[X] <: UnDiEdge[X]]() = new RAlgorithmSettings[VType, EType](None, new MinimalLabelingFactory())
}
