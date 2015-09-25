package net.cyndeline.rlgraph.subgraph.isomorphism.jGraphT

import net.cyndeline.rlgraph.subgraph.isomorphism.{ElementEquivalence, NegativeCondition}
import net.cyndeline.rlgraph.util.graphConverters.jGraphT.ScalaGraphToJGraphTConverter
import org.jgrapht.experimental.equivalence.EquivalenceComparator
import org.jgrapht.experimental.isomorphism.{AdaptiveIsomorphismInspectorFactory, IsomorphismRelation}
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.{Graph => JGraphT}

import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * A scala-graph wrapper for isomorphism inspection of planar undirected graphs. Checks if two graphs share the
 * exact same topology.
 *
 * @constructor Creates a new isomorphism inspector.
 */
class IsomorphismInspector {
  private val converter = new ScalaGraphToJGraphTConverter()

  /**
   * Compares if a graph has identical structure compared to another graph using the JGraphT library by first converting
   * both graphs to JGraphT.
   *
   * @param g1 Graph to check isomorphism for.
   * @param g2 Graph that 'g1s isomorphism is checked against.
   * @param comparator Compares two vertices to see if they're equal.
   * @param negativeCondition Invalidates an isomorphic solution if its mappings fail a condition.
   * @param fullG2Graph If g2 is a sub graph, this is the graph it is based on. Otherwise g2.
   * @return True if g1 is isomorphic in regards to g2.
   */
  def isIsomorphic[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                   g1: Graph[VType, EType],
                   g2: Graph[VType, EType],
                   comparator: ElementEquivalence[VType, EType],
                   negativeCondition: Option[NegativeCondition[VType, EType]],
                   fullG2Graph: Graph[VType, EType]): Boolean = {
    val jGraphT1 = converter.convert(g1)
    isIsomorphic[VType, EType](jGraphT1, g2, g1, comparator, negativeCondition, fullG2Graph)
  }

  /**
   * Checks isomorphism with a pre-converted JGraphT graph. This is in case a specific graph pattern is to be checked
   * against multiple graphs, and converting it multiple times is unnecessary.
   * @param g1 Graph to check isomorphism for.
   * @param g2 Graph that 'g1s isomorphism is checked against.
   * @param g1Context The graph that g1 was derived from.
   * @param comparator Compares two vertices to see if they're equal.
   * @param negativeCondition Invalidates an isomorphic solution if its mappings fail a condition.
   * @param fullG2Graph If g2 is a sub graph, this is the graph it is based on. Otherwise g2.
   * @return True if g1 is isomorphic in regards to g2.
   */
  def isIsomorphic[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                   g1: JGraphT[VType, DefaultEdge],
                   g2: Graph[VType, EType],
                   g1Context: Graph[VType, EType],
                   comparator: ElementEquivalence[VType, EType],
                   negativeCondition: Option[NegativeCondition[VType, EType]],
                   fullG2Graph: Graph[VType, EType]): Boolean = {
    val jGraphT2 = converter.convert(g2)
    createMapping[VType, EType](g1, jGraphT2, g1Context, g2, comparator, negativeCondition, fullG2Graph).isDefined
  }

  /**
   * Returns an isomorphic mapping between vertices of a pre-converted JGraphT graph and a scala graph.
   * @param g1 Graph to check isomorphism for.
   * @param g2 Graph that 'g1s isomorphism is checked against.
   * @param comparator Compares two vertices to see if they're equal.
   * @param negativeCondition Invalidates an isomorphic solution if its mappings fail a condition.
   * @param fullG2Graph If g2 is a sub graph, this is the graph it is based on. Otherwise g2.
   * @return a mapping of nodes from g1 to g2 if graphs are isomorphic, otherwise None.
   */
  def isomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                        g1: Graph[VType, EType],
                        g2: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        negativeCondition: Option[NegativeCondition[VType, EType]],
                        fullG2Graph: Graph[VType, EType]): Option[Map[VType, VType]] = {
    val jGraphT1 = converter.convert(g1)
    isomorphicMapping[VType, EType](jGraphT1, g2, g1, comparator, negativeCondition, fullG2Graph)
  }

  /**
   * Returns an isomorphic mapping between vertices of a pre-converted JGraphT graph and a scala graph.
   * @param g1 Graph to check isomorphism for.
   * @param g2 Graph that 'g1s isomorphism is checked against.
   * @param g1Context The graph that g1 was derived from.
   * @param comparator Compares two vertices to see if they're equal.
   * @param negativeCondition Invalidates an isomorphic solution if its mappings fail a condition.
   * @param fullG2Graph If g2 is a sub graph, this is the graph it is based on. Otherwise g2.
   * @return a mapping of nodes from g1 to g2 if graphs are isomorphic, otherwise None.
   */
  def isomorphicMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                        g1: JGraphT[VType, DefaultEdge],
                        g2: Graph[VType, EType],
                        g1Context: Graph[VType, EType],
                        comparator: ElementEquivalence[VType, EType],
                        negativeCondition: Option[NegativeCondition[VType, EType]],
                        fullG2Graph: Graph[VType, EType]): Option[Map[VType, VType]] = {
    val jGraphT2 = converter.convert(g2)
    createMapping(g1, jGraphT2, g1Context, g2, comparator, negativeCondition, fullG2Graph)
  }


  /**
   * Computes an isomorphic relationship between two JGraphT graphs and returns the mapping between vertices.
   *
   * @param g1 Graph to check isomorphism for.
   * @param g2 Graph that 'g1s isomorphism is checked against.
   * @param g1Context The graph that g1 was derived from.
   * @param g2Context The graph that g2 was derived from.
   * @param comparator Compares two vertices to see if they're equal.
   * @param negativeCondition Invalidates an isomorphic solution if its mappings fail a condition.
   * @param fullG2Graph If g2 is a sub graph, this is the graph it is based on. Otherwise g2.
   * @return a mapping of nodes from g1 to g2 if graphs are isomorphic, otherwise None.
   */
  private def createMapping[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                                g1: JGraphT[VType, DefaultEdge],
                                g2: JGraphT[VType, DefaultEdge],
                                g1Context: Graph[VType, EType],
                                g2Context: Graph[VType, EType],
                                comparator: ElementEquivalence[VType, EType],
                                negativeCondition: Option[NegativeCondition[VType, EType]],
                                fullG2Graph: Graph[VType, EType]): Option[Map[VType, VType]] = {
    val vComp = new VertexComparator(comparator, g1Context, g2Context)
    val isomorphicInspector = AdaptiveIsomorphismInspectorFactory.createIsomorphismInspectorByType(AdaptiveIsomorphismInspectorFactory.GRAPH_TYPE_PLANAR, g1, g2, vComp, null)
    if (isomorphicInspector.isIsomorphic) {

      /* Go over every mapping until one is found that satisfies the negative condition, of any. */
      while(isomorphicInspector.hasNext) {
        val mapping = isomorphicInspector.next().asInstanceOf[IsomorphismRelation[VType, DefaultEdge]]
        val vertices = g1.vertexSet().iterator()
        var result = Map[VType, VType]()
        while (vertices.hasNext) {
          val v = vertices.next()
          val mappedValueInG2: VType = mapping.getVertexCorrespondence(v, true)
          result += (v -> mappedValueInG2)
        }

        if (negativeCondition.isEmpty || (negativeCondition.isDefined && negativeCondition.get.isValid(result, fullG2Graph)))
          return Some(result)

      }

      None // No mappings fulfilled the negative condition
    } else {
      None
    }
  }

  /**
   * Comparator needed by JGraphT's library.
   */
  private class VertexComparator[VType : TypeTag, EType[X] <: UnDiEdge[X]](
                                 vertexEquivalence: ElementEquivalence[VType, EType],
                                 g1: Graph[VType, EType],
                                 g2: Graph[VType, EType]) extends EquivalenceComparator[VType, JGraphT[VType, DefaultEdge]] {
    def equivalenceCompare(p1: VType,
                           p2: VType,
                           p3: JGraphT[VType, DefaultEdge],
                           p4: JGraphT[VType, DefaultEdge]): Boolean = {
      vertexEquivalence.compares(p1, p2, g1, g2)
    }

    def equivalenceHashcode(element: VType, graph: JGraphT[VType, DefaultEdge]): Int = vertexEquivalence.elementHash(element)
  }

}
