package rlgraph.unit.util.graphConverters

import java.util

import de.fhstralsund.vinets.structure.{Link, Node}
import net.cyndeline.rlgraph.util.graphConverters.vinets.VinetsConverter
import rlgraph.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import collection.JavaConverters._

class VinetsConverterSpec extends SpecImports {
  private val converter = new VinetsConverter[Int, UnDiEdge]()

  describe("VinetsConverter") {

    it ("should convert a scala graph to vinets") {

      Given("a scala graph")
      val g = Graph(1~2, 2~3)

      When("converting the graph to vinets")
      val converterData = converter.convertTo(g)
      val vg = converterData.convertedGraph
      val v1: Node = converterData.vertexMap(1)
      val v2 = converterData.vertexMap(2)
      val v3 = converterData.vertexMap(3)
      val e1: Link = converterData.edgeMap(1~2)
      val e2 = converterData.edgeMap(2~3)

      Then("two edges between the converted vertices 1-2 and 2-3 should exist")
      val nodes1 = asNodeVector(e1.incidentNodes()).toSet
      nodes1 should be (Set(v1, v2))

      val nodes2 = asNodeVector(e2.incidentNodes()).toSet
      nodes2 should be (Set(v2, v3))

    }

    it ("should convert a graph from vinets") {

      Given("a scala graph")
      val g = Graph(1~2, 2~3)

      When("converting it to and from vinets")
      val thereAndBack = converter.convertFrom(converter.convertTo(g))

      Then("the result should be the original graph")
      thereAndBack should be (g)

    }

  }

  private def asNodeVector(it: util.Iterator[_]): Vector[Node] = it.asScala.toVector.map(v => v.asInstanceOf[Node])
  private def asLinkVector(it: util.Iterator[_]): Vector[Link] = it.asScala.toVector.map(l => l.asInstanceOf[Link])

}
