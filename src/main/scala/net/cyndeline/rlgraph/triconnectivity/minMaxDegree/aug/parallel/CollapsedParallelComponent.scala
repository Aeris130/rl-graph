package net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.parallel

import net.cyndeline.rlgraph.triconnectivity.minMaxDegree.aug.{Augmentation, Component}

/**
 * Represents the various ways components can be collapsed when ordering them in the parallel case:
 *  - Collapsing a series of (1,1) components into a single (1,1)
 *  - Collapsing a series of (2,2) components into a single (2,2)
 *  - Collapsing two (1,0) components into a single (1,1) with a gap
 *  - Collapsing two (2,0) components into a single (2,2) with a gap
 *  - Collapsing a (1,0) and a (2,0) component into a single (2,1)
 *  - Collapsing a series of (2,1) with odd size into a single (2,1)
 *  - Collapsing a series of (2,1) with even size into a single (1,1)
 *  - Collapsing a series of (2,1) with even size into a single (2,2)
 */
sealed trait CollapsedParallelComponent[VType] {
  def augmentation: Augmentation
  def components: Vector[Component[VType]]
}

case class K1Collapse[VType](components: Vector[Component[VType]]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(1, 1)
}
case class K2Collapse[VType](components: Vector[Component[VType]]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 2)
}
case class K4GapCollapse[VType](a: Component[VType], b: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(1, 1)
  def components = Vector(a, b)
}
case class K5GapCollapse[VType](a: Component[VType], b: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 2)
  def components = Vector(a, b)
}
case class K4K5IntoK3WithGapCollapse[VType](k4: Component[VType], k5: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 1)
  def components = Vector(k4, k5)
}
case class K3IntoK3Collapse[VType](components: Vector[Component[VType]]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 1)
}
case class K3IntoK1Collapse[VType](components: Vector[Component[VType]]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(1, 1)
}
case class K3IntoK2Collapse[VType](components: Vector[Component[VType]]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 2)
}

/* Singles used to store components before being processed. */

case class K1Single[VType](component: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(1, 1)
  def components = Vector(component)
}
case class K2Single[VType](component: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 2)
  def components = Vector(component)
}
case class K3Single[VType](component: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 1)
  def components = Vector(component)
}
case class K4Single[VType](component: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(1, 0)
  def components = Vector(component)
}
case class K5Single[VType](component: Component[VType]) extends CollapsedParallelComponent[VType] {
  def augmentation = Augmentation(2, 0)
  def components = Vector(component)
}