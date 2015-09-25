package rlgraph.unit.spqr.jbpt

import net.cyndeline.rlgraph.spqr.SPQRNode
import net.cyndeline.rlgraph.spqr.jbpt.{HashNode, JBPTTree}
import rlgraph.SpecImports

class JBPTTreeSpec extends SpecImports {

  private def rootWithChildren = new {
    val root = HashNode[Int](0, null)
    val n1 = HashNode[Int](1, root, (1,2), null)
    val n2 = HashNode[Int](2, root, (3,4), null)
    val tree = new JBPTTree(root)
    tree.addChild(n1)
    tree.addChild(n2)
  }

  describe("JBPTTree") {

    it ("should add children to a currently present node") {

      Given("a tree with a root")
      val root = HashNode[Int](0, null)
      val tree = new JBPTTree(root)
      val initialSize = tree.size

      When("adding a child to the root")
      val child = HashNode[Int](1, root, (1,2), null)
      tree.addChild(child)

      Then("the root should contain the child")
      root.children should contain (child.asInstanceOf[SPQRNode[Int]])

      And("the size should increase by 1")
      tree.size should be (initialSize  + 1)

    }

    it ("should delete nodes and all underlying successors") {

      Given("a graph with a chain of nodes root -> n1 -> n2")
      val root = HashNode[Int](0, null)
      val n1 = HashNode[Int](1, root, (1,2), null)
      val n2 = HashNode[Int](2, n1, (3,4), null)
      val tree = new JBPTTree(root)
      tree.addChild(n1)
      tree.addChild(n2)

      When("deleting n1")
      tree.deleteNode(n1)

      Then("the root should have no children")
      root.children should be ('empty)

      And("the tree should have size 1")
      tree.nodes.size should be (1)
      assert(tree.nodes.head eq root, "The current node in the tree is " + tree.nodes.head + ", not " + root + ".")

    }

    it ("should return leaves") {

      Given("a root with two children")
      val f = rootWithChildren
      import f._

      When("retrieving the leaves of the tree")
      val leaves = tree.leaves

      Then("Both children should be in the list")
      leaves should have size 2
      leaves.toSet should equal (Set(n1, n2))

    }
  }
}
