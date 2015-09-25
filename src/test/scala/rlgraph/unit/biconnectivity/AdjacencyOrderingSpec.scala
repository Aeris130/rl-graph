package rlgraph.unit.biconnectivity

import net.cyndeline.rlgraph.biconnectivity.biconnect.KantBodlaenderBiconnection.help.AdjacencyOrdering
import rlgraph.SpecImports

class AdjacencyOrderingSpec extends SpecImports {

  describe("AdjacencyOrdering") {

    it ("should throw an exception when initialized with a list of size 1 or less") {

      Given("two list of size less than 2")
      val l1 = Vector(1)
      val l2 = Vector[Int]()

      When("initializing orderings")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new AdjacencyOrdering(l1)
      }
      intercept[IllegalArgumentException] {
        new AdjacencyOrdering(l2)
      }

    }

    it ("should thrown an exception if the list is reduced to less than 2 elements") {

      Given("a list of size 3")
      val list = new AdjacencyOrdering(Vector(1, 2))

      When("deleting element 2")
      Then("an exception should be thrown")
      intercept[Error] {
        list.delete(2)
      }

    }

    it ("should return a single pair") {

      Given("a list with 2 elements")
      val list = new AdjacencyOrdering(Vector(1, 2))

      When("retrieving the first pair")
      val pair = list.next()

      Then("the pair should be (1, 2)")
      pair should be ((1, 2))

    }

    it ("should return consecutive pairs") {

      Given("a list with 3 elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("retrieving the first two pairs")
      val pair1 = list.next()
      val pair2 = list.next()

      Then("the pairs should be (1, 2) and (2, 3)")
      pair1 should be ((1, 2))
      pair2 should be ((2, 3))

    }

    it ("should confirm next elements while there are more pairs to retrieve") {

      Given("a list with 4 pairs")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4)) // 1, 2 | 2, 3 | 3, 4 | 4, 1

      When("retrieving pairs")
      Then("hasNext should yield true four times")
      list.hasNext should be (true)
      list.next()
      list.hasNext should be (true)
      list.next()
      list.hasNext should be (true)
      list.next()
      list.hasNext should be (true)
      list.next()
      list.hasNext should be (false)

    }

    it ("should wrap around lists with more than 2 elements") {

      Given("a list with 3 elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("retrieving the first two pairs")
      list.next()
      list.next()

      Then("the third pair should be (3, 1)")
      list.next() should be ((3, 1))

    }

    it ("should not wrap around lists with less than 3 elements") {

      Given("a list with 2 elements")
      val list = new AdjacencyOrdering(Vector(1, 2))

      When("retrieving the first pair")
      list.next()

      Then("no more elements should be present")
      list.hasNext should be (false)

    }

    it ("should not affect pairs when deleting an element that has already been returned as the second entry in a pair") {

      Given("a list with three elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("retrieving the first pair (1, 2) and deleting entry 1")
      list.next()
      list.delete(1)

      Then("the next pair should be (2, 3)")
      list.next() should be ((2, 3))

    }

    it ("should include the previous last element if the first current element is removed") {

      Given("a list with 4 pairs")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4)) // 1, 2 | 2, 3 | 3, 4 | 4, 1

      When("retrieving the pair (1, 2), causing (2, 3) to be next, and then removing 2")
      list.next()
      list.head should be ((2, 3))
      list.delete(2)

      Then("the next element should be 1, 3")
      list.next() should be ((1, 3))

    }

    it ("should include the next element when deleting the second entry of the current pair") {

      Given("a list with 4 pairs")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4)) // 1, 2 | 2, 3 | 3, 4 | 4, 1

      When("retrieving the pair (1, 2), causing (2, 3) to be next, and then removing 3")
      list.next()
      list.head should be ((2, 3))
      list.delete(3)

      Then("the next element should be 2, 4")
      list.next() should be ((2, 4))

    }

    it ("should not wrap around when reducing the size of the ordering to less than 3 while the first pair is active") {

      Given("a list of size 4")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4))

      When("deleting entries 3 and 4 before calling next()")
      list.delete(3)
      list.delete(4)

      Then("the list should have (1, 2) as head entry and no more pairs")
      list.next() should be ((1, 2))
      list.hasNext should be (false)

    }

    it ("should wrap around when deleting the last element, " +
            "and the list had more than 2 entries before deletion," +
            "and the current pair is not the first or last") {

      Given("a list of size 4")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4))

      When("moving to pair 2 (2, 3) and then removing element 4")
      list.next()
      list.delete(4)
      list.next() // Retrieve (2, 3)

      Then("the next pair should be (3, 1)")
      list.next() should be ((3, 1))

      And("no more pairs should be present")
      list.hasNext should be (false)

    }

    it ("should wrap around when deleting the last element, " +
      "and the list had more than 2 entries before deletion," +
      "and the current pair is the last") {

      Given("a list of size 4")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4))

      When("moving to the last pair (3, 4) and then removing element 4")
      list.next() // 1, 2
      list.next() // 2, 3
      list.delete(4) // Currently at 3, 4

      Then("the next pair should be (3, 1)")
      list.next() should be ((3, 1))

      And("no more pairs should be present")
      list.hasNext should be (false)

    }

    it ("should not wrap around if the current pair is the wrap-around and the last element is deleted," +
        "causing the list to reach size 2") {

      Given("a list with three elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("moving to the wrap-around pair 3,1 and deleting element 3")
      list.next() // 1, 2
      list.next() // 2, 3
      list.delete(3)

      Then("No more pairs should be found")
      list.hasNext should be (false)

    }

    it ("should wrap around if the first element in the list is deleted") {

      Given("a list of size 4")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4))

      When("moving to the 4th pair (4, 1) and deleting 1")
      list.next() // 1, 2
      list.next() // 2, 3
      list.next() // 3, 4
      list.head should be ((4, 1))
      list.delete(1)

      Then("the next pair should be (4, 2)")
      list.next() should be ((4, 2))

      And("no more pairs should be found")
      list.hasNext should be (false)

    }

    it ("should only delete the first occurrence of an element") {

      Given("a list with duplicate elements 2")
      val list = new AdjacencyOrdering(Vector(2, 1, 2))

      When("deleting the element 2")
      list.delete(2)

      Then("the first pair should be (1, 2)")
      list.next() should be ((1, 2))

    }

    it ("should produce a list of its vertices") {

      Given("a list with three elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("creating a list from the ordering")
      Then("the list should equal the input")
      list.currentList should equal (Vector(1, 2, 3))

      And("deleting an element should be reflected")
      list.delete(2)
      list.currentList should equal (Vector(1, 3))

    }

    it ("should reset itself") {

      Given("a list of size 4")
      val list = new AdjacencyOrdering(Vector(1, 2, 3, 4))

      When("retrieving the pairs for the first time, there should be 4")
      list.hasNext should be (true)
      list.next() should be ((1, 2))
      list.next() should be ((2, 3))
      list.next() should be ((3, 4))
      list.next() should be ((4, 1))
      list.hasNext should be (false)

      And("resetting the list should give the same result")
      list.reset()
      list.hasNext should be (true)
      list.next() should be ((1, 2))
      list.next() should be ((2, 3))
      list.next() should be ((3, 4))
      list.next() should be ((4, 1))
      list.hasNext should be (false)

    }

    it ("should restore deleted vertices when resetting") {

      Given("a list with three elements")
      val list = new AdjacencyOrdering(Vector(1, 2, 3))

      When("deleting an element and resetting the list")
      list.delete(3)
      list.reset()

      Then("the list should math the original")
      list.currentList should equal (Vector(1, 2, 3))

    }
  }
}
