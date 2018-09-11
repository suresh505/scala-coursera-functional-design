package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      i <- arbitrary[Int]
      h <- frequency((1, Gen.const(empty)), (5, genHeap))
    } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Find minimum after adding an element to empty heap") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Adding two elements to an empty heap and finding minimum") = forAll { (i: Int, j: Int) =>
    Math.min(i, j) == findMin(insert(j, insert(i, empty)))
  }

  property("Adding an element to an empty heap and then deleting it") = forAll { (i: Int) =>
    deleteMin(insert(i, empty)) == empty
  }

  property("Finding the minimum after meldling two heaps") = forAll { (h1: H, h2: H) => {
    if(isEmpty(h1) && isEmpty(h2))
      true
    else {
      val min = findMin(meld(h1, h2))
      min == findMin(h1) || min == findMin(h2)
    }
  }}

  property("Creating a list from a heap") = forAll {(h: H) => {
    def buildListFromHeap(h: H, acc: List[Int]) : List[Int] = {
      if (isEmpty(h))
        acc
      else {
        val value = findMin(h)
        val newHeap = deleteMin(h)
        buildListFromHeap(newHeap, value :: acc)
      }
    }

    val list = buildListFromHeap(h, List())
    list == list.sorted.reverse
  }}

  property("Associative") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    toList(a) == toList(b)
  }

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
}
