package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("delete") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted_sequence") = forAll { h: H =>
    def ltn(x: A, h: H): Boolean = if (isEmpty(h)) true
    else {
      val y = findMin(h)
      val m = deleteMin(h)
      if (x <= y) ltn(y, m)
      else false
    }

    val a = findMin(h)
    val m = deleteMin(h)
    ltn(a, m)
  }

  property("min_meld") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val m = meld(h1, h2)
    findMin(m) == Math.min(h1Min, h2Min)
  }

  property("same_in_&_out") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    val first = findMin(h)
    val second = findMin(deleteMin(h))

    (first == a && second == b) ||
      (first == b && second == a)
  }

  property("melding_preserves_all_elements") = forAll { (h1: H, h2: H) =>
    def eq(h1: H, h2: H): Boolean = if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val a = findMin(h1)
      val h1m = deleteMin(h1)
      val b = findMin(h2)
      val h2m = deleteMin(h2)

      if (a == b) eq(h1m, h2m)
      else false
    }

    val m = deleteMin(meld(h1, h2))
    val a = findMin(h1)
    val b = findMin(h2)
    val mc = if (a <= b) meld(deleteMin(h1), h2)
    else meld(h1, deleteMin(h2))

    eq(m, mc)
  }


  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}