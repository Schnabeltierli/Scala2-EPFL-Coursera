package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- Gen.oneOf(genHeap, Gen.const(empty)) // oneOf(value(empty), genHeap)
  } yield insert(a, h) // insert(a, h) gives error ??

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min of 1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of 2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == a.min(b)
  }

  property("min of 3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    findMin(h) == a.min(b.min(c))
  }

  // inserting an element into an empty heap, then deleting the minimum, should result in an empty heap
  property("insert then delete") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  // The minimum of a 2-element heap should be the smallest of the two elements
  property("min of 2-element heap") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a1, insert(a2, empty))
    findMin(h) == a1.min(a2)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min of meld") = forAll { (h1: H, h2: H) =>
    val h12 = meld(h1, h2)
    findMin(h12) == findMin(h1) || findMin(h12) == findMin(h2)
  }

  // Given any heap, obtain a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted minima") = forAll { h: H =>
    def reduceH(h1: H, x1: List[Int]): List[Int] = {
      if (isEmpty(h1)) x1
      else findMin(h1) :: reduceH(deleteMin(h1), x1)
    }
    val xs = reduceH(h, Nil)
    xs == xs.sorted

    /** def reduce(h: H): List[A] =
    if (!isEmpty(h)) findMin(h) :: reduce(deleteMin(h))
      else Nil
    reduce(h) == reduce(h).sorted */
  }

  property("gen insert minimum") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("meld 2 heaps minimum") = forAll { (h1: H, h2: H) =>
    val min = findMin(h1) min findMin(h2)
    findMin(meld(h1, h2)) == min
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("Transfer min btw heaps") = forAll { (h1: H, h2: H) =>
    def reduce(h: H): List[A] =
      if (!isEmpty(h)) findMin(h) :: reduce(deleteMin(h))
      else Nil
    val melded = meld(h1, h2)
    val transferred = meld(deleteMin(h1), insert(findMin(h1), h2))
    reduce(melded) == reduce(transferred)
  }


 /** lazy val genMap: Gen[Map[Int,Int]] = for {
   * k <- arbitrary[Int]
   * v <- arbitrary[Int]
   * m <- oneOf(const(Map.empty[Int,Int]), genMap)
   * } yield m.updated(k, v)   */


}
