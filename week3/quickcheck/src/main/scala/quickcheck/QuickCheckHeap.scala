package quickcheck

import java.lang.Math.min

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // Example generator
  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, const(empty)), (9, genHeap))
  } yield insert(n, h)

//  lazy val genHeap: Gen[H] = for {
//    a <- arbitrary[A]
//    h <- oneOf(empty, genMap)
//  } yield insert(a, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Propertie generator
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Example property -> Min
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Properties
  property("empty") = forAll { a: Int =>
    val h = empty
    isEmpty(h) == true
  }

  property("order") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("minimum") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("two elements, findMin gives smaller one") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    findMin(heap) == min(x, y)
  }

  property("delete inserted element") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("meld two heaps and find the minimum") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("Recusivly finding and deleting elements return same elements") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)

        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }

    isSorted(h)
  }

  property("Equal if recursivly removing min elements result in same elements") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
