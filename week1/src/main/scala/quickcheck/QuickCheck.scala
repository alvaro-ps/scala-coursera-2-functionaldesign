package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      h <- genHeap
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Reinserting the minimum and finding it should return fine") = 
    forAll { (h: H) => (!isEmpty(h)) ==> {
      val m = findMin(h)
      findMin(insert(m, h)) == m
    }}

  property("Inserting an element in an empty heap and retrieving it returns fine") = 
    forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
    }

  property("Inserting 2 elements and retrieving the minimum actually gives the minimum") = 
    forAll { (x: Int, y: Int) =>
      val h = insert(y, insert(x, empty))

      val min_value = if (x < y) x else y
      val min_heap = findMin(h)
      min_heap == min_value
    }

  property("Deletemin of a 1 element heap should give empty") = 
    forAll { x: Int =>
      val h = insert(x, empty)
      isEmpty(deleteMin(h))
    }

  property("Min of meld should be min1 or min2") = 
    forAll { (h1: H, h2: H) => 
      val m = meld(h1, h2)

      val min1 = if (!isEmpty(h1)) findMin(h1) else 0
      val min2 = if (!isEmpty(h2)) findMin(h2) else 0

      val min_both = if (!isEmpty(m)) findMin(m) else 0

      min_both == min1 || min_both == min2
    }

  property("Recursively getting and deleting obtains a sorted sequence") = 
    forAll { (h: H) => (!isEmpty(h)) ==> {

      def extractAll(heap: H, all_values: List[A]): List[A] =
        if (isEmpty(heap)) all_values
        else {
          val min_val = findMin(heap)
          extractAll(deleteMin(heap), all_values ++ List(min_val))
        }

      val values = extractAll(h, Nil)
      values == values.sorted
    }}

  property("Min should be the same regardless of melding order") =
    forAll { (h1: H, h2: H) =>
      val meld1 = meld(h1, h2)
      val meld2 = meld(h2, h1)

      val min1 = if (!isEmpty(meld1)) findMin(meld1) else 0
      val min2 = if (!isEmpty(meld2)) findMin(meld2) else 0

      min1 == min2
    }

 property("Melding two empty heaps should return empty") = meld(empty, empty) == empty

 property("Melding any heap with an empty one should return the original") =
    forAll { (h: H) => meld(h, empty) == h }
}
