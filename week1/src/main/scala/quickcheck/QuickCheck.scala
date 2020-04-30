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
    forAll { (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
    }

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
      val new_h = deleteMin(h)

      isEmpty(new_h)
    }

  property("Min of meld should be min1 or min2") = 
    forAll { (h1: H, h2: H) => (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min1 = if (isEmpty(h1)) 0 else findMin(h1)
      val min2 = if (isEmpty(h2)) 0 else findMin(h2)

      val min_both = findMin(meld(h1, h2))

      min_both == min1 || min_both == min2
    }}
    
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

}
