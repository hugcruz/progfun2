package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] =
    for{
      number <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(number, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min with 2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == scala.math.min(a, b)
  }

  property("meld1") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    findMin(meld(h1, h2)) == scala.math.min(a, b)
  }

  property("deleteMin1") = forAll { (a: Int, b: Int, c: Int, d: Int) =>
    val h1Min = scala.math.max(a, b)
    val h1 = insert(d, insert(c, deleteMin( insert(a, insert(b, empty) ) ) ) )
    findMin(h1) == scala.math.min(scala.math.min(h1Min, c), d)
  }

  property("deleteToEmpty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  def checkNext(h1: H, min: Int): Boolean = {
    if(isEmpty(h1)) true
    else if(findMin(h1) < min) false
    else checkNext(deleteMin(h1), findMin(h1))
  }

  property("getDeletes") = forAll { (h: H) =>
    checkNext(h, findMin(h))
  }

  property("getSorted") = { 
    val h1 = insert(1, insert(2,empty))
    val h2 = insert(1, insert(3,empty))

    val m = meld(h1, h2)

    findMin(deleteMin(m)) == 1
    isEmpty(deleteMin(deleteMin(m))) || findMin(deleteMin(deleteMin(m))) == 2
  }

}
