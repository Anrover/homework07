package fintech.homework07

import scala.collection.mutable

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {
  type MutSeq[T] = mutable.Seq[T]
  
  def mergeSort[T](collection: MutSeq[T])(implicit ord: Ordering[T]): Unit = {
    import ord._

    def merge(left: Int, mid: Int, right: Int) {
      var ind1 = left
      var ind2 = mid
      val result = mutable.Queue[T]()

      for (_ <- left until right) {
        if (ind1 < mid && (ind2 >= right || collection(ind1) < collection(ind2))) {
          result.enqueue(collection(ind1))
          ind1 += 1
        } else {
          result.enqueue(collection(ind2))
          ind2 += 1
        }
      }
      for(i <- left until right)
        collection(i) = result.dequeue()
    }

    def mergeSortRec(left: Int, right: Int): Unit = {
      if (right - left > 1) {
        val mid = (left + right) / 2
        mergeSortRec(left, mid)
        mergeSortRec(mid, right)
        merge(left, mid, right)
      }
    }

    mergeSortRec(0, collection.length)
//    for(i <- Iterator.iterate(1)(_ * 2).takeWhile(_ < collection.length);
//        j <- collection.indices by 2 * i)
//        merge(j, j + i, math.min(j + 2 * i, collection.length))
//    collection
  }

  def quickSort[T](collection: MutSeq[T])(implicit ord: Ordering[T]): Unit = {
    import ord._

    def quickSortRec(lowerBound: Int, upperBound: Int): Unit = {
      if (lowerBound < upperBound){
        val part = partition(lowerBound, upperBound)
        quickSortRec(lowerBound, part)
        quickSortRec(part + 1, upperBound)
      }
    }

    def partition(lowerBound: Int, upperBound: Int): Int = {
      val pivot = collection(lowerBound)
      var ind1 = lowerBound - 1
      var ind2 = upperBound + 1
      while (ind1 < ind2) {
        do ind1 += 1 while(collection(ind1) < pivot)
        do ind2 -= 1 while(collection(ind2) > pivot)
        if (ind1 < ind2)
          swap(ind1, ind2)
      }
      ind2
    }

    def swap(ind1: Int, ind2: Int): Unit = {
      val element = collection(ind1)
      collection(ind1) = collection(ind2)
      collection(ind2) = element
    }

    quickSortRec(0, collection.length - 1)
  }
}
