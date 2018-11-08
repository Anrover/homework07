package fintech.homework07

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {
  def mergeSort[T](data: Seq[T])(implicit ord: Ordering[T]): Seq[T] = {
    import ord._

    def merge(arr: ArrayBuffer[T], left: Int, mid: Int, right: Int) {
      var ind1 = left
      var ind2 = mid
      val result = mutable.Queue[T]()

      for (_ <- left until right) {
        if (ind1 < mid && (ind2 >= right || arr(ind1) < arr(ind2))) {
          result.enqueue(arr(ind1))
          ind1 += 1
        } else {
          result.enqueue(arr(ind2))
          ind2 += 1
        }
      }
      for(i <- left until right)
        arr(i) = result.dequeue()
    }

    val arr = data.to[ArrayBuffer]
    for(i <- Iterator.iterate(1)(_ * 2).takeWhile(_ < arr.length);
        j <- arr.indices by 2 * i)
        merge(arr, j, j + i, math.min(j + 2 * i, arr.length))
    arr
  }

  def quickSort[T](data: Seq[T])(implicit ord: Ordering[T]): Seq[T] = {
    import ord._

    def recQuickSort(arr: ArrayBuffer[T], lowerBound: Int, upperBound: Int): Unit = {
      if (lowerBound < upperBound){
        val part = partition(arr, lowerBound, upperBound)
        recQuickSort(arr, lowerBound, part)
        recQuickSort(arr, part + 1, upperBound)
      }
    }

    def partition(arr: ArrayBuffer[T], lowerBound: Int, upperBound: Int): Int = {
      val pivot = arr(lowerBound)
      var ind1 = lowerBound - 1
      var ind2 = upperBound + 1
      while (ind1 < ind2) {
        do ind1 += 1 while(arr(ind1) < pivot)
        do ind2 -= 1 while(arr(ind2) > pivot)
        if (ind1 < ind2)
          swap(arr, ind1, ind2)
      }
      ind2
    }

    def swap(arr: ArrayBuffer[T], ind1: Int, ind2: Int): Unit = {
      val element = arr(ind1)
      arr(ind1) = arr(ind2)
      arr(ind2) = element
    }

    val array = data.to[ArrayBuffer]
    recQuickSort(array, 0, data.length - 1)
    array
  }
}
