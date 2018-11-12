package fintech.homework07
import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable.ArrayBuffer

class SortingSpec extends FlatSpec with Matchers {
  case class Person(name: String, birthDate: LocalDate)

  "QuickSort" should "correct sort mutable sequences with Int" in {
    val arrayInteger = ArrayBuffer(1, 7, 5, 5, 12, 2, 10)
    val emptyArray = ArrayBuffer.empty[Int]

    Sorting.quickSort(arrayInteger)
    arrayInteger should be (arrayInteger.sorted)

    Sorting.quickSort(emptyArray)
    emptyArray should be (ArrayBuffer.empty)
  }
  it should "correct sort mutable sequences with String or Char" in {
    val arrayString = ArrayBuffer("def", "abb", "abcd", "a", "ab")
    val arrayChar = ArrayBuffer('a', 'f', 'm', 'b', 'c', 'e', 'f')

    Sorting.quickSort(arrayChar)
    arrayChar should be (arrayChar.sorted)
    Sorting.quickSort(arrayString)
    arrayString should be (arrayString.sorted)
  }
  it should "correct sort mutable sequences with Person" in {
    implicit val peopleOrdering: Ordering[Person] = Ordering.by(_.name)
    val people = ArrayBuffer(
      Person("Bob", LocalDate.of(1981, 5, 12)),
      Person("Charlie", LocalDate.of(1982, 5, 11)),
      Person("Alice", LocalDate.of(1970, 1, 1)),
      Person("Michael", LocalDate.of(1978, 5, 12))
    )

    Sorting.quickSort(people)
    people should be (people.sorted)
  }
  it should "correct sort mutable BIG sequences with Int" in {
    val bigArrayInteger: ArrayBuffer[Int] = ArrayBuffer.fill(1000000)(util.Random.nextInt)
    Sorting.quickSort(bigArrayInteger)
    bigArrayInteger should be (bigArrayInteger.sorted)
  }

  "MergeSort" should "correct sort mutable sequences with Int" in {
    val arrayInteger = ArrayBuffer(1, 7, 5, 5, 12, 2, 10)
    Sorting.mergeSort(arrayInteger)
    arrayInteger should be (arrayInteger.sorted)
  }
  it should "correct sort mutable sequences with String or Char" in {
    val arrayString = ArrayBuffer("def", "abb", "abcd", "a", "ab")
    val arrayChar = ArrayBuffer('a', 'f', 'm', 'b', 'c', 'e', 'f')

    Sorting.mergeSort(arrayChar)
    arrayChar should be (arrayChar.sorted)
    Sorting.mergeSort(arrayString)
    arrayString should be (arrayString.sorted)
  }
  it should "correct sort mutable sequences with Person" in {
    implicit val peopleOrdering: Ordering[Person] = Ordering.by(_.name)
    val people = ArrayBuffer(
      Person("Bob", LocalDate.of(1981, 5, 12)),
      Person("Charlie", LocalDate.of(1982, 5, 11)),
      Person("Alice", LocalDate.of(1970, 1, 1)),
      Person("Michael", LocalDate.of(1978, 5, 12))
    )
    Sorting.mergeSort(people)
    people should be (people.sorted)
  }
  it should "correct sort mutable BIG sequences with Int" in {
    val bigArrayInteger: ArrayBuffer[Int] = ArrayBuffer.fill(1000000)(util.Random.nextInt)
    Sorting.mergeSort(bigArrayInteger)
    bigArrayInteger should be (bigArrayInteger.sorted)
  }
}
