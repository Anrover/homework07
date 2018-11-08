package fintech.homework07
import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}

class SortingSpec extends FlatSpec with Matchers {
  case class Person(name: String, birthDate: LocalDate)

  val people = List(
    Person("Bob", LocalDate.of(1981, 5, 12)),
    Person("Charlie", LocalDate.of(1982, 5, 11)),
    Person("Alice", LocalDate.of(1970, 1, 1)),
    Person("Michael", LocalDate.of(1978, 5, 12))
  )
  val listInteger = List(1, 7, 5, 5, 12, 2, 10)
  val listString = List("def", "abb", "abcd", "a", "ab")
  val listChar = List('a', 'f', 'm', 'b', 'c', 'e', 'f')
  val seqInteger = Seq(1, 7, 5, 5, 12, 2, 10)
  val sortedSeq = Seq(1, 2, 3)

  "QuickSort" should "correct sort sequence with Int" in {
    Sorting.quickSort(listInteger) should be (listInteger.sorted)
    Sorting.mergeSort(List.empty) should be (List.empty)
    Sorting.quickSort(seqInteger) should be (seqInteger.sorted)
    Sorting.quickSort(sortedSeq) should be (sortedSeq.sorted)
  }
  it should "correct sort sequence with String or Char" in {
    Sorting.quickSort(listChar) should be (listChar.sorted)
    Sorting.quickSort(listString) should be (listString.sorted)
  }
  it should "correct sort sequence with Person" in {
    implicit val peopleOrdering: Ordering[Person] = Ordering.by(_.name)
    Sorting.quickSort(people) should be (people.sorted)
  }

  "MergeSort" should "correct sort sequence with Int" in {
    Sorting.mergeSort(listInteger) should be (listInteger.sorted)
    Sorting.mergeSort(seqInteger) should be (seqInteger.sorted)
    Sorting.mergeSort(sortedSeq) should be (sortedSeq.sorted)
  }
  it should "correct sort sequence with String or Char" in {
    Sorting.mergeSort(listChar) should be (listChar.sorted)
    Sorting.mergeSort(listString) should be (listString.sorted)
  }
  it should "correct sort sequence with Person" in {
    implicit val peopleOrdering: Ordering[Person] = Ordering.by(_.birthDate.getYear)
    Sorting.mergeSort(people) should be (people.sorted)
  }
}
