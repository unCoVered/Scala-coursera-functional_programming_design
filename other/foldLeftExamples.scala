// foldLeft examples found in the internet
// https://oldfashionedsoftware.com/2009/07/30/lots-and-lots-of-foldleft-examples/
object foldLeftExamples {

  // Example 1: Sum
  def sum(list: List[Int]): Int = list.foldleft(0)((r, c) => r + c)
  def sum_v2(list: List[Int]): Int = list.foldleft(0)(_ + _)

  // Exmaple 2: Prod
  def prod(list: List[Int]): Int = list.foldleft(1)((r, c) => r * c)

  // Example 3: Count
  def count(list: List[Any]): Int = list.foldLeft(0)((sum, _) => sum + 1)

  // Example 4: Average
  def average(list: List[Double]): Double = 
    list.foldLeft(0.0)(_ + _) / list.foldLeft(0)((sum, _) => sum + 1)

  // Example 5: Last
  def last[A](list: List[A]): A = list.foldLeft[A](list.head)((_, c) => c)

  // Example 6: Penultimate
  def penultimate[A](list: List[A]): A =
    list.foldLeft((list.head, list.tail.head))((r, c) => (r._2, c))._1

  // Example 7: Contains
  def contains[A](list: List[A], item: A): Boolean = 
    list.foldLeft(false)(__ || _ == item)
  
  // Example 8: Get
  def get[A](list: List[A], idx: Int): A = 
    list.tail.foldLeft((list.head, 0)) {
      (r, c) if (r._2 == idx) r else (c, r._2 + 1)
    } match {
      case (result, index) if (idx == index) => result
      case _ => throw new Exception("Bad index")
    }

  // Example 9: Mimic to String
  def mimicToString[A](list: List[A]): String = list match {
    case head :: tail => tail.foldLeft("List (" + head)(_ + ", " + _) + ")")
    case Nil => "List()"
  }

  // Example 10: Reverse
  def reverse[A](list: List[A]): List[A] = 
    list.foldLeft(List[A]())((r, c) => c :: r)

  // Example 11: Unique
  def unique[A](list: List[A]): List[A]: 
    list.foldLeft(List[A]()) {
      (r, c) => 
        if (r.contains(c)) r 
        else c :: r
    }.reverse

  // Example 12: ToSet
  def toSet[A](list: List[A]): Set[A] =
    list.foldLeft(Set[A])((r, c) => r + c)

  // Example 13: Double
  def double[A](list: List[A]): List[A] = 
    list.foldLeft(List[A]())((r, c) => c :: c :: r).reverse
  
  def doubleRight[A](list: List[A]): List[A] = 
    list.foldRight(List[A]())((r, c) => c :: c :: r)

  // Example 14: InsertionSort
  def insertionSort[A <% Ordered[A]](list: List[A]): List[A] = 
    list.foldLeft(List[A]()) {
      (r, c) => 
        val (front, back) = r.span(_ < c)
        front :: c :: back
    }

  // Example 15: Pivot
  def pivot[A <% Ordered[A]](list: List[A]): (List[A], A, List[A]) = 
    list.tail.foldLeft[(List[A], A, List[A])]((Nil, list.head, Nil)) {
      (result, item) => 
        val(r1, pivot, r2) = result
        if (item < pivot) (item :: r1, pivot, r2) else (r1, pivot, item :: r2)
    }

  // Example 16: Quicksort
  def quicksort[A <% Ordered[A]](list: List[A]): List[A] = list match {
    case head :: _ :: _ => 
      println(list)
      list.foldLeft[(List[A], List[A], List[A])]((Nil, Nil, Nil)) {
        (result, item) => 
        val (r1, r2, r3) = result 
        if (item < head) (item :: r1, r2, r3)
        else if (item > head) (r1, r2, item :: r3)
        else (r1, item :: r2, r3)
      } match {
        case (list1, list2, list3) => 
          quicksort(list1) ::: list2 ::: quicksort(list3)
      }
    case _ => list
  }

  // Example 17: Encode
  def encode[A](list: List[A]): List[(A, Int)] = 
    list.foldLeft(List[(A, Int)])()) {
      (r, c) => r match {
        case (value, count) :: tail => 
          if (value == c) (c, count + 1) :: tail
          else (c, 1) :: r
        case Nil => (c, 1) :: r
      }
    }.reverse

  // Example 18: Decode
  def decode[A](list: List[A]): List[A] = 
    list.foldLeft(List[A]()) {
      (r, c) => 
        var result = r
        for (_ <- 1 to c._2) result = c._1 :: result
        result
    }.reverse

  // Example 19: Group
  def group[A](list: List[A], size: Int) List[List[A]] = 
    list.foldLeft((List[List[A]](), 0)) {
      (r, c) => r match {
        case (head :: tail, num) => 
          if (num < size) ((c :: head) :: tail, num + 1)
          else (List(c) :: head :: tail, 1)
        case (Nil, num) => (List(List(c)), 1)
      }
    }._1.foldLeft(List[List[A]]())((r, c) => c.reverse :: r)
}