package org.workingonbits.s99

import scala.util.Random

/**
 *
 */
object WorkingWithLists {

  /**
   * P01 (*) Find the last element of a list.
   * Example:
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   */
  def last[A](ls: List[A]): A = ls match {
    case Nil => error("empty list")
    case h :: Nil => h
    case _ :: tail => last(tail)
  }

  /**
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](ls: List[A]): A = ls match {
    case Nil => error("empty list")
    case p :: _ :: Nil => p
    case _ :: tail => penultimate(tail)
  }

  /**
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   *
   */
  def nth[A](n: Int, ls: List[A]): A = ls match {
    case Nil => error("empty list")
    case h :: tail => if (n == 0) h else nth(n - 1, tail)
  }

  /**
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[A](ls: List[A]): Int = {
    def lengthRecursive(n: Int, list: List[A]): Int = list match {
      case Nil => n
      case _ :: tail => lengthRecursive(n + 1, tail)
    }

    lengthRecursive(0, ls);
  }

  /**
   * P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)
  }

  /**
   * P06 (*) Find out whether a list is a palindrome.
   * Example:
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  /**
   * P07 (**) Flatten a nested list structure.
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   * Example:
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[A](ls: List[A]): List[A] = ls match {
    case head :: Nil => head :: Nil
    case head :: tail => if (head == tail.head) compress(tail) else head :: compress(tail)
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * Example:
   * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  /**
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   * Example:
   * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[A](ls: List[A]): List[(Int, A)] = pack(ls) map { l => (l.size, l.head) }

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
   * Example:
   * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeModified[A](ls: List[A]): List[Any] = {
    encode(ls) map { l => if (l._1 == 1) l._2 else l }
  }

  /**
   * P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
   * Example:
   * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode[A](ls: List[(Int, A)]): List[A] = {
    ls flatMap { l => List.fill(l._1)(l._2) }
  }

  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   * Example:
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.size, packed.head) :: encodeDirect(next)
    }
  }

  /**
   * P14 (*) Duplicate the elements of a list.
   * Example:
   * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List.fill(2)(e) }

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   * Example:
   * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap { e => List.fill(n)(e) }

  /**
   * P16 (**) Drop every Nth element from a list.
   * Example:
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[A](n: Int, ls: List[A]): List[A] = {
    val (left, right) = ls.splitAt(n - 1)
    if (right.isEmpty) left
    else left ::: drop(n, right.tail)
  }

  /**
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * Example:
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

  /**
   * P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
   * Example:
   * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   */
  def slice[A](from: Int, to: Int, ls: List[A]): List[A] = ls.take(to).drop(from)

  /**
   * P19 (**) Rotate a list N places to the left.
   * Examples:
   * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   *
   * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   */
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    if (n == 0) ls
    else if (n < 0) rotate(n + 1, ls.last :: ls.take(ls.size - 1))
    else rotate(n - 1, ls.tail ::: List(ls.head))
  }

  /**
   * P20 (*) Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   * Example:
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    val (left, right) = ls splitAt n
    (left ::: right.tail, right.head)
  }

  /**
   * P21 (*) Insert an element at a given position into a list.
   * Example:
   * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
    val (left, right) = ls splitAt n
    left ::: e :: right
  }

  /**
   * P22 (*) Create a list containing all integers within a given range.
   * Example:
   * scala> range(4, 9)
   * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range(from: Int, end: Int): List[Int] = from to end toList

  /**
   * P23 (**) Extract a given number of randomly selected elements from a list.
   * Example:
   * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
   * res0: List[Symbol] = List('e, 'd, 'a)
   * Hint: Use the solution to problem P20
   */
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    if (n == 0) Nil
    else {
      val (rest, e) = removeAt(Random.nextInt(ls.size), ls)
      e :: randomSelect(n - 1, rest)
    }
  }

  /**
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   * Example:
   * scala> lotto(6, 49)
   * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
   */
  def lotto(n: Int, m: Int): List[Int] = {
    if (n == 0) Nil
    else Random.nextInt(m) :: lotto(n - 1, m)
  }

  /**
   * P25 (*) Generate a random permutation of the elements of a list.
   * Hint: Use the solution of problem P23.
   * Example:
   * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */
  def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

  /**
   * P26 (**) Generate the combinations of K distinct objects 
   * chosen from the N elements of a list.
   * In how many ways can a committee of 3 be chosen from a group 
   * of 12 people? We all know that there are C(12,3) = 220 possibilities 
   * (C(N,K) denotes the well-known binomial coefficient). 
   * For pure mathematicians, this result may be great. But we want 
   * to really generate all the possibilities.
   * Example:
   * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
   * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
   */
  def combinations[A](n: Int, ls: List[A]): List[List[A]] = ls.combinations(n).toList

  def main(args: Array[String]) {
    val list = combinations(3, List('a, 'b, 'c, 'd))
    println(list)
  }
}