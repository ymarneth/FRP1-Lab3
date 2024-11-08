package stream

import reduce._
import reduce.Monoid

sealed trait Stream[+A] {
  val isEmpty: Boolean

  // TODO: Task 9.2: Lazy methods in trait Stream
  def map[B](mapper: A => B): Stream[B] =
    this match
      case Empty => Empty
      case Cons(headFn, tailFn) => Stream(mapper(headFn()), tailFn().map(mapper))

  def take(n: Int): Stream[A] = this match
    case Empty => Empty
    case Cons(headFn, tailFn) =>
      if (n == 0) Empty
      else Stream(headFn(), tailFn().take(n - 1))


  def filter(pred: A => Boolean): Stream[A] = ???

  // TODO: Task 9.3: Methods in trait Stream returning results
  def head: A = ???

  def tail: Stream[A] = ???

  def headOption: Option[A] =
    this match
      case Empty => None
      case Cons(headFn, _) => Some(headFn()) // force the evaluation of the head

  def tailOption: Option[Stream[A]] =
    this match
      case Empty => None
      case Cons(_, tailFn) => Some(tailFn()) // force the evaluation of the tail

  def forEach(action: A => Unit): Unit =
    this match
      case Empty => ()
      case Cons(headFn, tailFn) =>
        action(headFn())
        tailFn().forEach(action)

  def toList: List[A] =
    this match
      case Empty => Nil
      case Cons(headFn, tailFn) => headFn() :: tailFn().toList

  def reduceMap[R](mapper: A => R)(using monoid: Monoid[R]): R = ???

  def count: Long = ???

}

case object Empty extends Stream[Nothing] {
  override val isEmpty = true
}

case class Cons[+A](hdFn: () => A, tlFn: () => Stream[A]) extends Stream[A] {
  override val isEmpty = false
}


object Stream {
  // Task 9.1: Factory methods
  def apply[A](head: => A, tail: => Stream[A]): Stream[A] =
    Cons(() => head, () => tail)

  def from[A](lst: List[A]): Stream[A] = lst match
    case Nil => Empty
    case head :: tail => Stream(head, from(tail))

  def iterate[A](first: A, nextFn: A => A): Stream[A] =
    Cons(() => first, () => iterate(nextFn(first), nextFn))
}
