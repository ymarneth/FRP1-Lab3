package stream

import reduce.*

sealed trait Stream[+A] {
  val isEmpty: Boolean

  // Task 9.2: Lazy methods in trait Stream
  def map[B](mapper: A => B): Stream[B] =
    this match
      case Empty => Empty
      case Cons(headFn, tailFn) => Stream(mapper(headFn()), tailFn().map(mapper))

  def take(n: Int): Stream[A] =
    this match
      case Empty => Empty
      case Cons(headFn, tailFn) =>
        if (n == 0) Empty
        else Stream(headFn(), tailFn().take(n - 1))

  def filter(pred: A => Boolean): Stream[A] =
    this match
      case Empty => Empty
      case Cons(headFn, tailFn) =>
        if pred(headFn()) then Stream(headFn(), tailFn().filter(pred))
        else tailFn().filter(pred)

  // Task 9.3: Methods in trait Stream returning results
  def head: A =
    this match
      case Empty => throw new NoSuchElementException("head of empty stream")
      case Cons(headFn, _) => headFn()

  def tail: Stream[A] =
    this match
      case Empty => throw new UnsupportedOperationException("tail of empty stream")
      case Cons(_, tailFn) => tailFn()

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

  def reduceMap[R](mapper: A => R)(using monoid: Monoid[R]): R =
    this match
      case Empty => monoid.zero
      case Cons(headFn, tailFn) => monoid.op(mapper(headFn()), tailFn().reduceMap(mapper))

  def count: Long =
    this match
      case Empty => 0
      case Cons(_, tailFn) => 1 + tailFn().count
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
