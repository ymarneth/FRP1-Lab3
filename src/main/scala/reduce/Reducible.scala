package reduce

import Monoid.{intPlusMonoid, *}
import tree.{BinNode, BinTree, EmptyTree}

trait Reducible[A] {
  def reduceMap[B](mapper: A => B)(using monoid: Monoid[B]): B
  def reduce(using monoid: Monoid[A]): A = reduceMap(a => a)

  def asList: List[A] = ???
  def asSet: Set[A] = ???
  def count: Int = ???
  def sum(fn: A => Int): Int = ???
}

object Reducible {

  def apply[A](as: Iterable[A]): Reducible[A] = new Reducible[A]:
    override def reduceMap[B](mapper: A => B)(using monoid: Monoid[B]): B = {
      var result = monoid.zero
      for a <- as do result = monoid.op(result, mapper(a))
      result
    }

  def apply[A](tree: BinTree[A]): Reducible[A] = ???

}

