package reduce

import Monoid.{intPlusMonoid, *}
import tree.{BinNode, BinTree, EmptyTree}

trait Reducible[A] {
  def reduceMap[B](mapper: A => B)(using monoid: Monoid[B]): B

  def reduce(using monoid: Monoid[A]): A = reduceMap(a => a)

  def asList: List[A] = reduceMap(List(_)) (using listMonoid)

  def asSet: Set[A] = reduceMap(Set(_)) (using setMonoid())

  def count: Int = reduceMap(_ => 1)

  def sum(fn: A => Int): Int = reduceMap(fn)
}

object Reducible {

  def apply[A](as: Iterable[A]): Reducible[A] = new Reducible[A]:
    override def reduceMap[B](mapper: A => B)(using monoid: Monoid[B]): B = {
      var result = monoid.zero
      for a <- as do result = monoid.op(result, mapper(a))
      result
    }

  def apply[A](tree: BinTree[A]): Reducible[A] = new Reducible[A]:
    override def reduceMap[B](mapper: A => B)(using monoid: Monoid[B]): B = {
      def reduceTree(tree: BinTree[A]): B = tree match {
        case EmptyTree => monoid.zero
        case BinNode(value, left, right) =>
          val leftResult = reduceTree(left)
          val rightResult = reduceTree(right)
          monoid.op(mapper(value), monoid.op(leftResult, rightResult))
      }

      reduceTree(tree)
    }
}
