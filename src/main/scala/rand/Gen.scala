package rand

import rand.Gen.intsFromTo
import stream.*

trait Gen[A] extends (Long => (A, Long)) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    seed => {
      val (a, nextSeed) = this (seed)
      f(a)(nextSeed)
    }

  def map[B](f: A => B): Gen[B] =
    this.flatMap(a => Gen.unit(f.apply(a)))

  // TODO: Task 8.3.a)
  def lists(len: Int): Gen[List[A]] =
    if len == 0 then Gen.unit(List.empty)
    else
      for {
        ra <- this
        rl <- lists(len - 1)
      } yield ra :: rl

  // TODO: Task 8.3.b)
  def listsOfLengths(minLen: Int, maxLen: Int): Gen[List[A]] =
    intsFromTo(minLen, maxLen).flatMap(len => lists(len))

  // TODO: Task 9.5
  def stream(seed: Long): Stream[A] = ???

  def stream: Stream[A] = stream(System.currentTimeMillis())
}

object Gen {

  private def unit[A](a: A): Gen[A] = seed => (a, seed)

  val ints: Gen[Int] = seed => {
    val randInt = (seed >>> 16).toInt
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    (randInt, newSeed)
  }

  // TODO: Task 8.1: Random number generators
  val posInts: Gen[Int] = ints.map(i => i.abs)

  def intsFromTo(from: Int, to: Int): Gen[Int] =
    doubles.map(d => from + (d * (to - from)).toInt)

  def intsTo(to: Int): Gen[Int] = intsFromTo(0, to)

  val doubles: Gen[Double] = posInts.map(i => i.toDouble / Int.MaxValue.toDouble)

  def doublesFromTo(from: Double, to: Double): Gen[Double] = ???

  def doublesTo(to: Double): Gen[Double] = ???

  // TODO: Task 8.2: Discrete random values

  def booleans(prob: Double) = doubles.map(d => d < prob)

  def valuesOf[A](values: A*): Gen[A] = intsTo(values.length).map(i => values(i))

  val letters: Gen[Char] = valuesOf(
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
    'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  )

  // TODO: Task 8.3c: words
  def words(maxLength: Int): Gen[String] = letters
    .listsOfLengths(2, maxLength)
    .map(_.mkString)
}
