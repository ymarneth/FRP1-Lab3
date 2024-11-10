package rand

import rand.Gen.*

object GenTests {

  def main(args: Array[String]): Unit = {

    println("\n===================================================================================================")
    println("     Task 8 - Random Generators   ")
    println("===================================================================================================\n")

    // ints.flatMap(i1 => ints.map(i2 => (i1, i2)))
    val pairGen =
      for {
        i1 <- ints
        i2 <- ints
      } yield (i1, i2)

    val rp = pairGen(574934312343L)
    println(rp)
    println

    println("===== Task 8.4.a Generator of list of 10 Int values from 0 to 100 =====")
    val b10: Gen[List[Int]] = intsFromTo(0, 100).lists(10)
    val (bs, _) = b10(3293)
    for (b <- bs) println(b)
    println


    println("===== Task 8.4.b Generator of list of 10 Boolean values with probability 0.5 =====")
    val i10: Gen[List[Boolean]] = booleans(0.5).lists(10)
    val (is, _) = i10(40591)
    for (i <- is) println(i)
    println

    println("===== Task 8.4.c Generator for list with 10 lists with length between 2 and 10 of integers from 0 to 100 =====")
    val nIntListLists: Gen[List[List[Int]]] = intsFromTo(0, 100).listsOfLengths(2, 10).lists(10)
    val (r1, _) = nIntListLists(34243)
    for (l <- r1)
      println(l)
    println

    println("===== Task 8.4.d Generator for list with 10 random words up to 10 characters =====")
    val nWordsLists: Gen[List[String]] = words(10).lists(10)
    val (r3, _) = nWordsLists(23987)
    for (l <- r3) println(l)
    println

    println("===== Task 8.4.e Generator for list with 10 random values from Strings 'A', 'B', 'C' =====")
    val nElemsLists: Gen[List[String]] = valuesOf("A", "B", "C").lists(10)
    val (r4, _) = nElemsLists(87236481)
    for (l <- r4) println(l)
    println

    println("===== Further tests: Generator for list with 10 random Double values from 0 to 1 =====")
    val d10 = doublesFromTo(0.0, 1.0).lists(10)
    val (r5, _) = d10(12345)
    for (l <- r5) println(l)
    println

    println("===== Further tests: Generator for list with 10 random Char values from 'a' to 'z' =====")
    val c10 = letters.lists(10)
    val (r6, _) = c10(12345)
    for (l <- r6) println(l)
    println

    println("===== Further tests: intsFromTo with zero-width range =====")
    val zeroRange = intsFromTo(5, 5).lists(10)
    val (zr, _) = zeroRange(100)
    println(zr)
    println

    println("===== Further tests: int list only 5 allowed =====")
    val emptyList = ints.lists(0)
    val (el, _) = emptyList(100)
    println(el)
    println

    println("===== Further tests: Boolean generator with probability 1.0 =====")
    val allTrueBooleans = booleans(1.0).lists(10)
    val (atb, _) = allTrueBooleans(200)
    println(atb)
    println

    println("===== Further tests: Boolean generator with probability 0.0 =====")
    val allFalseBooleans = booleans(0.0).lists(10)
    val (afb, _) = allFalseBooleans(200)
    println(afb)
    println
  }
}
