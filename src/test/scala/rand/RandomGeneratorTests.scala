package rand

import org.scalatest.funspec.AnyFunSpec

class RandomGeneratorTests extends AnyFunSpec {

  it("should generate random integers based on seed") {
    val (i1, _) = Gen.ints(574934312343L)
    val (i2, _) = Gen.ints(642389034598L)
    assert(i1 != i2)
  }

  it("should generate the same random integers based on the same seed") {
    val (i1, _) = Gen.ints(574934312343L)
    val (i2, _) = Gen.ints(574934312343L)
    assert(i1 == i2)
  }

  it("should generate random positive integers based on seed") {
    val (i1, _) = Gen.posInts(574934312343L)
    val (i2, _) = Gen.posInts(642389034598L)
    assert(i1 != i2)
    assert(i1 > 0)
    assert(i2 > 0)
  }

  it("Task 8.4.a: should generate a list of 10 Int values from 0 to 100") {
    val b10: Gen[List[Int]] = Gen.intsFromTo(0, 100).lists(10)
    val (bs, _) = b10(3293)
    assert(bs.length == 10)
    for (b <- bs) assert(b >= 0 && b <= 100)
  }

  it("Task 8.4.b: should generate a list of 10 Boolean values with probability 0.5") {
    val i10: Gen[List[Boolean]] = Gen.booleans(0.5).lists(10)
    val (is, _) = i10(40591)
    assert(is.length == 10)
    for (i <- is) assert(i || !i)
  }

  it("Task 8.4.c: should generate a list with 10 lists with length between 2 and 10 of integers from 0 to 100") {
    val nIntListLists: Gen[List[List[Int]]] = Gen.intsFromTo(0, 100).listsOfLengths(2, 10).lists(10)
    val (r1, _) = nIntListLists(34243)
    assert(r1.length == 10)
    for (l <- r1) {
      assert(l.length >= 2 && l.length <= 10)
      for (i <- l) assert(i >= 0 && i <= 100)
    }
  }

  it("Task 8.4.d: should generate a list with 10 random words up to 10 characters") {
    val nWordsLists: Gen[List[String]] = Gen.words(10).lists(10)
    val (r3, _) = nWordsLists(23987)
    assert(r3.length == 10)
    for (l <- r3) {
      assert(l.length <= 10)
      for (c <- l) assert(c.isLetter)
    }
  }

  it("Task 8.4.e: should generate a list with 10 random values from Strings 'A', 'B', 'C'") {
    val nElemsLists: Gen[List[String]] = Gen.valuesOf("A", "B", "C").lists(10)
    val (r4, _) = nElemsLists(87236481)
    assert(r4.length == 10)
    for (l <- r4) assert(l == "A" || l == "B" || l == "C")
  }

  it("Further tests: should generate a list with with 10 random Double values from 0 to 1") {
    val d10 = Gen.doublesFromTo(0.0, 1.0).lists(10)
    val (r5, _) = d10(12345)
    assert(r5.length == 10)
    for (l <- r5) assert(l >= 0.0 && l <= 1.0)
  }

  it("Further tests: should generate a list with 10 random Char values from 'a' to 'z'") {
    val c10 = Gen.letters.lists(10)
    val (r6, _) = c10(12345)
    assert(r6.length == 10)
    for (l <- r6) assert(l.isLetter && l >= 'a' && l <= 'z')
  }

  it("Further tests: intsFromTo with zero-width range") {
    val zeroRange = Gen.intsFromTo(5, 5).lists(10)
    val (zr, _) = zeroRange(100)
    assert(zr.length == 10)
    for (i <- zr) assert(i == 5)
  }

  it("Further tests: int list only 5 allowed") {
    val zeroRange = Gen.intsFromTo(5, 5).lists(10)
    val (zr, _) = zeroRange(100)
    assert(zr.length == 10)
    for (i <- zr) assert(i == 5)
  }

  it("Further tests: Boolean generator with probability 1.0") {
    val emptyList = Gen.booleans(1.0).lists(10)
    val (el, _) = emptyList(100)
    assert(el.length == 10)
    for (b <- el) assert(b)
  }

  it("Further tests: Boolean generator with probability 0.0") {
    val emptyList = Gen.booleans(0.0).lists(10)
    val (el, _) = emptyList(100)
    assert(el.length == 10)
    for (b <- el) assert(!b)
  }
}
