package stream

import org.scalatest.funspec.AnyFunSpec
import rand.Gen

class StreamTests extends AnyFunSpec {
  describe("Task 9.1 Factory methods") {
    it("apply should create a non-empty stream") {
      val stream = Stream(1, Empty)
      assert(!stream.isEmpty)
      assert(stream.head == 1)
    }

    it("from should create a stream from a list") {
      val list = List(1, 2, 3)
      val stream = Stream.from(list)
      assert(!stream.isEmpty)
      assert(stream.head == 1)
      assert(stream.tail.head == 2)
      assert(stream.tail.tail.head == 3)
      assert(stream.tail.tail.tail.isEmpty)
    }

    it("iterate should create an infinite stream") {
      val stream = Stream.iterate(1, _ + 1)
      assert(!stream.isEmpty)
      assert(stream.head == 1)
      assert(stream.tail.head == 2)
      assert(stream.tail.tail.head == 3)
    }

    it("from should create an empty stream from an empty list") {
      val stream = Stream.from(Nil)
      assert(stream.isEmpty)
    }

    it("iterate should handle complex functions") {
      val stream = Stream.iterate(1, _ * 2)
      assert(stream.head == 1)
      assert(stream.tail.head == 2)
      assert(stream.tail.tail.head == 4)
    }
  }

  describe("Task 9.2 + Task 9.3 Stream operations") {
    it("head should return the first element") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.head == 1)
    }

    it("tail should return the rest of the stream") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.tail.head == 2)
      assert(stream.tail.tail.head == 3)
    }

    it("headOption should return the first element") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.headOption.contains(1))
    }

    it("tailOption should return the rest of the stream") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.tailOption.get.head == 2)
      assert(stream.tailOption.get.tail.head == 3)
    }

    it("forEach should apply the function to all elements") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      var sum = 0
      stream.forEach(sum += _)
      assert(sum == 6)
    }

    it("toList should convert the stream to a list") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.toList == List(1, 2, 3))
    }

    it("reduceMap should apply the function to all elements") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.reduceMap(_ * 2) == 12)
    }

    it("count should return the number of elements") {
      val stream = Stream(1, Stream(2, Stream(3, Empty)))
      assert(stream.count == 3)
    }
  }

  describe("Task 9.4 Using Stream") {
    it("Task 9.4.a should filter out odd values and map them to their square") {
      val stream = Stream.iterate(1, _ + 1).filter(_ % 2 == 0).map(i => i * i).take(10)
      assert(stream.toList == List(4, 16, 36, 64, 100, 144, 196, 256, 324, 400))
    }

    it("Task 9.4.b should create a stream of powers of 2") {
      val stream = Stream.iterate(2, _ * 2).take(10)
      assert(stream.toList == List(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))
    }

    it("Task 9.4.c should filter out words of length 0 and map them to their length") {
      val words = List("Hannah", "Ann", "Sepp", "Peter", "", "Paul", "Flora", "Nick")
      val stream = Stream.from(words).filter(_.nonEmpty).map(_.length)
      assert(stream.toList == List(6, 3, 4, 5, 4, 5, 4))
    }

    it("Task 9.4.d should find words containing 'A' or 'a'") {
      val words = List("Hannah", "Ann", "Sepp", "Peter", "", "Paul", "Flora", "Nick")
      val stream = Stream.from(words).filter(word => word.contains("A") || word.contains("a"))
      assert(stream.toList == List("Hannah", "Ann", "Paul", "Flora"))
    }

    it("Task 9.4.e should find the first prime number") {
      val stream = Stream.iterate(1000, _ + 1).filter(isPrime)
      assert(stream.head == 1009)
    }
  }

  describe("Task 9.5 Stream from Gen") {
    it("Task 9.5.a should create a stream of integers to 100 and take 10 values") {
      val stream = Gen.intsTo(100).stream.take(10)
      assert(stream.toList.length == 10)
    }

    it("Task 9.5.b should create a stream of words of maximal length 20 and take 10 values") {
      val stream = Gen.words(20).stream.take(10)
      assert(stream.toList.length == 10)
      assert(stream.toList.forall(_.length <= 20))
    }

    it("Task 9.5.c should find the first prime number") {
      val stream = Gen.intsFromTo(2, 100).stream.filter(isPrime)
      assert(stream.head == 3)
    }

    it("Task 9.5.d should find the first word containing 'x'") {
      val stream = Gen.words(20).stream.filter(word => word.contains("x"))
      assert(stream.head.contains("x"))
    }
  }
}