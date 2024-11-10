package stream

import rand.Gen
import rand.Gen.{booleans, intsFromTo, intsTo, valuesOf, words}

object GenStreamTest {

  def main(args: Array[String]): Unit = {

    // Task 9.5.a Create a stream from generator for integers to 100 and take 10 values and print them out
    val stream: Stream[Int] = Gen.intsTo(100).stream.take(10)
    stream.forEach(println)
    println
    
    // Task 9.5.b Create a stream from generator for words of maximal length 20 and take 10 values and print them out
    val streamWords: Stream[String] = Gen.words(20).stream.take(10)
    streamWords.forEach(println)
    println

    // Task 9.5.c Create a stream from generator for integers from 2 to 100 and find one which is a prime
    //  (use filter and headOption)
    val primeStream: Option[Int] = Gen.intsFromTo(2, 100).stream.filter(isPrime).headOption
    println(s"The first prime number: ${primeStream.get}")
    println

    // Task 9.5.d Create a stream from generator for words and find one which contains "x"
    //  (use filter and headOption)
    val wordStream: Option[String] = Gen.words(20).stream.filter(word => word.contains("x")).headOption
    println(s"The first word containing 'x': ${wordStream.get}")
  }
}
