package stream

object StreamTest extends App {

  import Stream._

  // Task 9.4.a Use iterate for defining an infinite stream of positive integers,
  //    filter out odd values, map them to their square and print out the first 10.
  private val intsFrom1: Stream[Int] =
    iterate(1, i => i + 1).filter(_ % 2 == 0).map(i => i * i).take(10)
  for (i <- intsFrom1.toList) {
    println(i)
  }
  println

  // Task 9.4.b	Use iterate for defining an infinite stream of powers of 2 starting by 2.
  //    Take the first 10 and create a list.
  private val powerOf2: List[Int] = iterate(2, i => i * 2).take(10).toList
  powerOf2.foreach(println)
  println

  // Task 9.4.c)	Use from for creating a Stream from a list of words, filter out those of length 0,
  //    map them to their length and print out the result
  val words = List("Hannah", "Ann", "Sepp", "Peter", "", "Paul", "Flora", "Nick")
  private val wordLengths: Stream[Int] = from(words).filter(_.nonEmpty).map(_.length)
  wordLengths.forEach(println)
  println

  // Task 9.4.d)	Use from for creating a Stream from a list of words and
  //    find a word which contain a "A" or "a"". Create a list.
  private val wordsA: List[String] = from(words).filter(word => word.contains("A") || word.contains("a")).toList
  wordsA.foreach(println)
  println

  // Task 9.4.e	Use iterate for creating a Stream of all integers
  //  starting with 1000 and find the first prime number (Use filter and headOption)
  private val firstPrime: Option[Int] = iterate(1000, i => i + 1).filter(isPrime).headOption
  println(s"The first prime number: ${firstPrime.get}")
}

def isPrime(x: Int) : Boolean = {
  for (i <- 2 to Math.sqrt(x).toInt) {
    if (x % i == 0) return false
  }
  true
}