package reduce

object Task6_2_ListReducible {

  def main(args: Array[String]): Unit = {

    import Monoid.*

    val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
    val namesReducible = Reducible(names)

    // === Task 6.2 ====================

    //a) count the elements
    val n = namesReducible.reduceMap(_ => 1) //intPlusMonoid
    println(s"Number elements = $n")

    //b) concatenate the elements to a single string
    val one = namesReducible.reduce
    println(s"Concatenated = $one")

    //c) compute length of all strings
    val length = namesReducible.reduceMap(_.length) //intPlusMonoid
    println(s"Length of elements = $length")

    //d) create a set of the elements
    val setOfNames: Set[String] = namesReducible.reduceMap(name => Set(name))(using setMonoid[String]()) //setMonoid
    println(s"Set of elements = $setOfNames")

    val maxLength = namesReducible.reduceMap(name => name.length)(using Monoid(0, (l1, l2) => if l1 > l2 then l1 else l2))
  }
}
