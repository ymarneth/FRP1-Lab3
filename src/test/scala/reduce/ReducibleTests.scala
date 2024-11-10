package reduce

import org.scalatest.funspec.AnyFunSpec
import reduce.Monoid.setMonoid
import tree.BinTree
import tree.BinTree.node

class ReducibleTests extends AnyFunSpec {

  describe("Task6_2_ListReducible") {
    it("should count the elements") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)

      val n = namesReducible.reduceMap(_ => 1)
      assert(n == 7)
    }

    it("should concatenate the elements to a single string") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)

      val one = namesReducible.reduce
      assert(one == "SusiFritzHansAloisJosefGustPeter")
    }

    it("should compute length of all strings") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)

      val length = namesReducible.reduceMap(_.length)
      assert(length == 4 + 5 + 4 + 5 + 5 + 4 + 5)
    }

    it("should create a set of the elements") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)

      val setOfNames: Set[String] = namesReducible.reduceMap(name => Set(name))(using setMonoid[String]())
      assert(setOfNames == Set("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter"))
    }

    it("should compute the max length of all strings") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)

      val maxLength = namesReducible.reduceMap(name => name.length)(using Monoid(0, (l1, l2) => if l1 > l2 then l1 else l2))
      assert(maxLength == 5)
    }
  }

  describe("Task6_2_TreeReducible") {
    it("should count the elements") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)

      val n = namesReducible.reduceMap(_ => 1)
      assert(n == 7)
    }

    it("should concatenate the elements to a single string") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)

      val one = namesReducible.reduce
      assert(one == "SusiFritzAloisGustPeterJosefHans")
    }

    it("should compute length of all strings") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)

      val length = namesReducible.reduceMap(_.length)
      assert(length == 4 + 5 + 5 + 4 + 5 + 5 + 4)
    }

    it("should create a set of the elements") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)

      val setOfNames: Set[String] = namesReducible.reduceMap(name => Set(name))(using setMonoid[String]())
      assert(setOfNames == Set("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter"))
    }
  }

  describe("Task6_3_ListReducible") {
    it("should count the elements") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)
      assert(namesReducible.count == 7)
    }

    it("should concatenate the elements to a single string") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)
      assert(namesReducible.reduce == "SusiFritzHansAloisJosefGustPeter")
    }

    it("should compute length of all strings") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)
      val length = namesReducible.sum(_.length)
      assert(length == 4 + 5 + 4 + 5 + 5 + 4 + 5)
    }

    it("should create a set of the elements") {
      val names = List("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter")
      val namesReducible = Reducible(names)
      val setOfNames = namesReducible.reduceMap(name => Set(name))(using setMonoid[String]())
      assert(setOfNames == Set("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter"))
    }
  }

  describe("Task6_3_TreeReducible") {
    it("should count the elements") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)
      assert(namesReducible.count == 7)
    }

    it("should concatenate the elements to a single string") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)
      assert(namesReducible.reduce == "SusiFritzAloisGustPeterJosefHans")
    }

    it("should compute length of all strings") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)
      val length = namesReducible.sum(_.length)
      assert(length == 4 + 5 + 5 + 4 + 5 + 5 + 4)
    }

    it("should create a set of the elements") {
      val nameTree: BinTree[String] =
        node("Susi",
          node(
            "Fritz",
            node(
              "Alois",
              node(
                "Gust", BinTree.empty, BinTree.empty
              ),
              node("Peter", BinTree.empty, BinTree.empty)
            ),
            node("Josef", BinTree.empty, BinTree.empty)
          ),
          node("Hans", BinTree.empty, BinTree.empty)
        )

      val namesReducible = Reducible(nameTree)
      val setOfNames = namesReducible.reduceMap(name => Set(name))(using setMonoid[String]())
      assert(setOfNames == Set("Susi", "Fritz", "Hans", "Alois", "Josef", "Gust", "Peter"))
    }
  }

}
