package monads

import expr.*
import org.scalatest.funspec.AnyFunSpec

class ExprEvalTests extends AnyFunSpec {

  describe("Task 7.1 - Expression Evaluator with Option Monad") {
    it("should evaluate x * (1 / y) with Option monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr1 = Mult(Var("x"), Rec(Var("y")))
      val r1 = ExprEvalOption.eval(expr1, bds)
      assert(r1.contains(0.75))
    }

    it("should evaluate x * (1 / z) with Option monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr2 = Mult(Var("x"), Rec(Var("z")))
      val r2 = ExprEvalOption.eval(expr2, bds)
      assert(r2.isEmpty)
    }

    it("should evaluate x * (1 / u) with Option monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr3 = Mult(Var("x"), Rec(Var("u")))
      val r3 = ExprEvalOption.eval(expr3, bds)
      assert(r3.isEmpty)
    }
  }

  describe("Task 7.2 - Expression Evaluator with Try Monad") {
    it("should evaluate x * (1 / y) with Try monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr1 = Mult(Var("x"), Rec(Var("y")))
      val r1 = ExprEvalTry.eval(expr1, bds)
      assert(r1.isSuccess)
      assert(r1.get == 0.75)
    }

    it("should evaluate x * (1 / z) with Try monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr2 = Mult(Var("x"), Rec(Var("z")))
      val r2 = ExprEvalTry.eval(expr2, bds)
      assert(r2.isFailure)
    }

    it("should evaluate x * (1 / u) with Try monad") {
      val bds = Map("x" -> 3.0, "y" -> 4.0, "z" -> 0.0)

      val expr3 = Mult(Var("x"), Rec(Var("u")))
      val r3 = ExprEvalTry.eval(expr3, bds)
      assert(r3.isFailure)
    }
  }

  describe("Task 7.3 - Expression Evaluator with Set Monad") {
    it("should evaluate x * (1 / y) with Set monad") {
      val bds = Map("x" -> Set(3.0), "y" -> Set(2.0, 3.0))

      val expr1 = Mult(Var("x"), Rec(Var("y")))
      val r1 = ExprEvalSet.eval(expr1, bds)
      assert(r1 == Set(1.5, 1.0))
    }

    it("should evaluate x * (1 / z) with Set monad") {
      val bds = Map("x" -> Set(3.0), "y" -> Set(2.0, 3.0))

      val expr2 = Mult(Var("x"), Rec(Var("z")))
      val r2 = ExprEvalSet.eval(expr2, bds)
      assert(r2.isEmpty)
    }

    it("should evaluate x * (1 / u) with Set monad") {
      val bds = Map("x" -> Set(3.0), "y" -> Set(2.0, 3.0))

      val expr3 = Mult(Var("x"), Rec(Var("u")))
      val r3 = ExprEvalSet.eval(expr3, bds)
      assert(r3.isEmpty)
    }
  }
}
