package monads

import expr.*

import scala.util.parsing.combinator.JavaTokenParsers

object ExprParser extends JavaTokenParsers {

  def expr: Parser[Expr] = lit | vbl | add | mult | rec | min
  private def lit: Parser[Lit] = decimalNumber ^^ (d => Lit(d.toDouble))
  private def vbl: Parser[Var] = ident ^^ (n => Var(n))
  private def add: Parser[Add] = "(" ~> expr ~ "+" ~ expr <~ ")" ^^ { case l ~ op ~ r => Add(l, r) }
  private def mult: Parser[Mult] = "(" ~> expr ~ "*" ~ expr <~ ")" ^^ { case l ~ op ~ r => Mult(l, r) }
  private def min: Parser[Min] = "(" ~> "-" ~> expr <~ ")" ^^ (s => Min(s))
  private def rec: Parser[Rec] = "(" ~> "/" ~> expr <~ ")" ^^ (s => Rec(s))

  def parseExpr(input: String): ParseResult[Expr] = {
    parse(expr, input)
  }

}
