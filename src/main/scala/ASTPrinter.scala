package lox

import lox.grammar._
import lox.Token
import lox.TokenType

object AstPrinter {
  def main() = {
    val expr = Binary(
      Unary(Token(TokenType.BANG, "!", null, 1), Literal(123)),
      Token(TokenType.STAR, "*", null, 1),
      Grouping(
        Literal(45.67)
      )
    )

    println(exprToString(expr))
  }

  def exprToString(expr: Expr): String = expr match {
    case Unary(token, literal) => token.lexeme + exprToString(literal)
    case Binary(left, operator, right) => s"${operator.lexeme} ${exprToString(left)} ${exprToString(right)}"
    case Grouping(expr) => exprToString(expr)
    case Literal(value) => value.toString
  }
}
