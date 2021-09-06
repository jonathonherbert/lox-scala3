package lox

import lox.grammar._
import lox.Token
import lox.TokenType

object AstPrinter {
  def exprToString(expr: Expr): String = expr match {
    case Unary(token, literal) => token.lexeme + exprToString(literal)
    case Binary(left, operator, right) => s"${operator.lexeme} ${exprToString(left)} ${exprToString(right)}"
    case Grouping(expr) => exprToString(expr)
    case Literal(value) => value.toString
  }
}
