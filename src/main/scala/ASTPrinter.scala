package lox

import lox.grammar._
import lox.Token
import lox.TokenType

object AstPrinter {
  def programToString(program: List[Stmt]) = program.map(stmtToString).mkString(";\n")

  def stmtToString(stmt: Stmt) = stmt match
    case Expression(expr) => exprToString(expr)
    case Print(expr) => s"print(${exprToString(expr)})"
    case VarDecl(name, value) => s"= ${name.lexeme} ${exprToString(value)}"

  def exprToString(expr: Expr): String = expr match
    case ExprList(expr, maybeExprList) => s"${exprToString(expr)} ${maybeExprList.map(expr => s", ${exprToString(expr)}").getOrElse("")}"
    case Unary(token, literal) => token.lexeme + exprToString(literal)
    case Binary(left, operator, right) => s"${operator.lexeme} ${exprToString(left)} ${exprToString(right)}"
    case Grouping(expr) => exprToString(expr)
    case Literal(value) => value.toString
}
