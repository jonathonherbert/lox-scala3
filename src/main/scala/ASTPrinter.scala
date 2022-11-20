package lox

import lox.grammar._
import lox.Token
import lox.TokenType

object AstPrinter {
  def programToString(program: List[Stmt]) =
    program.map(stmtToString).mkString(";\n")

  def stmtToString(stmt: Stmt): String = stmt match
    case Expression(expr)     => exprToString(expr)
    case Print(expr)          => s"print(${exprToString(expr)})"
    case VarDecl(name, value) => s"= ${name.lexeme} ${exprToString(value)}"
    case Block(statements) =>
      s"Block start:\n${statements.map(statement => s"\t${stmtToString(statement)}").mkString("\n")}"
    case IfStmt(expr, thenStmt, elseStmt) =>
      s"If ${expr} \n  ${thenStmt}\n else ${elseStmt}"
    case WhileStmt(expr, stmt) => s"While ${expr} \n ${stmtToString(stmt)}"

  def exprToString(expr: Expr): String = expr match
    case Variable(name) => s"var ${name.lexeme}".trim
    case Assign(name, expr) =>
      s"reassign ${name.lexeme} to ${exprToString(expr)}"
    case Logical(left, operator, right) =>
      s"${exprToString(left)} ${operator.tokenType} ${exprToString(right)}"
    case ExprList(expr, maybeExprList) =>
      s"${exprToString(expr)} ${maybeExprList.map(expr => s", ${exprToString(expr)}").getOrElse("")}"
    case Unary(token, literal) => token.lexeme + exprToString(literal)
    case Binary(left, operator, right) =>
      s"${operator.lexeme} ${exprToString(left)} ${exprToString(right)}"
    case Grouping(expr) => exprToString(expr)
    case Literal(value) => value.toString
}
