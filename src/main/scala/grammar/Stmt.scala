package lox.grammar

import lox.Token

trait Stmt

case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class VarDecl(name: Token, value: Expr) extends Stmt
