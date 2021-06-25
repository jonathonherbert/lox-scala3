package lox.grammar

import lox.Token

trait Expr

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: String | Number | Boolean) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
