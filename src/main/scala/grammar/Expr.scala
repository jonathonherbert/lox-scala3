package lox.grammar

import lox.Token

trait Expr

case class ExprList(left: Expr, right: Option[ExprList] = None) extends Expr
case class Assign(name: Token, value: Expr) extends Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: String | Double | Boolean) extends Expr
case class Variable(name: Token) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
