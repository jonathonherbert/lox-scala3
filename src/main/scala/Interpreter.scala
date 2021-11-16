package lox

import TokenType._
import lox.grammar._

type LoxValue  = String | Double | Boolean

class InterpreterError(message: String) extends Exception

object Interpreter {
	def evaluate(program: List[Stmt]) = program.foreach {
		case Expression(expr) => evaluateExpr
		case Print(expr) => 
			println(evaluateExpr(expr))
	}

	def evaluateExpr(expr: Expr): LoxValue = expr match
		case ExprList(left, right) =>
			val leftVal = evaluateExpr(left)
			right.map(evaluateExpr).getOrElse(leftVal)
		case Binary(left, operator, right) =>
			val leftVal = evaluateExpr(left)
			val rightVal = evaluateExpr(right)
			(operator.tokenType, leftVal, rightVal) match
				case (EQUAL_EQUAL, l, r) => l == r
				case (GREATER_EQUAL, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l >= r
				case (LESS_EQUAL, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l <= r
				case (LESS, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l < r
				case (GREATER, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l > r
				case (STAR, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l * r
				case (SLASH, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l / r
				case (MINUS, l: Double, r: Double) =>
					assertNumericOperation(l, r, operator.tokenType)
					l - r
				case (PLUS, l: Double, r: Double) =>
					l + r
				case (PLUS, l, r) =>
					l.toString + r.toString
				case _ => throw new InterpreterError(s"Cannot use operator ${operator.tokenType} with ${leftVal} and ${rightVal}")
		case Grouping(expr) => evaluateExpr(expr)
		case Literal(value) => value
		case Unary(operator, right) =>
			val rightVal = evaluateExpr(right)
			(operator.tokenType, rightVal) match
				case (BANG, _) => !isTruthy(rightVal)
				case (MINUS, r: Double) => -r

	def isTruthy(value: LoxValue): Boolean = value match
		case _: Double => false
		case _: String => false
		case b: Boolean => b

	def assertNumericOperation(l: LoxValue, r: LoxValue, t: TokenType) =
		if (!isNumber(l) || !isNumber(r)) throw new InterpreterError(s"Operator ${t} must receive two numbers – instead, it got ${l} and ${r}")

	def isString(value: LoxValue) = value match
		case _: String => true
		case _ => false

	def isNumber(value: LoxValue) = value match
		case _: Double => true
		case _ => false

	def isBool(value: LoxValue) = value match
		case _: Boolean => true
		case _ => false
}