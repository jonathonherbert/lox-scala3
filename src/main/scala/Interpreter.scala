package lox

import TokenType._
import lox.grammar._

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

type LoxValue = String | Double | Boolean

class Environment(parent: Option[Environment] = None):
  private val values: Map[String, LoxValue] = Map.empty

  def get(name: String): Option[LoxValue] =
    values.get(name).orElse(parent.flatMap(_.get(name)))
  def set(name: String, value: LoxValue): Unit = parent match
    // Set in the parent, if a value exists.
    case Some(p) if p.get(name).isDefined => p.set(name, value)
    // Else, set locally.
    case _ => values += (name -> value)

  def mkString: String =
    s"${values} ${parent.map(p => s"${p.mkString}").getOrElse("END")}"

class InterpreterError(message: String) extends Exception(message)

object Interpreter {

  def evaluate(
      program: List[Stmt],
      environment: Environment = new Environment()
  ): List[String] =
    program.flatMap {
      case Expression(expr) =>
        evaluateExpr(expr, environment)
        None
      case Print(expr) =>
        val result = evaluateExpr(expr, environment)
        println(result)
        Some(result.toString())
      case VarDecl(name, expr) =>
        environment.set(name.lexeme, evaluateExpr(expr, environment))
        None
      case Block(stmts) =>
        evaluate(stmts, new Environment(Some(environment)))
      case IfStmt(expr, thenStmt, elseStmt) =>
        if (isTruthy(evaluateExpr(expr, environment)))
          evaluate(List(thenStmt), new Environment(Some(environment)))
        else
          elseStmt.toList.map(stmt =>
            evaluate(List(stmt), new Environment(Some(environment)))
          ).flatten
      case WhileStmt(expr, stmt) =>
        val results = ListBuffer.empty[List[String]]
        while (isTruthy(evaluateExpr(expr, environment)))
          results.addOne(evaluate(List(stmt), environment))
        results.toList.flatten
    }

  def evaluateExpr(expr: Expr, env: Environment): LoxValue = expr match
    case ExprList(left, right) =>
      val leftVal = evaluateExpr(left, env)
      right.map(r => evaluateExpr(r, env)).getOrElse(leftVal)
    case Binary(left, operator, right) =>
      val leftVal = evaluateExpr(left, env)
      val rightVal = evaluateExpr(right, env)
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
        case _ =>
          throw new InterpreterError(
            s"Cannot use operator ${operator.tokenType} with ${leftVal} and ${rightVal}"
          )
    case Grouping(expr) => evaluateExpr(expr, env)
    case Literal(value) => value
    case Unary(operator, right) =>
      val rightVal = evaluateExpr(right, env)
      (operator.tokenType, rightVal) match
        case (BANG, _)          => !isTruthy(rightVal)
        case (MINUS, r: Double) => -r
    case Variable(name) =>
      env.get(name.lexeme) match
        case Some(value) => value
        case None =>
          throw new InterpreterError(s"Variable ${name.lexeme} not found")
    case Assign(name, expr) =>
      if (env.get(name.lexeme).isDefined)
        env.set(name.lexeme, evaluateExpr(expr, env))
        null
      else
        println(env.mkString)
        throw new InterpreterError(
          s"Cannot assign to variable ${name}: it has not been defined"
        )
    case Logical(left, operator, right) =>
      val leftVal = evaluateExpr(left, env)
      operator.tokenType match
        case OR =>
          if (isTruthy(leftVal)) leftVal
          else evaluateExpr(right, env)
        case AND =>
          if (isTruthy(leftVal)) evaluateExpr(right, env)
          else leftVal
        case _ =>
          throw new InterpreterError(
            s"Tried to evaluate an operator, but operator ${operator.tokenType} not supported"
          )

  def isTruthy(value: LoxValue): Boolean = value match
    case _: Double  => false
    case _: String  => false
    case b: Boolean => b

  def assertNumericOperation(l: LoxValue, r: LoxValue, t: TokenType) =
    if (!isNumber(l) || !isNumber(r))
      throw new InterpreterError(
        s"Operator ${t} must receive two numbers – instead, it got ${l} and ${r}"
      )

  def isString(value: LoxValue) = value match
    case _: String => true
    case _         => false

  def isNumber(value: LoxValue) = value match
    case _: Double => true
    case _         => false

  def isBool(value: LoxValue) = value match
    case _: Boolean => true
    case _          => false
}
