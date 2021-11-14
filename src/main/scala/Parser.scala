package lox

import TokenType._
import lox.grammar.Binary
import lox.grammar.Unary
import lox.grammar.Literal
import lox.grammar.Expr
import lox.grammar.Grouping
import scala.util.Try
import scala.util.Success
import lox.grammar.ExprList

class ParseError extends Exception

object Parser:
  def error(token: Token, message: String) =
    if (token.tokenType == EOF)
      report(token.line, " at end of file", message)
    else
      report(token.line, s" at '${token.lexeme}'", message)
    new ParseError

  def report(line: Int, location: String, message: String) =
    println(s"${message} ${location} on line ${line}")

class Parser(tokens: List[Token]):
  var current: Int = 0;
  val skipTypes = List(CLASS, FOR, FUN, IF, PRINT, RETURN, VAR, WHILE)

  def parse(): Try[Expr] =
    Try { expressionList }

  // expression_list ->  expression (',' expression_list)
  private def expressionList: ExprList = 
    var expr = expression
    if (matchTokens(COMMA))
      ExprList(expr, Some(expressionList))
    else
      ExprList(expr)

  // expression -> equality
  private def expression = equality

  // equality   -> comparison (('==' | '!=' comparison)*
  private def equality =
    var expr = comparison
    while (matchTokens(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison
      expr = Binary(expr, operator, right)
    }
    expr

  // comparison -> term (('>' | '>=' | '<' | '<=') term)*
  private def comparison =
    var expr = term
    while (matchTokens(LESS, LESS_EQUAL, GREATER, GREATER_EQUAL)) {
      val operator = previous()
      val right = term
      expr = Binary(expr, operator, right)
    }
    expr

  // term       -> factor (("+" | "-") factor)*
  private def term =
    var expr = factor
    while (matchTokens(PLUS, MINUS)) {
      val operator = previous()
      val right = factor
      expr = Binary(expr, operator, right)
    }
    expr

  // factor     -> unary (("/" | "*") unary)*
  private def factor =
    var expr = unary
    while (matchTokens(STAR, SLASH)) {
      val operator = previous()
      val right = unary
      expr = Binary(expr, operator, right)
    }
    expr

  // unary      -> (("!" | "-") unary) | primary
  private def unary: Expr =
    if (matchTokens(BANG, MINUS)) {
      val operator = previous()
      val right = unary
      Unary(operator, right)
    } else primary

  // primary    -> number | string | true | false | Nil | "(" expression ")"
  private def primary = () match {
    case () if matchTokens(FALSE) => Literal(false)
    case () if matchTokens(TRUE) => Literal(true)
    case () if matchTokens(NIL) => Literal(null)
    case () if matchTokens(STRING, NUMBER) => Literal(previous().literal)
    case () if matchTokens(LEFT_PAREN) =>
      val expr = expression
      consume(RIGHT_PAREN, "Expected an ')' after an expression. Did you miss a brace?")
      Grouping(expr)
    case () => throw Parser.error(peek(), "Expected an expression.")
  }

  private def matchTokens(tokens: TokenType*) =
    tokens.exists(token =>
      if (check(token)) {
        advance()
        true
      } else false
    )

  private def check(tokenType: TokenType) =
    if (isAtEnd) false else peek().tokenType == tokenType

  private def isAtEnd = peek().tokenType == EOF

  private def peek() = tokens(current)

  private def advance() =
    if (!isAtEnd) current = current + 1
    previous()

  private def consume(tokenType: TokenType, message: String) = {
    if (check(tokenType)) advance()
    else throw Parser.error(peek(), message)
  }

  private def previous() = tokens(current - 1)

  private def synchronize() =
    advance()
    while(!isAtEnd) {
      if (previous().tokenType != SEMICOLON)
        if (skipTypes.contains(peek().tokenType))
          ()
        else advance()
    }
