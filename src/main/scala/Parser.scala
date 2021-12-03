package lox

import TokenType._
import lox.grammar._
import scala.util.Try
import scala.util.Success

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
  val skipTypes = List(CLASS, FOR, FUN, IF, RETURN, VAR, WHILE)

  def parse(): Try[List[Stmt]] =
    Try { program }

  // program    -> statement* EOF
  private def program =
    var declarations = List.empty[Option[Stmt]]
    while (peek().tokenType != EOF) {
      declarations = declarations :+ declaration
    }
    declarations.flatten

  // declaration  -> varDecl | statement
  private def declaration: Option[Stmt] =
    try {
      if (matchTokens(VAR)) Some(varDecl)
      else Some(statement)
    } catch {
      case e: ParseError =>
        synchronize()
        None
    }

  // varDecl  -> 'var' identifier ('=' expression)? ";"
  private def varDecl =
    val varName = consume(IDENTIFIER, "Expected variable name")
    var initialisedValue = if (matchTokens(EQUAL)) expression else null
    consume(SEMICOLON, "Expected ';' after variable declaration")
    VarDecl(varName, initialisedValue)

  // statement  -> exprStmt | printStmt | block
  private def statement: Stmt =
    if (matchTokens(PRINT)) printStmt
    else if (matchTokens(LEFT_BRACE)) block
    else exprStmt

  // block      -> "{" declaration* "}"
  private def block: Block =
    var declarations = List.empty[Option[Stmt]]
    while (peek().tokenType != RIGHT_BRACE && !isAtEnd) {
      declarations = declarations :+ declaration
    }
    consume(RIGHT_BRACE, "Expected a '}' at the end of a block")
    Block(declarations.flatten)

  // printStmt  -> "print" expression_list
  private def printStmt =
    val expr = expressionList
    consume(SEMICOLON, "Expected an ';' after an expression.")
    Print(expr)

  // exprStmt   -> expression_list
  private def exprStmt =
    val expr = expressionList
    consume(SEMICOLON, "Expected an ';' after an expression.")
    Expression(expr)

  // expression_list ->  expression (',' expression_list)
  private def expressionList: ExprList =
    var expr = expression
    if (matchTokens(COMMA))
      ExprList(expr, Some(expressionList))
    else
      ExprList(expr)

  // expression -> equality
  private def expression = assignment

  // assignment -> IDENTIFIER '=' assignment | equality
  private def assignment =
    val expr = equality

    (matchTokens(EQUAL), expr) match
      case (true, Variable(name)) =>
        Assign(name, equality)
      case (false, expr) => expr
      case _ => throw Parser.error(previous(), s"Cannot assign to an expression")

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
    case () if matchTokens(IDENTIFIER) => Variable(previous())
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

  private def synchronize(): Unit =
    advance()
    while(!isAtEnd) {
      if (previous().tokenType == SEMICOLON)
        return
      if (skipTypes.contains(peek().tokenType))
        return
      advance()
    }
