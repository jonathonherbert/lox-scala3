package lox

enum TokenType:
  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER, STRING, NUMBER,

    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
    EOF

object Token:
  val reservedWords = Map(
    "and" -> TokenType.AND,
    "class" -> TokenType.CLASS,
    "else" -> TokenType.ELSE,
    "false" -> TokenType.FALSE,
    "for" -> TokenType.FOR,
    "fun" -> TokenType.FUN,
    "if" -> TokenType.IF,
    "nil" -> TokenType.NIL,
    "or" -> TokenType.OR,
    "print" -> TokenType.PRINT,
    "return" -> TokenType.RETURN,
    "super" -> TokenType.SUPER,
    "this" -> TokenType.THIS,
    "true" -> TokenType.TRUE,
    "var" -> TokenType.VAR,
    "while" -> TokenType.WHILE
  )

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Double | String | Null,
    line: Int
):
  override def toString = s"${tokenType} ${lexeme} ${literal} ${line}"
