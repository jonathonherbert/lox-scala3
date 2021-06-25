package lox

class ScannerTest extends BaseTest {

  describe("A Set") {
    it("should handle a simple program") {
      val scanner = new Scanner("""
        if (example) {
          return 1
        }"""
      )
      val tokens = scanner.scanTokens
      val expectedTokens = List(
        Token(TokenType.IF, "if", null, 2),
        Token(TokenType.LEFT_PAREN, "(",  null, 2),
        Token(TokenType.IDENTIFIER, "example", null, 2),
        Token(TokenType.RIGHT_PAREN, ")",  null, 2),
        Token(TokenType.LEFT_BRACE, "{",  null, 2),
        Token(TokenType.RETURN, "return", null, 3),
        Token(TokenType.NUMBER, "1",  1.0, 3),
        Token(TokenType.RIGHT_BRACE, "}",  null, 4),
        Token(TokenType.EOF, "", null, 4)
      )
      assert(tokens === expectedTokens)
    }

    it("should ignore block comments") {
      val scanner = new Scanner("""
        /**
         * A block comment
         */
        if (example) {
          return 1
        }"""
      )
      val tokens = scanner.scanTokens
      val expectedTokens = List(
        Token(TokenType.IF, "if", null, 5),
        Token(TokenType.LEFT_PAREN, "(",  null, 5),
        Token(TokenType.IDENTIFIER, "example", null, 5),
        Token(TokenType.RIGHT_PAREN, ")",  null, 5),
        Token(TokenType.LEFT_BRACE, "{",  null, 5),
        Token(TokenType.RETURN, "return", null, 6),
        Token(TokenType.NUMBER, "1",  1.0, 6),
        Token(TokenType.RIGHT_BRACE, "}",  null, 7),
        Token(TokenType.EOF, "", null, 7)
      )
      assert(tokens === expectedTokens)
    }

    it("should ignore nested block comments") {
      val scanner = new Scanner("""
        /**
         * A block comment
         * /** A nested block comment */
         */"""
      )
      val tokens = scanner.scanTokens
      val expectedTokens = List(
        Token(TokenType.EOF, "", null, 5)
      )
      assert(tokens === expectedTokens)
    }
  }
}
