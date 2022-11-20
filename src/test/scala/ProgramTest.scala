package lox

class ProgramTest extends BaseTest {

  describe("a program") {
    val lox = new Lox()
    it("should run a program with block scoping") {
      val result = lox.run("""
        var a = 1;
        for (var b = 1; b < 3; b = b + 1) print(b);
      """)

      assert(result === List("2.0", "3.0"))
    }
  }
}
