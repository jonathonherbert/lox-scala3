package lox

class ProgramTest extends BaseTest {

  describe("a program") {
    it("should run a program with block scoping") {
      (new Lox()).run("""
        var a = 1;
        for (var b = 1; b < 10; b = b + 1) print(b);
        print("done");
      """)
    }
  }
}
