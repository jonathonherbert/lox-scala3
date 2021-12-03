package lox

class ProgramTest extends BaseTest {

  describe("a program") {
    it("should run a program with block scoping") {
      (new Lox()).run("""
        var a = "global a";
        var b = "global b";
        var c = "global c";
        {
          var a = "outer a";
          var b = "outer b";
          {
            var a = "inner a";
            print a;
            print b;
            print c;
          }
            print a;
            print b;
            print c;
        }
        print a;
        print b;
        print c;
      """)
    }
  }
}
