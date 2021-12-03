package lox

import java.io.PrintWriter
import java.io.File

object ASTGenerator {
  def main(args: Array[String]) = {
    if (args.headOption.isEmpty || args.size > 1) {
      println("Usage: scala AstGenerator.scala <output_directory>")
      System.exit(1)
    }

    val directory = args(0)
    val defineAst = getAstDefiner(directory)

    defineAst("Expr", List(
      "ExprList   :: left: Expr, right: Option[ExprList] = None",
      "Assign     :: name: Token, value: Expr",
      "Binary     :: left: Expr, operator: Token, right: Expr",
      "Grouping   :: expression: Expr",
      "Literal    :: value: String | Double | Boolean",
      "Logical    :: left: Expr, operator: Token, right: Expr",
      "Variable   :: name: Token",
      "Unary      :: operator: Token, right: Expr"
    ))

    defineAst("Stmt", List(
      "Expression :: expr: Expr",
      "Print      :: expr: Expr",
      "VarDecl    :: name: Token, value: Expr",
      "Block      :: statements: List[Stmt]",
      "IfStmt     :: expr: Expr, thenStmt: Stmt, elseStmt: Option[Stmt]"
    ))
  }

  def getAstDefiner = (directory: String) => (baseName: String, types: List[String]) => {
    val expressions = types.flatMap { typeExpr =>
      val className = typeExpr.split("::").head.trim
      val fields = typeExpr.split("::")(1).trim
      getTypeDefinition(baseName, className, fields)
    }

    val lines = getFileHeader(baseName, expressions)

    val writer = new PrintWriter(new File(s"$directory/$baseName.scala"))
    lines.foreach(line => writer.print(line + "\n"))
    writer.flush()
    writer.close()
  }

  def getFileHeader(baseName: String, content: List[String]): List[String] = List(
    "package lox.grammar",
    "",
    "import lox.Token",
    "",
    s"trait ${baseName}",
    ""
  ) ++ content

  def getTypeDefinition(baseName: String, className: String, fields: String): List[String] = List(
    s"case class ${className}($fields) extends $baseName"
  )
}
