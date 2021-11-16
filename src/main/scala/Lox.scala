package lox

import io.StdIn.readLine
import scala.io.Source
import scala.util.Success
import scala.util.Failure

class Lox:
  def runFile(fileName: String) =
    run(Source.fromFile(fileName).getLines.mkString("\n"))

  def runPrompt() =
    Iterator.continually(readLine)
        .takeWhile(_ != "exit")
        .foreach(run)

  def run(program: String) =
    val scanner = new Scanner(program)
    val tokens = scanner.scanTokens
    val parser = new Parser(tokens)
    parser.parse() match {
      case Success(expr) =>
        println(s"Parsed to ${AstPrinter.programToString(expr)}")
        // Interpreter.evaluate(expr)
      case Failure(e) => println(e.getMessage)
    }
