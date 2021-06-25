package lox

import io.StdIn.readLine
import scala.io.Source

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
    println(tokens.map(_.toString).mkString(",\n"))


