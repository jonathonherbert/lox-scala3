package lox

import scala.util.CommandLineParser

@main def main(args: String*) =
  val lox = new Lox
  args.toList match
    case head::Nil =>
      if (head == "print")
        AstPrinter.main()
      else
        lox.runFile(head)
    case Nil =>
      lox.runPrompt()
    case _ =>
      println("Usage: slox [script]")
      sys.exit(64)
