package lox

import scala.util.CommandLineParser

@main def main(args: String*) =
  val lox = new Lox
  println(args)
  args.toList match
    case head::Nil =>
      lox.runFile(head)
    case Nil =>
      lox.runPrompt()
    case _ =>
      println("Usage: slox [script]")
      sys.exit(64)
