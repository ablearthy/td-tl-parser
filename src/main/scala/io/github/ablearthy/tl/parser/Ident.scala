package io.github.ablearthy.tl.parser

import fastparse._

object Ident {
  def lcParser[_: P]: P[String] =
    P(
      CharIn("a-z").! ~~ CharsWhile(
        a => a.isLetter || a.isDigit || a == '_',
        0
      ).!
    ).map { case (fst, rest) =>
      fst + rest
    }

  def ucParser[_: P]: P[String] =
    P(
      CharIn("A-Z").! ~~ CharsWhile(
        a => a.isLetter || a.isDigit || a == '_',
        0
      ).!
    ).map { case (fst, rest) =>
      fst + rest
    }

  def namespaceParser[_: P]: P[String] = Ident.lcParser

  def varParser[_: P]: P[String] = Ident.lcParser | Ident.ucParser
}
