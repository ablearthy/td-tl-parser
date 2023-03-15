package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

case class OptArgs(
    vars: Vector[String],
    exclMark: Boolean,
    typeExpr: Term.Expr
)

object OptArgs {
  def parser[_: P]: P[OptArgs] =
    P(
      "{" ~/ Ident.varParser.rep(1) ~/ ":" ~ Utils.exclMark ~/ Term.parser.rep(
        1
      ) ~/ "}"
    ).map { case (idents, maybeExcl, terms) =>
      OptArgs(
        vars = idents.toVector,
        exclMark = maybeExcl,
        typeExpr = terms.toVector
      )
    }
}
