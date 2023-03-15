package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

case class ResultType(
    ident: IdentWithNs,
    hasAngleBrackets: Boolean,
    terms: Vector[Term]
)

object ResultType {
  def parser[_: P]: P[ResultType] =
    P(IdentWithNs.ucIdentNsParser ~ (parseBrackets | parseNoBrackets)).map {
      case (ns, (has, terms)) => ResultType(ns, has, terms)
    }

  private def parseBrackets[_: P] =
    P("<" ~/ Term.parser.rep(1, ",") ~ ">").map(terms => (true, terms.toVector))

  private def parseNoBrackets[_: P] =
    Term.parser.rep.map(terms => (false, terms.toVector))
}
