package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

sealed trait Term {
  val bare: Boolean
}

case class BracketTerm(bare: Boolean, t: Term.Expr) extends Term
case class SimpleTerm(bare: Boolean, s: TypeIdent) extends Term
case class AngleBracketTerm(
    bare: Boolean,
    typeIdent: TypeIdent,
    rest: Term.Expr
) extends Term

object Term {
  type Expr = Vector[Term]
  def parser[_: P]: P[Term] = bracketTerm | angleBracketTerm | simpleTerm

  private def bracketTerm[_: P]: P[BracketTerm] =
    P("%".!.? ~~ "(" ~ Term.parser.rep(1) ~ ")").map { case (bare, terms) =>
      BracketTerm(bare = bare.isDefined, t = terms.toVector)
    }

  private def simpleTerm[_: P]: P[SimpleTerm] =
    P("%".!.? ~~ TypeIdent.parser).map { case (bare, x) =>
      SimpleTerm(bare = bare.isDefined, s = x)
    }

  private def angleBracketTerm[_: P]: P[AngleBracketTerm] = P(
    "%".!.? ~~ TypeIdent.parser ~ "<" ~/ Term.parser.rep(
      min = 1,
      sep = ","./
    ) ~/ ">"
  ).map { case (bare, typeIdent, expressions) =>
    AngleBracketTerm(
      bare = bare.isDefined,
      typeIdent = typeIdent,
      rest = expressions.toVector
    )
  }
}
