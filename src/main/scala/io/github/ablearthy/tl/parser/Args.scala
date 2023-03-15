package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._
sealed trait Args

case class CondArg(
    ident: Option[String],
    cond: Option[Args.ConditionalDef],
    exclMark: Boolean,
    typeTerm: Term
) extends Args

case class ArrayArg(
    ident: Option[String],
    multiplicity: Option[Int],
    args: Vector[Args]
) extends Args

case class BracketArg(
    ident: Vector[Option[String]],
    exclMark: Boolean,
    typeTerm: Term
) extends Args

case class AnonymousArg(exclMark: Boolean, typeTerm: Term) extends Args

object Args {
  case class ConditionalDef(ident: String, index: Option[Int])

  def parser[_: P]: P[Args] =
    bracketArgParser | arrayArgParser | condArgParser | anonymousArgParser

  private def conditionalDefParser[_: P]: P[ConditionalDef] =
    P(Ident.varParser ~ ("." ~~ Utils.number).? ~ "?").map { case (u, v) =>
      ConditionalDef(ident = u, index = v)
    }

  private def condArgParser[_: P]: P[Args] =
    P(
      VarIdentOpt.parser ~ ":" ~ conditionalDefParser.? ~ Utils.exclMark ~ Term.parser
    )
      .map { case (opt, maybeDef, exclMark, term) =>
        CondArg(
          ident = opt.internal,
          cond = maybeDef,
          exclMark = exclMark,
          typeTerm = term
        )
      }

  private def arrayArgParser[_: P]: P[Args] = P(
    (VarIdentOpt.parser ~ ":").? ~ (Utils.number ~ "*").? ~ "[" ~/ parser.rep ~ "]"
  ).map { case (maybeOpt, maybeInt, value) =>
    ArrayArg(
      ident = maybeOpt.flatMap(_.internal),
      multiplicity = maybeInt,
      args = value.toVector
    )
  }

  private def bracketArgParser[_: P]: P[Args] = P(
    "(" ~/ VarIdentOpt.parser.rep ~ ":" ~ Utils.exclMark ~ Term.parser ~ ")"
  ).map { case (idents, exclMark, term) =>
    BracketArg(
      ident = idents.map(_.internal).toVector,
      exclMark = exclMark,
      typeTerm = term
    )
  }

  private def anonymousArgParser[_: P]: P[Args] =
    P(Utils.exclMark ~ Term.parser).map { case (bool, term) =>
      AnonymousArg(bool, term)
    }
}
