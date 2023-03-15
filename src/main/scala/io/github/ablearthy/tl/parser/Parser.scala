package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

case class IdentWithNs(
    namespace: Option[String],
    ident: String
)

case class LcIdentFull(
    namespace: Option[String],
    ident: String,
    id: Option[Int]
)

object StringUtils {
  def hexToInt(s: String): Int = {
    s.toList.map("0123456789abcdef".indexOf(_)).reduceLeft(_ * 16 + _)
  }
}

object Utils {
  def lcIdent[_: P]: P[String] =
    P(
      CharIn("a-z").! ~~ CharsWhile(
        a => a.isLetter || a.isDigit || a == '_',
        0
      ).!
    ).map { case (fst, rest) =>
      fst + rest
    }

  def ucIdent[_: P]: P[String] =
    P(
      CharIn("A-Z").! ~~ CharsWhile(
        a => a.isLetter || a.isDigit || a == '_',
        0
      ).!
    ).map { case (fst, rest) =>
      fst + rest
    }

  def namespaceIdent[_: P]: P[String] = Utils.lcIdent

  private def identNsWith[_: P](x: => P[String]): P[IdentWithNs] =
    P((Utils.namespaceIdent ~~ CharIn(".")).? ~~ x).map { case (a, b) =>
      IdentWithNs(a, b)
    }

  def lcIdentNs[_: P]: P[IdentWithNs] = Utils.identNsWith(Utils.lcIdent)

  def ucIdentNs[_: P]: P[IdentWithNs] = Utils.identNsWith(Utils.ucIdent)

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def varIdent[_: P]: P[String] = Utils.lcIdent | Utils.ucIdent

  def varIdentOpt[_: P]: P[VarIdentOpt] = Utils.varIdent.map {
    case "_" => VarIdentOpt(None)
    case s   => VarIdentOpt(Some(s))
  }

  def exclMark[_: P]: P[Boolean] = P("!".!.?).map(_.isDefined)
}

sealed trait TypeIdent

case class SimpleTypeIdent(s: IdentWithNs) extends TypeIdent
case object HashTypeIdent extends TypeIdent

object TypeIdentParser {
  def parse[_: P]: P[TypeIdent] = parseHash | parseSimple

  private def parseHash[_: P]: P[TypeIdent] = P("#".!).map(_ => HashTypeIdent)
  private def parseSimple[_: P]: P[TypeIdent] =
    (Utils.lcIdentNs | Utils.ucIdentNs).map(i => SimpleTypeIdent(i))
}

case class Definition(
    comments: Vector[String],
    constructorName: LcIdentFull,
    optArgs: Vector[OptArgs],
    args: Vector[Args],
    resultType: ResultType
)

case class VarIdentOpt(internal: Option[String]) extends AnyVal

case class ConditionalDef(ident: String, index: Option[Int])

sealed trait Args

case class CondArg(
    ident: Option[String],
    cond: Option[ConditionalDef],
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

object ArgsParser {
  def args[_: P]: P[Args] = bracketArg | arrayArg | condArg | anonymousArg

  private def conditionalDef[_: P]: P[ConditionalDef] =
    P(Utils.varIdent ~ ("." ~~ Utils.number).? ~ "?").map { case (u, v) =>
      ConditionalDef(ident = u, index = v)
    }

  private def condArg[_: P]: P[Args] =
    P(Utils.varIdentOpt ~ ":" ~ conditionalDef.? ~ Utils.exclMark ~ Term.apply)
      .map { case (opt, maybeDef, exclMark, term) =>
        CondArg(
          ident = opt.internal,
          cond = maybeDef,
          exclMark = exclMark,
          typeTerm = term
        )
      }

  private def arrayArg[_: P]: P[Args] = P(
    (Utils.varIdentOpt ~ ":").? ~ (Utils.number ~ "*").? ~ "[" ~/ args.rep ~ "]"
  ).map { case (maybeOpt, maybeInt, value) =>
    ArrayArg(
      ident = maybeOpt.flatMap(_.internal),
      multiplicity = maybeInt,
      args = value.toVector
    )
  }

  private def bracketArg[_: P]: P[Args] = P(
    "(" ~/ Utils.varIdentOpt.rep ~ ":" ~ Utils.exclMark ~ Term.apply ~ ")"
  ).map { case (idents, exclMark, term) =>
    BracketArg(
      ident = idents.map(_.internal).toVector,
      exclMark = exclMark,
      typeTerm = term
    )
  }

  private def anonymousArg[_: P]: P[Args] = P(Utils.exclMark ~ Term.apply).map {
    case (bool, term) =>
      AnonymousArg(bool, term)
  }
}

case class OptArgs(
    vars: Vector[String],
    exclMark: Boolean,
    typeExpr: Term.Expr
)

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
  def apply[_: P]: P[Term] = bracketTerm | angleBracketTerm | simpleTerm

  private def bracketTerm[_: P]: P[BracketTerm] =
    P("%".!.? ~~ "(" ~ Term.apply.rep(1) ~ ")").map { case (bare, terms) =>
      BracketTerm(bare = bare.isDefined, t = terms.toVector)
    }

  private def simpleTerm[_: P]: P[SimpleTerm] =
    P("%".!.? ~~ TypeIdentParser.parse).map { case (bare, x) =>
      SimpleTerm(bare = bare.isDefined, s = x)
    }

  private def angleBracketTerm[_: P]: P[AngleBracketTerm] = P(
    "%".!.? ~~ TypeIdentParser.parse ~ "<" ~/ apply.rep(
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

case class ResultType(
    ident: IdentWithNs,
    hasAngleBrackets: Boolean,
    terms: Vector[Term]
)

case class Schema(types: Vector[Definition], functions: Vector[Definition])

object ResultType {

  def apply[_: P]: P[ResultType] =
    P(Utils.ucIdentNs ~ (parseBrackets | parseNoBrackets)).map {
      case (ns, (has, terms)) => ResultType(ns, has, terms)
    }

  private def parseBrackets[_: P] =
    P("<" ~/ Term.apply.rep(1, ",") ~ ">").map(terms => (true, terms.toVector))

  private def parseNoBrackets[_: P] =
    Term.apply.rep.map(terms => (false, terms.toVector))
}

object Parser {

  private def parseOneLineComment[_: P] = P("//" ~~ CharsWhile(_ != '\n').!)

  private def combinatorId[_: P] =
    P("#" ~~ CharIn("0-9a-f").repX(exactly = 8).!).map(StringUtils.hexToInt)

  private def fullCombinator[_: P] = P(Utils.lcIdentNs ~~ combinatorId.?).map {
    case (IdentWithNs(ns, name), id) => LcIdentFull(ns, name, id)
  }

  private def optArgs[_: P] =
    P(
      "{" ~/ Utils.varIdent.rep(1) ~/ ":" ~ Utils.exclMark ~/ Term.apply.rep(
        1
      ) ~/ "}"
    ).map { case (idents, maybeExcl, terms) =>
      OptArgs(
        vars = idents.toVector,
        exclMark = maybeExcl,
        typeExpr = terms.toVector
      )
    }

  private def definition[_: P] = P(
    parseOneLineComment.rep ~ fullCombinator ~ optArgs.rep ~ ArgsParser.args.rep ~ "=" ~/ ResultType.apply ~ ";"
  ).map { case (value, full, optArgs, args, resultType) =>
    Definition(
      comments = value.toVector,
      constructorName = full,
      optArgs = optArgs.toVector,
      args = args.toVector,
      resultType = resultType
    )
  }

  private def funcs[_: P] =
    P("---" ~ "functions" ~ "---" ~ definition.rep).map(defs =>
      (true, defs.toVector)
    )
  private def types[_: P] =
    P("---" ~ "types" ~ "---" ~ definition.rep).map(defs =>
      (false, defs.toVector)
    )

  def schema[_: P]: P[Schema] = P(definition.rep ~ (funcs | types).rep).map {
    case (fst, rest) =>
      rest.foldLeft(Schema(types = fst.toVector, functions = Vector.empty)) {
        case (Schema(types, funcs), (isFuncs, defs)) =>
          if (isFuncs) {
            Schema(types, funcs ++ defs)
          } else {
            Schema(types ++ defs, funcs)
          }
      }
  }
}
