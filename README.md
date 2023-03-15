# td-tl-parser

`td-tl-parser` provides parser for [TL Language](https://core.telegram.org/mtproto/TL).
It's tested on [td_api.tl](https://github.com/tdlib/td/blob/93c42f6d7c1209937431469f80427d48907f1b8d/td/generate/scheme/td_api.tl)

## Structure

```scala
case class Schema(types: Vector[Definition], functions: Vector[Definition])

case class Definition(
                       comments: Vector[String],
                       constructorName: LcIdentFull,
                       optArgs: Vector[OptArgs],
                       args: Vector[Args],
                       resultType: ResultType
                     )

case class LcIdentFull(
                        namespace: Option[String],
                        ident: String,
                        id: Option[Int]
                      )

case class OptArgs(
                    vars: Vector[String],
                    exclMark: Boolean,
                    typeExpr: Term.Expr
                  )

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
}

case class ResultType(
                       ident: IdentWithNs,
                       hasAngleBrackets: Boolean,
                       terms: Vector[Term]
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
```

## Usage

```scala
import fastparse.parse
import io.github.ablearthy.tl.parser.schemaParser

val stream = new FileInputStream(file)
val result = parse(stream, schemaParser(_))
```