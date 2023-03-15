package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

case class Definition(
    comments: Vector[String],
    constructorName: LcIdentFull,
    optArgs: Vector[OptArgs],
    args: Vector[Args],
    resultType: ResultType
)

object Definition {
  private def commentParser[_: P]: P[String] = P(
    "//" ~~ CharsWhile(_ != '\n').!
  )

  def parser[_: P]: P[Definition] = P(
    commentParser.rep ~ LcIdentFull.parser ~ OptArgs.parser.rep ~ Args.parser.rep ~ "=" ~/ ResultType.parser ~ ";"
  ).map { case (value, full, optArgs, args, resultType) =>
    Definition(
      comments = value.toVector,
      constructorName = full,
      optArgs = optArgs.toVector,
      args = args.toVector,
      resultType = resultType
    )
  }
}
