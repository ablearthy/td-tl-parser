package io.github.ablearthy.tl.parser

import fastparse._

case class LcIdentFull(
    namespace: Option[String],
    ident: String,
    id: Option[Int]
)

object LcIdentFull {
  private def combinatorIdParser[_: P]: P[Int] =
    P("#" ~~ CharIn("0-9a-f").repX(exactly = 8).!).map(StringUtils.hexToInt)

  def parser[_: P]: P[LcIdentFull] =
    P(IdentWithNs.lcIdentNsParser ~~ combinatorIdParser.?).map {
      case (IdentWithNs(ns, name), id) => LcIdentFull(ns, name, id)
    }
}
