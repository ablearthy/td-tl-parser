package io.github.ablearthy.tl.parser

import fastparse._

case class IdentWithNs(
    namespace: Option[String],
    ident: String
)

object IdentWithNs {

  private def identNsWith[_: P](x: => P[String]): P[IdentWithNs] =
    P((Ident.namespaceParser ~~ CharIn(".")).? ~~ x).map { case (a, b) =>
      IdentWithNs(a, b)
    }

  def lcIdentNsParser[_: P]: P[IdentWithNs] = identNsWith(Ident.lcParser)

  def ucIdentNsParser[_: P]: P[IdentWithNs] = identNsWith(Ident.ucParser)
}
