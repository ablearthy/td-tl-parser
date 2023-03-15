package io.github.ablearthy.tl.parser

import fastparse._

sealed trait TypeIdent

case class SimpleTypeIdent(s: IdentWithNs) extends TypeIdent
case object HashTypeIdent extends TypeIdent

object TypeIdent {
  def parser[_: P]: P[TypeIdent] = hashParser | simpleParser

  private def hashParser[_: P]: P[TypeIdent] = P("#".!).map(_ => HashTypeIdent)
  private def simpleParser[_: P]: P[TypeIdent] =
    (IdentWithNs.lcIdentNsParser | IdentWithNs.ucIdentNsParser).map(i =>
      SimpleTypeIdent(i)
    )
}
