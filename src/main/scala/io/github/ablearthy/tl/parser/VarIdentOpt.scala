package io.github.ablearthy.tl.parser

import fastparse._
case class VarIdentOpt(internal: Option[String]) extends AnyVal

object VarIdentOpt {
  def parser[_: P]: P[VarIdentOpt] = Ident.varParser.map {
    case "_" => VarIdentOpt(None)
    case s   => VarIdentOpt(Some(s))
  }
}
