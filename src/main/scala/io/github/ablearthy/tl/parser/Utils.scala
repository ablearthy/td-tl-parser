package io.github.ablearthy.tl.parser

import fastparse._
import NoWhitespace._

object Utils {

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def exclMark[_: P]: P[Boolean] = P("!".!.?).map(_.isDefined)
}
