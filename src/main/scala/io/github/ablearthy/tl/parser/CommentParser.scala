package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

object CommentParser {
  def parser[_: P]: P[Seq[(String, String)]] = unit.rep

  private def unit[_: P] = P(
    ("@" ~~ CharsWhile(!_.isWhitespace).!) ~ CharsWhile(_ != '@').!
  )
}
