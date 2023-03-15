package io.github.ablearthy.tl.parser

object StringUtils {
  def hexToInt(s: String): Int = {
    s.toList.map("0123456789abcdef".indexOf(_)).reduceLeft(_ * 16 + _)
  }
}
