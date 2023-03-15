package io.github.ablearthy.tl

import fastparse._

package object parser {
  def schemaParser[_: P]: P[Schema] = Schema.parser
}
