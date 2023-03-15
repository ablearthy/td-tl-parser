package io.github.ablearthy.tl.parser

import fastparse._
import MultiLineWhitespace._

case class Schema(types: Vector[Definition], functions: Vector[Definition])

object Schema {
  private sealed trait InternalDefinitions
  private case class FunctionDefinitions(functions: Vector[Definition])
      extends InternalDefinitions
  private case class TypesDefinitions(types: Vector[Definition])
      extends InternalDefinitions

  private def functionsParser[_: P]: P[InternalDefinitions] =
    P("---" ~ "functions" ~ "---" ~ Definition.parser.rep).map(r =>
      FunctionDefinitions(r.toVector)
    )
  private def typesParser[_: P]: P[InternalDefinitions] =
    P("---" ~ "types" ~ "---" ~ Definition.parser.rep).map(r =>
      TypesDefinitions(r.toVector)
    )

  def parser[_: P]: P[Schema] =
    P(Definition.parser.rep ~ (functionsParser | typesParser).rep).map {
      case (fst, rest) =>
        val initialSchema =
          Schema(types = fst.toVector, functions = Vector.empty)
        rest.foldLeft(initialSchema) {
          case (Schema(oldTypes, oldFunctions), r) =>
            r match {
              case FunctionDefinitions(functions) =>
                Schema(oldTypes, oldFunctions ++ functions)
              case TypesDefinitions(types) =>
                Schema(oldTypes ++ types, oldFunctions)
            }
        }
    }
}
