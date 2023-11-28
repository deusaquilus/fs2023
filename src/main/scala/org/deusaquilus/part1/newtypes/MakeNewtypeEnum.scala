package org.deusaquilus.part1.newtypes

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

case class MakeNewtypeEnum(label: String, innerSchema: Schema) {
  def construct(value: Data) =
    Data.Enum("value" -> value)(label, this.schema)

  def schema =
    Schema.Enum(label, Schema.Enum.Case(label, "value" -> innerSchema))
}
