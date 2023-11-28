package org.deusaquilus.part1.newtypes.approach4_typeclasses

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

trait FromPrimitive[MorphirType <: Data.Basic[Primitive], Primitive] {
  def schema: Schema.Basic[Primitive]
  def fromPrimitive(value: Primitive): MorphirType
}
object FromPrimitive {
  def apply[MorphirType <: Data.Basic[Primitive], Primitive](
    morphirSchema: Schema.Basic[Primitive],
    buildData: Primitive => MorphirType
  ): FromPrimitive[MorphirType, Primitive] =
    new FromPrimitive[MorphirType, Primitive] {
      override def schema = morphirSchema
      override def fromPrimitive(value: Primitive) = buildData(value)
    }
}

implicit val booleanFromPrimitive: FromPrimitive[Data.Boolean, Boolean] =
  FromPrimitive[Data.Boolean, Boolean](Schema.Boolean, Data.Boolean(_))

implicit val intFromPrimitive: FromPrimitive[Data.Int, Int] =
  FromPrimitive[Data.Int, Int](Schema.Int, Data.Int(_))

// 4 linebreaks

implicit val stringFromPrimitive: FromPrimitive[Data.String, String] =
  FromPrimitive[Data.String, String](Schema.String, Data.String(_))