package org.deusaquilus.part1.newtypes.approach5_typelevel

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

trait FromPrimitive[Primitive] {
  type MType <: Data.Basic[Primitive]
  def schema: Schema.Basic[Primitive]
  def fromPrimitive(value: Primitive): MType
}
object FromPrimitive {
  def apply[MorphirType <: Data.Basic[Primitive], Primitive](
    morphirSchema: Schema.Basic[Primitive],
    buildData: Primitive => MorphirType
  ): FromPrimitive[Primitive] =
    new FromPrimitive[Primitive] {
      override type MType = MorphirType
      override def schema = morphirSchema
      override def fromPrimitive(value: Primitive) = buildData(value)
    }
}

implicit val booleanFromPrimitive: FromPrimitive[Boolean] =
  FromPrimitive(Schema.Boolean, Data.Boolean(_))

implicit val intFromPrimitive: FromPrimitive[Int] =
  FromPrimitive(Schema.Int, Data.Int(_))

implicit val stringFromPrimitive: FromPrimitive[String] =
  FromPrimitive(Schema.String, Data.String(_))