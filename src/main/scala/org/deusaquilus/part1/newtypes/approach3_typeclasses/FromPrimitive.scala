package org.deusaquilus.part1.newtypes.approach3_typeclasses

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

trait FromPrimitive[MorphirType <: Data.Basic[Primitive], Primitive] {
  def schema: Schema.Basic[Primitive]
  def fromPrimitive(value: Primitive): MorphirType
}

implicit val booleanFromPrimitive: FromPrimitive[Data.Boolean, Boolean] =
  new FromPrimitive[Data.Boolean, Boolean] {
    override def schema = Schema.Boolean
    override def fromPrimitive(value: Boolean) = Data.Boolean(value)
  }

implicit val intFromPrimitive: FromPrimitive[Data.Int, Int] =
  new FromPrimitive[Data.Int, Int] {
    override def schema = Schema.Int
    override def fromPrimitive(value: Int) = Data.Int(value)
  }

// 10 linebreaks

implicit val stringFromPrimitive: FromPrimitive[Data.String, String] =
  new FromPrimitive[Data.String, String] {
    override def schema = Schema.String
    override def fromPrimitive(value: String) = Data.String(value)
  }