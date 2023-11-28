package org.deusaquilus.part1.newtypes.approach2_typeclasses

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}
import org.deusaquilus.part1.builder.*
import org.deusaquilus.part1.builder.BuilderMacros.typeName
import org.deusaquilus.part1.newtypes.*

import scala.annotation.targetName

object NewtypeEnum {
  inline def derive[NewType, MorphirType <: Data.Basic[Primitive], Primitive](
    extractValue: NewType => Primitive
  )(implicit repr: FromPrimitive[MorphirType, Primitive]): CustomDataBuilder[NewType] =
    new CustomDataBuilder[NewType] {
      val wrapper =
        MakeNewtypeEnum(typeName[NewType], repr.schema)
      override def build(value: NewType): Data.Enum =
        wrapper.construct(repr.fromPrimitive(extractValue(value)))
      override def schema: Schema = wrapper.schema
    }
}
