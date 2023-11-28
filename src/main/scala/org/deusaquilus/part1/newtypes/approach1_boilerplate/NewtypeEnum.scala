package org.deusaquilus.part1.newtypes.approach1_boilerplate

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}
import org.deusaquilus.part1.newtypes.*
import org.deusaquilus.part1.builder.*
import org.deusaquilus.part1.builder.BuilderMacros.typeName

import scala.annotation.targetName

object NewtypeEnum {
  @targetName("buildBoolean")
  inline def build[NewType](extractValue: NewType => Boolean): CustomDataBuilder[NewType] =
    new CustomDataBuilder[NewType] {
      val wrapper                                    =
        MakeNewtypeEnum(typeName[NewType], Schema.Boolean)
      override def build(value: NewType): Data.Enum =
        wrapper.construct(Data.Boolean(extractValue(value)))
      override def schema: Schema                  =
        wrapper.schema
    }

  @targetName("buildInt")
  inline def build[NewType](extractValue: NewType => Int): CustomDataBuilder[NewType] =
    new CustomDataBuilder[NewType] {
      val wrapper                                    =
        MakeNewtypeEnum(typeName[NewType], Schema.Int)
      override def build(value: NewType): Data.Enum =
        wrapper.construct(Data.Int(extractValue(value)))
      override def schema: Schema                  =
        wrapper.schema
    }

  // 20 linebreaks

  @targetName("buildString")
  inline def build[NewType](extractValue: NewType => String): CustomDataBuilder[NewType] =
    new CustomDataBuilder[NewType] {
      val wrapper                                    = MakeNewtypeEnum(typeName[NewType], Schema.String)
      override def build(value: NewType): Data.Enum = wrapper.construct(Data.String(extractValue(value)))
      override def schema: Schema                  = wrapper.schema
    }
}
