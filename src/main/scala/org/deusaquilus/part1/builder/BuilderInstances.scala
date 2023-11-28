package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

import scala.BigInt
import scala.BigDecimal
import java.time.{LocalDate, LocalTime, Month}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

trait BuilderInstances {

  implicit val booleanBuilder: CustomDataBuilder[Boolean] = new CustomDataBuilder[Boolean] {
    def build(value: Boolean) = Data.Boolean(value)
    def schema                = Schema.Boolean
  }

  implicit val intBuilder: CustomDataBuilder[Int] = new CustomDataBuilder[Int] {
    def build(value: Int) = Data.Int(value)
    def schema            = Schema.Int
  }

  implicit val stringBuilder: CustomDataBuilder[String] = new CustomDataBuilder[String] {
    def build(value: String) = Data.String(value)
    def schema               = Schema.String
  }

  implicit val charBuilder: CustomDataBuilder[Char] = new CustomDataBuilder[Char] {
    def build(value: Char) = Data.Char(value)
    def schema             = Schema.Char
  }

  implicit val unitBuilder: CustomDataBuilder[Unit] = new CustomDataBuilder[Unit] {
    def build(value: Unit) = Data.Unit
    def schema             = Schema.Unit
  }

  implicit def eitherBuilder[L, R](implicit
      leftBuilder: DataBuilder[L],
      rightBuilder: DataBuilder[R]
  ): CustomDataBuilder[Either[L, R]] =
    new CustomDataBuilder[Either[L, R]] {
      def build(value: Either[L, R]) =
        value match {
          case Right(value) => Data.Result.Ok(rightBuilder.build(value), this.schema)
          case Left(value)  => Data.Result.Err(leftBuilder.build(value), this.schema)
        }
      def schema: Schema.Result = Schema.Result(leftBuilder.schema, rightBuilder.schema)
    }

  implicit def rightBuilder[L, R](implicit
      leftBuilder: DataBuilder[L],
      rightBuilder: DataBuilder[R]
  ): CustomDataBuilder[Right[L, R]] = eitherBuilder[L, R](leftBuilder, rightBuilder).contramap(v => v)

  implicit def leftBuilder[L, R](implicit
      leftBuilder: DataBuilder[L],
      rightBuilder: DataBuilder[R]
  ): CustomDataBuilder[Left[L, R]] = eitherBuilder[L, R](leftBuilder, rightBuilder).contramap(v => v)

  implicit def leftBuilder[L](implicit leftBuilder: DataBuilder[L]): CustomDataBuilder[Left[L, Nothing]] =
    new CustomDataBuilder[Left[L, Nothing]] {
      def build(value: Left[L, Nothing]) = Data.Result.Err(leftBuilder.build(value.value), this.schema)
      def schema: Schema.Result         = Schema.Result(leftBuilder.schema, Schema.Nothing)
    }

  implicit def optionBuilder[T](implicit elementBuilder: DataBuilder[T]): CustomDataBuilder[Option[T]] =
    new CustomDataBuilder[Option[T]] {
      def build(value: Option[T]) =
        value match {
          case Some(value) => Data.Optional.Some(elementBuilder.build(value), elementBuilder.schema)
          case None        => Data.Optional.None(elementBuilder.schema)
        }
      def schema = Schema.Optional(elementBuilder.schema)
    }

  implicit def optionSomeBuilder[T](implicit elementBuilder: DataBuilder[T]): CustomDataBuilder[Some[T]] =
    new CustomDataBuilder[Some[T]] {
      def build(value: Some[T]) = Data.Optional.Some(elementBuilder.build(value.value), elementBuilder.schema)
      def schema                = Schema.Optional(elementBuilder.schema)
    }

  implicit val optionNoneBuilder: CustomDataBuilder[scala.None.type] = new CustomDataBuilder[scala.None.type] {
    def build(value: scala.None.type) = Data.Optional.None(Schema.Nothing)
    def schema                        = Schema.Optional(Schema.Nothing)
  }

  implicit def listBuilder[T](implicit elementBuilder: DataBuilder[T]): CustomDataBuilder[List[T]] =
    new CustomDataBuilder[List[T]] {
      def build(value: scala.List[T]) = {
        def toData(value: T) = elementBuilder.build(value)
        // Take the schema from the elementBuilder instead of the list elements
        // because even if the elements themeselves have more specific schemas than the derver schema,
        // the builder schema has a generalization of the elements whose type is the only valid
        // type for the whole list.
        Data.List(value.map(toData(_)), elementBuilder.schema)
      }

      def schema: Schema.List = Schema.List(elementBuilder.schema)
    }

  /*
   * Since we want to have the option to use either ordered or unordered maps in the DDL,
   * builders for ordered and non-ordered map variants have been provided. In particular
   * since the Scala ListMap is problematic in many ways (e.g. lookup time is O(n)) the
   * baseline implementation for the builder uses scala's LinkedHashMap. Since
   * this datastructure is mutable, we make a copy of it during the derivation process
   * so that changes to it will not cause changes to the underlying Data object.
   */

  implicit def linkedMapBuilder[K, V](implicit
      keyBuilder: DataBuilder[K],
      valueBuilder: DataBuilder[V]
  ): CustomDataBuilder[mutable.LinkedHashMap[K, V]] =
    new CustomDataBuilder[LinkedHashMap[K, V]] {
      def build(value: LinkedHashMap[K, V]) = {
        def toData(value: (K, V)) = (keyBuilder.build(value._1), valueBuilder.build(value._2))
        Data.Map.copyFrom(value.map(toData(_)), Schema.Map(keyBuilder.schema, valueBuilder.schema))
      }

      def schema: Schema.Map = Schema.Map(keyBuilder.schema, valueBuilder.schema)
    }

  implicit def listMapBuilder[K, V](implicit
      keyBuilder: DataBuilder[K],
      valueBuilder: DataBuilder[V]
  ): CustomDataBuilder[ListMap[K, V]] =
    new CustomDataBuilder[ListMap[K, V]] {
      def build(value: ListMap[K, V]): Data = linkedMapBuilder[K, V].build(LinkedHashMap.from(value))
      def schema: Schema                   = linkedMapBuilder[K, V].schema
    }

  implicit def mapBuilder[K, V](implicit keyBuilder: DataBuilder[K], valueBuilder: DataBuilder[V]): CustomDataBuilder[Map[K, V]] =
    new CustomDataBuilder[Map[K, V]] {
      def build(value: Map[K, V]): Data = linkedMapBuilder[K, V].build(LinkedHashMap.from(value))
      def schema: Schema               = linkedMapBuilder[K, V].schema
    }

}
