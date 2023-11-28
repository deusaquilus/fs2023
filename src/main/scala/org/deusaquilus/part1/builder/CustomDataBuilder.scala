package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

trait CustomDataBuilder[T] extends DataBuilder[T] {
  def build(value: T): Data
  def schema: Schema

  def contramap[R](f: R => T): CustomDataBuilder[R] =
    CustomDataBuilder.ofType(b => this.build(f(b)), this.schema)

  def contramapWithSchema[R](schema0: Schema)(f: R => T): CustomDataBuilder[R] =
    CustomDataBuilder.ofType(b => this.build(f(b)), schema0)
}

object CustomDataBuilder {
  def ofType[A](f: A => Data, schema0: Schema): CustomDataBuilder[A] =
    new CustomDataBuilder[A] {
      override def build(a: A): Data = f(a)
      override def schema: Schema   = schema0
    }
}
