package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.Data

import scala.reflect.ClassTag
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}

trait GenericSumBuilder[T] extends DataBuilder[T] {
  def build(value: T): Data =
    builder.run(value)
  def deriveWithTag(value: T)(implicit ct: ClassTag[T]): Data =
    builder.run(value, Some(ct.asInstanceOf[ClassTag[Any]]))
  def builder: SumComposer
}
object GenericSumBuilder {
  def make[T](sumBuilder: SumComposer) =
    new GenericSumBuilder[T] {
      val builder = sumBuilder
      // For generic-sum, most of the type-compuation logic lives inside of the builder
      def schema = sumBuilder.enumType
    }

  inline def gen[T]: GenericSumBuilder[T] =
    summonFrom { case m: Mirror.SumOf[T] => DataBuilder.buildSumFromMirror[T](m) }

}
