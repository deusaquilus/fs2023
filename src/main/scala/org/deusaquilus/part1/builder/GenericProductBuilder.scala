package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}

trait GenericProductBuilder[T <: Product] extends DataBuilder[T] {
  def build(value: T): Data = builder.run(value)
  def builder: ProductComposer.MirrorProduct
}

object GenericProductBuilder {
  def make[T <: Product](productBuilder: ProductComposer.MirrorProduct): GenericProductBuilder[T] =
    new GenericProductBuilder[T] {
      val builder = productBuilder
      val schema: Schema.Record = {
        // Builder stage contains list of fields and child builders
        val fields: List[(String, Schema)] =
          productBuilder.fields.map {
            case ProductComposer.Leaf(field, _, builder) =>
              (field, builder.schema)
            case ProductComposer.Product(field, _, builder) =>
              (field, builder.schema)
            case ProductComposer.Sum(field, index, builder) =>
              (field, builder.schema)
          }
        Schema.Record(productBuilder.name, fields)
      }
    }

  /**
   * Automatic generator for Product types (and only product types). For anything that is automatically evaluated by the Scala compiler as a implicit (e.g. auto-derivation) we need
   * to be really careful for that not to evaulate for Products and Primitives (and/or Sums) otherwise there is a danger that it will recurse infinately on certain datatypes that
   * are not well-formed. Therefore for products we have a single function that handles derivation only for products and the implicit needed for that (in the Builders). This is
   * needed for the following purpose.
   *
   * Say that we have a simple case-class hierarchy like this {{ case class Person(name: Name, age: String) case class Name(first: String, last: String)
   *
   * }}
   */
  inline def gen[T <: Product]: GenericProductBuilder[T] =
    summonFrom { case m: Mirror.ProductOf[T] => DataBuilder.buildProductFromMirror[T](m) }
}
