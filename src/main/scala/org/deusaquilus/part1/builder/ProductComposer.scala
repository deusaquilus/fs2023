package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.Data

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}

private[builder] sealed trait ProductComposer

private[builder] sealed trait ProductComposerField extends ProductComposer {
  def run(parent: scala.Product): Data
  def field: String
}
private[builder] object ProductComposer {
  case class Leaf(field: String, index: Int, builder: CustomDataBuilder[Any]) extends ProductComposerField {
    def run(parent: scala.Product) = builder.build(parent.productElement(index))
  }

  private def fail(derived: Any, index: Int) =
    throw new IllegalArgumentException(
      s"The derived output element ${derived} at index: $index was not a product-type, ${derived.getClass()}"
    )

  case class Product(field: String, index: Int, builder: GenericProductBuilder[scala.Product])
      extends ProductComposerField {
    def run(parent: scala.Product) =
      builder.build {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }
  case class Sum(field: String, index: Int, builder: GenericSumBuilder[Any]) extends ProductComposerField {
    def run(parent: scala.Product) =
      builder.build {
        val derived = parent.productElement(index)
        derived match {
          case p: scala.Product => p
          case _                => fail(derived, index)
        }
      }
  }

  case class MirrorProduct(name: String, fields: List[ProductComposerField]) extends ProductComposer {
    def run(parent: scala.Product) =
      Data.Record(name, fields.map(f => (f.field, f.run(parent))))
  }
}
