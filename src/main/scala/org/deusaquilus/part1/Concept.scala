package org.deusaquilus.part1

import org.deusaquilus.part1.Schema
import zio.Chunk

import scala.annotation.tailrec

sealed trait Schema {
  def getName: Option[String] =
    this match {
      case _: Schema.Basic[_] => None
      case _: Schema.Any.type => None
      case c: Schema.Record   => Some(c.name)
      case _: Schema.Struct   => None
      case c: Schema.Alias    => Some(c.name)
      case _: Schema.List     => None
      case _: Schema.Map      => None
      case _: Schema.Set      => None
      case _: Schema.Tuple    => None
      case _: Schema.Optional => None
      case _: Schema.Result   => None
      case c: Schema.Enum     => Some(c.name)
    }
}

object Schema {
  sealed trait Basic[+A] extends Schema

  /// Represents any schema but also means that you have no reasonable idea of the schema of the associated data
  case object Any extends Schema

  case object Boolean   extends Basic[scala.Boolean]
  case object Int       extends Basic[scala.Int]
  case object String    extends Basic[java.lang.String]
  case object Char      extends Basic[scala.Char]
  case object Unit      extends Basic[scala.Unit]
  case object Nothing   extends Basic[scala.Nothing]

  case class Record(name: java.lang.String, fields: scala.List[(java.lang.String, Schema)]) extends Schema {
    def toStruct: Schema.Struct = Schema.Struct(fields: _*)
  }
  object Record {
    def apply(name: java.lang.String, fields: (java.lang.String, Schema)*) = new Record(name, fields.toList)
  }

  case class Struct(fields: scala.List[(java.lang.String, Schema)]) extends Schema
  object Struct {
    def apply(fields: (java.lang.String, Schema)*) = new Struct(fields.toList)
  }

  case class Alias(name: java.lang.String, value: Schema) extends Schema

  case class List(elementType: Schema) extends Schema

  case class Map(keyType: Schema, valueType: Schema) extends Schema

  case class Set(elementType: Schema) extends Schema

  case class Tuple(values: scala.List[Schema]) extends Schema

  /**
   * We can only know if an optional-value is Some or None on the value-level, not the type-level because the parent-derivation stage does not know this information. This is
   * generally understood to be a standard practice. For example, using Scala 3 enums, the specific type of an enum element is not known, only the general coproduct type. For
   * example:
   * {{{
   * enum Customer:
   *   case Person
   *   case Robot
   *
   * // this will be implicitly typed as Customer
   * val c = Customer.Person
   * }}}
   * Coproduct types in other languages (e.g. Haskell) work similarly.
   */
  case class Optional(elementType: Schema) extends Schema

  case class Result(errType: Schema, okType: Schema) extends Schema

  /**
   * A discrimiated union type such as an ELM union (either with labels or not)
   *
   * Given an Elm Datatype that looks like this:
   * {{{
   * type MyUnion =
   *   = NoValue
   *   | IntValue x:Int
   *   | MultiValue x:Int y:String
   *   | MultiValueAnon Int String // no labels for the types
   * }}}
   *
   * Or a Scala 3 enum that looks like this:
   * {{{
   *   enum MyUnion:
   *     case NoValue
   *     case IntValue(x:Int)
   *     case MultiValue(x:Int, y:String)
   *     // case MultiValueAnon(Int, String) // cannot have un-labeled unions in Scala3
   * }}}
   *
   * The corresponding type-representation should look like this:
   * {{{
   * Enum(
   *   Case("NoValue", List()),
   *   Case("IntValue", List(Case.Field.Named("x", Schema.Int))),
   *   Case("MultiValue", List(Case.Field.Named("x", Schema.Int), Case.Field.Named("y", Schema.String)))
   *   Case("MultiValueAnon", List(Case.Field.Anon(Schema.Int), Case.Field.Anon(Schema.String)))
   * )
   * }}}
   *
   * On the value level this should look as follows
   * {{{
   *   // Given a type definition that looks like this (In Scala)
   *   val x: MyUnion = MyUnion.IntValue(123)
   *
   *   // It's data-level encoding should look like this
   *   Data.Case(
   *     value: Data.Int(123)
   *     case: Case("IntValue", List(Case.Field.Named("x", Schema.Int)))
   *     schema: Schema.Enum
   *   )
   * }}}
   */
  case class Enum(name: java.lang.String, cases: scala.List[Enum.Case]) extends Schema

  object Enum {
    def apply(name: java.lang.String, cases: Enum.Case*) =
      new Enum(name, cases.toList)

    case class Case(label: java.lang.String, fields: scala.List[(java.lang.String, Schema)])

    object Case {
      def apply(label: java.lang.String, fields: (java.lang.String, Schema)*) =
        new Case(label, fields.toList)
    }
  }
}
