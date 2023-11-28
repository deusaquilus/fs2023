package org.deusaquilus.part1

import org.deusaquilus.part1.{Schema, Data}

import scala.collection.mutable

sealed trait Data {
  def schema: Schema

  def getName: Option[String] =
    this match {
      case _: Data.Basic[_] => None
      case v: Data.Enum     => Some(v.schema.name)
      case _: Data.Tuple    => None
      case v: Data.Record   => Some(v.schema.name)
      case _: Data.Struct   => None
      case _: Data.Optional => None
      case _: Data.Result   => None
      case _: Data.List     => None
      case _: Data.Map      => None
      case _: Data.Set      => None
      case v: Data.Aliased  => Some(v.schema.name)
    }
}

object Data {
  val True: Data  = Boolean(true)
  val False: Data = Boolean(false)

  sealed trait Basic[+A]                   extends Data
  case class Boolean(value: scala.Boolean) extends Basic[scala.Boolean] { val schema = Schema.Boolean }
  case class Int(value: scala.Int)               extends Basic[scala.Int]                 { val schema = Schema.Int     }
  case class Char(value: scala.Char)               extends Basic[scala.Char]          { val schema = Schema.Char      }
  case class String(value: java.lang.String)               extends Basic[java.lang.String]          { val schema = Schema.String      }
  case object Unit                                 extends Basic[scala.Unit]          { val schema = Schema.Unit      }

  // Needed for Scala 3 extension methods to work
  object Boolean   {}
  object Int     {}
  object String    {}
  object Char      {}

  /**
   * See notes on Schema.Enum for information on how this type is modelled
   */
case class Enum(
    values: scala.List[(java.lang.String, Data)],
    enumLabel: java.lang.String,
    schema: Schema.Enum
) extends Data

  object Enum {
    def apply(values: (java.lang.String, Data)*)(enumLabel: java.lang.String, schema: Schema.Enum) =
      new Enum(values.toList, enumLabel, schema)
  }

  case class Tuple(values: scala.List[Data]) extends Data {
    val schema: Schema.Tuple = Schema.Tuple(values.map(_.schema))
  }
  object Tuple {
    def apply(values: Data*): Tuple = Tuple(values.toList)
  }

  case class Record private (values: scala.List[(java.lang.String, Data)], schema: Schema.Record) extends Data {
    def toStruct = Data.Struct(values)
  }
  object Record {
    def apply(name: java.lang.String, fields: (java.lang.String, Data)*): Record =
      apply(name, fields.toList)

    def apply(name: java.lang.String, fields: scala.List[(java.lang.String, Data)]): Record = {
      val schema = Schema.Record(name, fields.map { case (label, data) => (label, data.schema) })
      Record(fields.toList, schema)
    }

    /** Unapply of a record should contain it's qname and the field values, not the schema */
    def unapply(record: Record) =
      Some((record.schema.name, record.values))
  }

  case class Struct(values: scala.List[(java.lang.String, Data)]) extends Data {
    val schema: Schema.Struct = Schema.Struct(values.map { case (label, data) => (label, data.schema) })
  }
  object Struct {
    def apply(fields: (java.lang.String, Data)*): Struct = new Struct(fields.toList)
  }

  /**
   * Equlvalent to ELM Optional or Scala Option
   */
  sealed trait Optional extends Data
  object Optional {
    // Note that despite the fact that there is only one element, the Data element
    // can potentially have a more specific schema than the schema given by Some. Therefore
    // the option is given to pass in the actual schema
    case class Some(data: Data, schema: Schema.Optional) extends Optional
    object Some {
      def apply(data: Data)                        = new Some(data, Schema.Optional(data.schema))
      def apply(data: Data, elementSchema: Schema) = new Some(data, Schema.Optional(elementSchema))
    }
    case class None(schema: Schema.Optional) extends Optional
    object None {
      def apply(elementSchema: Schema) = new None(Schema.Optional(elementSchema))
    }
  }
  sealed trait Result extends Data
  object Result {
    case class Ok(data: Data, schema: Schema.Result)  extends Result
    case class Err(data: Data, schema: Schema.Result) extends Result
  }

  case class List(values: scala.List[Data], schema: Schema.List) extends Data
  object List {
    def apply(values: scala.List[Data], elementSchema: Schema) =
      new List(values, Schema.List(elementSchema))

    def apply(value: Data, rest: Data*) =
      new List(value +: rest.toList, Schema.List(value.schema))

    def empty(elementSchema: Schema) =
      new List(scala.List(), Schema.List(elementSchema))

    def validated(values: scala.List[Data]): Option[List] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_.schema == values.head.schema))
        Some(List(values, Schema.List(values.head.schema)))
      else
        None
  }

  /**
   * Since in ELM, record-based datastructures are generally kept in order, we want to preserve element ordering until the last possible second (i.e. within the DDL structure).
   * This should perserve a certain amount of determinism. We have chosen to use LinkedHashMap because Scala's immutable ListMap is only suitable for a small amount of elements and
   * has O(n) lookup time. Despite the fact that this datastructure is mutable
   */
  case class Map(values: mutable.LinkedHashMap[Data, Data], schema: Schema.Map) extends Data
  object Map {
    def copyFrom(values: mutable.LinkedHashMap[Data, Data], schema: Schema.Map) =
      Map(values.clone(), schema)

    def apply(value: (Data, Data), rest: (Data, Data)*) =
      new Map(mutable.LinkedHashMap.from(value +: rest.toList), Schema.Map(value._1.schema, value._2.schema))

    def empty(keySchema: Schema, valueSchema: Schema) =
      new Map(mutable.LinkedHashMap.empty, Schema.Map(keySchema, valueSchema))
  }

  case class Set(values: mutable.LinkedHashSet[Data], schema: Schema.Set) extends Data

  object Set {
    def apply(values: mutable.LinkedHashSet[Data], elementSchema: Schema) =
      new Set(values, Schema.Set(elementSchema))

    def apply(value: Data, rest: Data*) =
      new Set(mutable.LinkedHashSet.from(value +: rest.toList), Schema.Set(value.schema))

    def empty(elementSchema: Schema) =
      new Set(mutable.LinkedHashSet(), Schema.Set(elementSchema))

    def validated(values: mutable.LinkedHashSet[Data]): Option[Set] =
      // Validate that element-type of everything is the same
      if (values.nonEmpty && values.forall(_.schema == values.head.schema))
        Some(Set(values, Schema.Set(values.head.schema)))
      else
        None
  }

  /**
   * Represents data that lives beind a typedef. For example,
   * {{{
   *   type String = String
   *   val x: String = "xyz"
   * }}}
   *
   * Should would be represented as
   * {{{
   *   Aliased(Data.String("xyz"), schema = Schema.Alias("String", Data.String))
   * }}}
   */
  case class Aliased(data: Data, schema: Schema.Alias) extends Data
}
