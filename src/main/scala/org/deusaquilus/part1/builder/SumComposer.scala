package org.deusaquilus.part1.builder

import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

private[builder] case class SumComposer(
    enumName: String,
    ordinalGetter: Any => Int,
    variants: List[SumComposer.Variant]
) {
  private def failInsideNotProduct(derivedAs: Any) =
    throw new IllegalArgumentException(
      s"Inner enum data is only allowed to come from a Scala product but it was derived as: $derivedAs"
    )

  lazy val enumType = {
    val enumCases =
      variants.map { v =>
        v match {
          // case object is represented as a case with no fields
          case variant: SumComposer.EnumSingleton =>
            Schema.Enum.Case(v.enumLabel, List())

          case variant: SumComposer.EnumProduct =>
            val structMembers =
              variant.builder.schema match {
                case record: Schema.Record =>
                  record.fields.map { case (label, schema) => (label, schema) }
                case other =>
                  failInsideNotProduct(other)
              }
            Schema.Enum.Case(v.enumLabel, structMembers)

          case variant: SumComposer.Variant =>
            throw new IllegalArgumentException("Non-Discrimiated union decoding is not supported yet.")
        }

      }
    Schema.Enum(enumName, enumCases)
  }

  def run(value: Any): Data = {
    val usedVariant = variants(ordinalGetter(value))

    val enumValues =
      usedVariant match {
        // for a enum case object, data-type is just a 'unit'
        case SumComposer.EnumSingleton(enumLabel) =>
          List()

        case v: SumComposer.EnumProduct =>
          value match {
            case p: Product =>
              val enumCaseRecord = v.builder.build(p)
              enumCaseRecord match {
                case record: Data.Record =>
                  /*
                  Translate:
                    sealed trait Foo
                    case class Bar(blin: String, blu: Int)
                    case object Baz
                    === or ===
                    enum Foo {
                      case Bar(blin: String, blu: Int)
                      case Baz
                    }
                    === into ===
                    type Foo
                      = Bar blin:String blu:Int
                      | Baz
                   */
                  record.values

                case other =>
                  failInsideNotProduct(other)
              }
            case other => throw new IllegalArgumentException(
                s"The value ($value) for the enum variant ${v.enumLabel} must be a scala product type (case class or multi-field enum) but it was a ${other.getClass}"
              )
          }

        case v: SumComposer.Variant =>
          throw new IllegalArgumentException("Non Discrimiated Unions are not supported yet.")
      }

    Data.Enum(enumValues, usedVariant.enumLabel, enumType)
  }
}
object SumComposer {
  sealed trait Variant {
    def enumLabel: java.lang.String
  }
  sealed trait EnumVariant extends Variant
  // case object variant of a sealed trait or a enum case with no fields
  case class EnumSingleton(enumLabel: java.lang.String)
      extends EnumVariant
  // case class variant of sealed trait or enum case with fields
  case class EnumProduct(enumLabel: java.lang.String, builder: GenericProductBuilder[Product])
      extends EnumVariant

  // for generic sums
  case class SumVariant(enumLabel: java.lang.String, builder: DataBuilder[Any]) extends Variant
}
