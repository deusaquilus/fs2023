package org.deusaquilus.part1.builder

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonFrom, summonInline}
import org.deusaquilus.*
import org.deusaquilus.part1.{Schema, Data}

trait DataBuilder[T] {
  final def apply(value: T): Data = build(value)
  def build(value: T): Data
  def schema: Schema
}

object DataBuilder extends BuilderInstances {
  type IsProduct[P <: scala.Product] = P
  type IsOption[P <: Option[_]]      = P

  import BuilderMacros._

  inline def toData[T](value: T): Data = {
    import org.deusaquilus.part1.builder.DataBuilder.{given, _}
    val builder = DataBuilder.gen[T]
    builder.build(value)
  }

  inline def summonSpecificBuilder[T] =
    summonFrom {
      case builder: CustomDataBuilder[T] => builder
      case _ =>
        error(s"Cannot find specific builder for type: ${showType[T]}")
    }

  private inline def deriveSumVariants[Fields <: Tuple, Elems <: Tuple]: List[SumComposer.Variant] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        // note that no printout done here (e.g. println("hello")) will actually happen at compile-time
        // since this is actually a string that will be spliced in at runtime
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            // need to make sure that ALL of the matches inside here (including the `unionType` match)
            // is inline otherwise very strange things happen! Macros will run for even variants that shouldn't be matching
            // (i.e. because the other side of the case match branch is also running)
            val variant =
              // enum case with fields
              if (isCaseClass[head]) {
                summonProductBuilder[head] match {
                  case builder: GenericProductBuilder[Product] @unchecked =>
                    SumComposer.EnumProduct(fieldName, builder)
                  case other =>
                    throw new IllegalArgumentException(
                      "Illegal state, should not be possible, summonProductBuilder always returns a GenericProductBuilder"
                    )
                }
              } // enum case without fields
              else {
                SumComposer.EnumSingleton(fieldName)
              }

            // return the variant and recurse
            variant +: deriveSumVariants[fields, tail]

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  /** Not possible to check variants in pattern-match here. Setting them as widest possible type. */
  inline def buildProductFields[Fields <: Tuple, Elems <: Tuple](i: Int): List[ProductComposerField] =
    inline erasedValue[Fields] match {
      case EmptyTuple => Nil

      case _: (field *: fields) =>
        val fieldName = constValue[field].toString
        inline erasedValue[Elems] match {
          case _: (head *: tail) =>
            val derivationStage =
              summonBuilder[head] match {
                case builder: CustomDataBuilder[Any] @unchecked =>
                  ProductComposer.Leaf(fieldName, i, builder)
                case builder: GenericProductBuilder[Product] @unchecked =>
                  ProductComposer.Product(fieldName, i, builder)
                case builder: GenericSumBuilder[Any] @unchecked =>
                  ProductComposer.Sum(fieldName, i, builder)
              }
            derivationStage +: buildProductFields[fields, tail](i + 1)

          case EmptyTuple =>
            error("shuold not be possible")
        }
    }

  inline def buildProductFromMirror[T](m: Mirror.ProductOf[T]): GenericProductBuilder[T & Product] =
    inline if (isCaseClass[T]) {
      val caseClassName  = BuilderMacros.typeName[T]
      val stageListTuple = buildProductFields[m.MirroredElemLabels, m.MirroredElemTypes](0)
      val mirrorProduct  = ProductComposer.MirrorProduct(caseClassName, stageListTuple)
      GenericProductBuilder
        .make[T & Product](mirrorProduct)
    } else {
      errorOnType[T]("Cannot summon a generic builder of the case class type. It is not a valid product type")
    }

  inline def buildSumFromMirror[T](m: Mirror.SumOf[T]): GenericSumBuilder[T] =
    inline if (isEnumOrSealedTrait[T]) {
      val sumTypeName = BuilderMacros.typeName[T]
      val enumName    = typeName[T]

      // The clause `inferUnionType` NEEDs to be  a macro otherwise we can't get the value
      // coming out if it to work with inline matches/ifs and if our matches/ifs are not inline
      // and there is a `scala.compiletime.error` command called of a non-inline match/if branch
      // called then it will happen no mater what the input data is because these constructs
      // just do the equivalent of report.errorAndAbort for all inputs. The only way to NOT
      // activate them is to have them on a branch of a inline match/if which is not being called
      val variants =
        deriveSumVariants[m.MirroredElemLabels, m.MirroredElemTypes]

      val builder = {
        val ordinalGetter: Any => Int =
          (v: Any) =>
            v match {
              case t: T => m.ordinal(t)
              case _ =>
                throw new IllegalArgumentException(
                  s"The value `$v` is not an instance of the needed enum class ${enumName}"
                )
            }
        SumComposer(sumTypeName, ordinalGetter, variants)
      }

      GenericSumBuilder.make[T](builder)
    } else {
      errorOnType[T]("The following type is not a valid enum and there is no specific builder defined for it")
    }

  // Needed so you can do the syntax `case class Foo(...) derives Builder`
  inline def derived[T]: DataBuilder[T] = gen[T]

  inline def build[T]: DataBuilder[T] = gen[T]

  // TODO When making a product builder, make sure to exclude Option[T] since
  //      we want a specific builder for that, not a generic one.
  inline def gen[T]: DataBuilder[T] =
    summonFrom {
      // If there is a leaf-level builder, summon that first. Do NOT EVER try to summon Builder[T]
      // directly because you will can run into infinite recursive derivation.
      case builder: CustomDataBuilder[T] =>
        builder
      case ev: Mirror.Of[T] =>
        inline ev match {
          case m: Mirror.ProductOf[IsOption[t]] =>
            error(
              s"Cannot summon a generic derivation of Option[T], a specific encoder is required."
            )

          case m: Mirror.ProductOf[T] =>
            // cast is needed otherwise it's T & Product which doesn't seem to derive correctly
            buildProductFromMirror[T](m).asInstanceOf[DataBuilder[T]]
          case m: Mirror.SumOf[T] =>
            buildSumFromMirror[T](m)
        }
    }
}
