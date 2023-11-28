package org.deusaquilus.part1.builder

import scala.quoted.*
import scala.deriving.Mirror

object BuilderMacros {

  inline def typeName[T]: String = ${ typeNameImpl[T] }
  def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.name)
  }

  inline def showFlags[T]: String = ${ showFlagsImpl[T] }
  def showFlagsImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.flags.show)
  }

  private def flagsOf[T: Type](using Quotes): quotes.reflect.Flags = {
    import quotes.reflect._
    TypeRepr.of[T].typeSymbol.flags
  }

  inline def isEnum[T]: Boolean = ${ isEnumImpl[T] }
  def isEnumImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Enum) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def isEnumOrSealedTrait[T]: Boolean = ${ isEnumOrSealedTraitImpl[T] }
  def isEnumOrSealedTraitImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    val isEnum        = flagsOf[T].is(Flags.Enum)
    val isSealedTrait = flagsOf[T].is(Flags.Sealed) && flagsOf[T].is(Flags.Trait)
    val result        = (isEnum || isSealedTrait) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]])
    Expr(result)
  }

  inline def isSealedTrait[T]: Boolean = ${ isSealedTraitImpl[T] }
  def isSealedTraitImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Sealed & Flags.Trait) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def errorOnType[T](msg: String): Nothing = ${ errorOnType[T]('msg) }
  def errorOnType[T: Type](msg: Expr[String])(using Quotes): Expr[Nothing] = {
    import quotes.reflect._
    val msgConst =
      msg match {
        case Expr(str: String) => str
        case _                 => report.errorAndAbort(s"Error-on-type has a non-constant value: ${msg.show}")
      }
    report.errorAndAbort(s"$msgConst: ${TypeRepr.of[T].widen.show}")
  }

  inline def isCaseClass[T]: Boolean = ${ isCaseClassImpl[T] }
  def isCaseClassImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    val flags = flagsOf[T]
    // for some reason case objects are considered case classes (or at least have a case flag so make sure it's not a module)
    // also, for some reason in Scala 3, the List.:: instance is actually a case class!
    Expr(flags.is(Flags.Case) && !flags.is(Flags.Module) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  inline def summonBuilder[T]: DataBuilder[T] = ${ summonBuilderImpl[T] }
  def summonBuilderImpl[T: Type](using Quotes): Expr[DataBuilder[T]] =
    import quotes.reflect._
    def failNotProductOrSum() =
      report.errorAndAbort(
        s"Cannot summon generic Builder for the type (was not a Product or Sum): ${TypeRepr.of[T].widen.show} from `summonBuilder` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.{given, _}"
      )

    val specificDriver = Expr.summon[CustomDataBuilder[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        val tpe   = TypeRepr.of[T]
        val flags = tpe.typeSymbol.flags
        Expr.summon[Mirror.ProductOf[T]] match {
          case Some(productMirror) =>
            '{ DataBuilder.buildProductFromMirror(${ productMirror }).asInstanceOf[DataBuilder[T]] }
          case None =>
            Expr.summon[Mirror.SumOf[T]] match {
              case Some(sumMirror) =>
                '{ DataBuilder.buildSumFromMirror(${ sumMirror }).asInstanceOf[DataBuilder[T]] }
              case None =>
                report.errorAndAbort(
                  s"[summonBuilder] Cannot summon specific or generic Product or Sum Builder for the sum type: ${tpe.widen.show} from `summonBuilder` (flags: ${flags.show}). Have you imported org.finos.morphir.datamodel.{given, _}"
                )
            }
        }
    }

  inline def summonProductBuilder[T]: DataBuilder[T] = ${ summonProductBuilderImpl[T] }
  def summonProductBuilderImpl[T: Type](using Quotes): Expr[DataBuilder[T]] =
    import quotes.reflect._
    def failNotProduct() =
      report.errorAndAbort(
        s"[summonProductBuilder] Cannot summon generic Builder for the type (was not a Product): ${TypeRepr.of[T].widen.show} from `summonProductBuilder` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.{given, _}"
      )
    val tpe   = TypeRepr.of[T]
    val flags = tpe.typeSymbol.flags

    // Even if it's a product-builder, try to grab a specific builder for it first because certain things
    // (e.g. the `::` class) should be as specific builders (i.e. SpecificBuilder[List[T]]) instead of the generic one
    val specificDriver = Expr.summon[CustomDataBuilder[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        val productMirror = Expr.summon[Mirror.ProductOf[T]]
        if (productMirror.nonEmpty) {
          '{ DataBuilder.buildProductFromMirror(${ productMirror.get }).asInstanceOf[DataBuilder[T]] }

          // ====== Doing it like this shuold work but it doesn't for the BuildEnum cases, I'm not sure why ======
          //          val genericBuilder = Expr.summon[Builder[T]]
          //          genericBuilder match {
          //            case Some(value) => value
          //            case _ =>
          //              report.errorAndAbort(
          //                s"[summonProductBuilder] Cannot summon specific or generic Product Builder for the product type: ${tpe.widen.show} from `summonProductBuilder` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.{given, _}"
          //              )
          //          }
        } else
          failNotProduct()
    }
}
