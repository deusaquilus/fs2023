package org.deusaquilus.part1.print

import org.deusaquilus.*
import fansi.Str
import org.deusaquilus.part1.{Schema, Data}
import pprint.{Renderer, Tree, Truncated}

sealed trait DetailLevel {
  def hideFQNames: Boolean
  def compressNestedSchemas: Boolean
  def compressData: Boolean
  def compressSchema: Boolean
  def hideInnerSchemas: Boolean
}
object DetailLevel {
  object Detailed extends DetailLevel {
    def hideFQNames            = false
    def compressNestedSchemas = false
    def compressData           = false
    def compressSchema        = false
    def hideInnerSchemas      = false
  }
  object Medium extends DetailLevel {
    def hideFQNames            = true
    def compressNestedSchemas = false
    def compressData           = false
    def compressSchema        = true
    def hideInnerSchemas      = false
  }
  object BirdsEye extends DetailLevel {
    def hideFQNames            = true
    def compressNestedSchemas = false
    def compressData           = true
    def compressSchema        = true
    def hideInnerSchemas      = true
  }

  object BirdsEye2 extends DetailLevel {
    def hideFQNames            = false
    def compressNestedSchemas = false
    def compressData           = true
    def compressSchema        = true
    def showInnerSchemas      = false
    def hideInnerSchemas      = true
  }

  object BirdsEye3 extends DetailLevel {
    def hideFQNames            = false
    def compressNestedSchemas = false
    def compressData           = true
    def compressSchema        = true
    def showInnerSchemas      = false
    def hideInnerSchemas      = false
  }
}

object PrintMDM {
  def apply(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide,
      defaultWidth: Int = 150
  ) = new PrintIR(detailLevel, fieldNames, defaultWidth).apply(any)
}

sealed trait FieldNames
object FieldNames {
  object Show extends FieldNames
  object Hide extends FieldNames
}

class PrintIR(
    detailLevel: DetailLevel,
    fieldNames: FieldNames,
    val defaultWidth: Int
) extends pprint.Walker {
  val showFieldNames                = fieldNames == FieldNames.Show
  val escapeUnicode                 = false
  val defaultHeight: Int            = Integer.MAX_VALUE
  val defaultIndent: Int            = 2
  val colorLiteral: fansi.Attrs     = fansi.Color.Green
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow
  var verbose: Boolean              = false

  override def additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty

  def apply(x: Any, verbose: Boolean = false): fansi.Str = {
    this.verbose = verbose
    val tokenized = this.tokenize(x).toSeq
    fansi.Str.join(tokenized)
  }

  def treeify(x: Any): Tree      = this.treeify(x, escapeUnicode, showFieldNames)
  def treeifySuper(x: Any): Tree = super.treeify(x, escapeUnicode, showFieldNames)

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = x match {

    case v: Data =>
      if (detailLevel.compressData)
        PrintData.of(v)
      else
        appendPrefix(super.treeify(v, escapeUnicode, showFieldNames), "Data.")

    case v: Schema =>
      if (detailLevel.compressSchema)
        PrintSchema.of(v)
      else
        appendPrefix(super.treeify(v, escapeUnicode, showFieldNames), "Schema.")

    case other => super.treeify(other, escapeUnicode, showFieldNames)
  }

  def appendPrefix(tree: Tree, prefix: String) =
    tree match {
      case Tree.Apply(head, body) => Tree.Apply(prefix + head, body)
      case other                  => other
    }

  object PrintData {
    def of(v: Data): Tree =
      v match {
        case v: Data.Basic[_] =>
          v match {
            case _                 => treeifySuper(v)
          }

        case v: Data.Enum =>
          val body = v.values.asInstanceOf[scala.List[(String, Data)]].map { case (label, data) =>
            Tree.KeyValue(label, treeify(data))
          }
          Tree.Apply("@" + v.enumLabel + ":Case", (body :+ treeify(v.schema)).iterator)

        case v: Data.Tuple =>
          Tree.ofData(v)(v.values.map(treeify(_)))

        case v: Data.Struct =>
          val body = v.values.map { case (k, v) => Tree.KeyValue(k, treeify(v)) }
          Tree.ofData(v)(body)

        case v: Data.Record =>
          val body = v.values.map { case (k, v) => Tree.KeyValue(k, treeify(v)) }
          Tree.ofData(v)(body)

        case v: Data.Optional =>
          v match {
            case Data.Optional.Some(data, _) => Tree.Apply(v.printName, List(treeify(data)).iterator)
            case Data.Optional.None(_)       => Tree.Literal(v.printName)
          }

        case v: Data.Result =>
          v match {
            case Data.Result.Ok(data, _)  => Tree.ofData(v)(List(treeify(data)))
            case Data.Result.Err(data, _) => Tree.ofData(v)(List(treeify(data)))
          }

        case v: Data.List =>
          Tree.ofData(v)(v.values.map(r => treeify(r)))

        case v: Data.Map =>
          Tree.ofData(v)(v.values.toList.map(treeify(_)))

        case v: Data.Set =>
          Tree.ofData(v)(v.values.toList.map(r => treeify(r)))

        case v: Data.Aliased =>
          Tree.ofData(v)(List(treeify(v.data)))
      }
  }

  object PrintSchema {
    def of(c: Schema) =
      c match {
        case v: Schema.Basic[_] => Tree.Literal(v.printName)
        case v: Schema.Any.type => Tree.Literal(v.printName)

        case v: Schema.Record =>
          val caseNames =
            if (detailLevel.hideInnerSchemas)
              v.fields.map(_._1).map(Tree.Literal(_))
            else {
              v.fields.map { case (label, value) => Tree.KeyValue(label, treeify(value)) }
            }
          Tree.ofSchema(v)(caseNames)

        case v: Schema.Struct =>
          val caseNames = v.fields.map(_._1)
          Tree.ofSchema(v)(caseNames.map(Tree.Literal(_)))

        case v @ Schema.Alias(name, value) =>
          if (detailLevel.hideFQNames)
            Tree.ofSchema(v)(List(treeify(value)))
          else
            Tree.ofSchema(v)(List(treeify(name), treeify(value)))

        case v @ Schema.List(elementType) =>
          Tree.ofSchema(v)(List(treeify(elementType)))

        case v @ Schema.Map(keyType, valueType) =>
          Tree.ofSchema(v)(List(treeify(keyType), treeify(valueType)))

        case v @ Schema.Set(elementType) =>
          Tree.ofSchema(v)(List(treeify(elementType)))

        case v: Schema.Tuple =>
          Tree.ofSchema(v)(v.values.map(treeify(_)))

        case c @ Schema.Optional(elementType) =>
          Tree.ofSchema(c)(List(treeify(elementType)))

        case r @ Schema.Result(errType, okType) =>
          Tree.ofSchema(r)(List(treeify(errType), treeify(okType)))

        case c: Schema.Enum =>
          val caseNames =
            if (detailLevel.hideInnerSchemas)
              c.cases.map(c => c.label).map(Tree.Literal(_))
            else {
              c.cases.map { enumCase =>
                val cases =
                  enumCase.fields.map {
                    case (enumLabel, data) => Tree.KeyValue(enumLabel, treeify(data))
                  }
                Tree.Apply("$" + enumCase.label + ":Enum.Case", cases.iterator)
              }
            }
          Tree.ofSchema(c)(caseNames)
      }
  }

  implicit class TreeOpts(tree: Tree.type) {
    // Don't display full paths even for data-elements that have them,
    // display all of that information in the schema-area
    def ofData(d: Data)(children: List[Tree]) =
      Tree.Apply(d.printName, children.iterator)

    def ofSchema(c: Schema)(children: List[Tree]) =
      if (detailLevel.hideFQNames)
        Tree.Apply(c.printName, children.iterator)
      else {
        val elems = c.getName.map(treeify(_)).toList ++ children
        c.getName match {
          case Some(value) => Tree.Apply(c.printName, (treeify(value) +: children).iterator)
          case None        => Tree.Apply(c.printName, children.iterator)
        }
      }
  }

  implicit class DataPrintOpts(v: Data) {
    def printName: String =
      v.getName match {
        // e.g. @Person:Record(...)
        case Some(value) => "@" + value + ":" + v.getClass.getSimpleName
        // e.g. @List(...)
        case None => "@" + v.getClass.getSimpleName
      }
  }

  implicit class SchemaPrintOpts(v: Schema) {
    def printName: String =
      v.getName match {
        // e.g. $Person:Record(...)
        case Some(value) => "$" + value + ":" + v.getClass.getSimpleName
        // e.g. $List(...)
        case None => "$" + v.getClass.getSimpleName
      }
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}
