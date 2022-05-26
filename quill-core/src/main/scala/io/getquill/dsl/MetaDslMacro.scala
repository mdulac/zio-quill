package io.getquill.dsl

import io.getquill.util.MacroContextExt._
import io.getquill.util.OptionalTypecheck

import scala.reflect.macros.whitebox.{ Context => MacroContext }

class MetaDslMacro(val c: MacroContext) extends ValueComputation {
  import c.universe._

  def schemaMeta[T](entity: Tree, columns: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    c.untypecheck {
      q"""
        new ${c.prefix}.SchemaMeta[$t] {
          private[this] val _entity = ${c.prefix}.quote {
            ${c.prefix}.querySchema[$t]($entity, ..$columns)
          }
          def entity = _entity
        }
      """
    }

  def queryMeta[T, R](expand: Tree)(extract: Tree)(implicit t: WeakTypeTag[T], r: WeakTypeTag[R]): Tree =
    c.untypecheck {
      q"""
        new ${c.prefix}.QueryMeta[$t] {
          private[this] val _expand = $expand
          def expand = _expand
          val extract =
            (r: ${c.prefix}.ResultRow, s: ${c.prefix}.Session) => $extract(implicitly[${c.prefix}.QueryMeta[$r]].extract(r, s))
        }
      """
    }

  def insertMeta[T](exclude: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe, exclude: _*), "insert")

  def updateMeta[T](exclude: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe, exclude: _*), "update")

  def materializeQueryMeta[T](implicit t: WeakTypeTag[T]): Tree = {
    val value = this.value("Decoder", t.tpe)
    q"""
      new ${c.prefix}.QueryMeta[$t] {
        private[this] val _expanded = ${expandQuery[T](value)}
        def expand = _expanded
        val extract = ${extract[T](value)}
      }
    """
  }

  def materializeUpdateMeta[T](implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe), "update")

  def materializeInsertMeta[T](implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe), "insert")

  def materializeSchemaMeta[T](implicit t: WeakTypeTag[T]): Tree =
    if (t.tpe.typeSymbol.isClass && t.tpe.typeSymbol.asClass.isCaseClass) {
      q"""
          new ${c.prefix}.SchemaMeta[$t] {
            private[this] val _entity =
              ${c.prefix}.quote(${c.prefix}.impliedQuerySchema[$t](${t.tpe.typeSymbol.name.decodedName.toString}))
            def entity = _entity
          }
        """
    } else {
      c.fail(s"Can't materialize a `SchemaMeta` for non-case-class type '${t.tpe}', please provide an implicit `SchemaMeta`.")
    }

  private def expandQuery[T](value: Value)(implicit t: WeakTypeTag[T]) = {
    val elements = flatten(q"x", value)
    if (elements.isEmpty)
      c.fail(s"Case class type ${t.tpe} has no values")
    //q"${c.prefix}.quote((q: io.getquill.Query[$t]) => q.map(x => io.getquill.dsl.UnlimitedTuple(..$elements)))"
    q"${c.prefix}.quote((q: io.getquill.Query[$t]) => q.map(x => x))"
  }

  private def extract[T](value: Value)(implicit t: WeakTypeTag[T]): Tree = {
    var index = -1

    val nullChecker =
      OptionalTypecheck(c)(q"implicitly[${c.prefix}.NullChecker]") match {
        case Some(nullChecker) => nullChecker
        case None =>
          c.fail(
            s"""Can't find a NullChecker for the context ${show(c.prefix.tree)}. // actualType.widen
               |Have you imported the context e.g.
               |import ${show(c.prefix.tree)}._""".stripMargin
          )
      }

    case class FieldExpansion(lookup: Tree, old: Tree) //nullCheck: Tree,

    def expand(value: Value, parentOptional: Boolean = false): Tree =
      expandRecurse(value, parentOptional)

    def expandRecurse(value: Value, parentOptional: Boolean = false): FieldExpansion =
      value match {

        case Scalar(_, tpe, decoder, optional) =>
          index += 1
          if (parentOptional) {
            if (optional) {
              val lookup = q"$decoder($index, row, session)"
              val oldLookup = q"Some($decoder($index, row, session))"
              FieldExpansion(lookup, oldLookup)
            }  else {
              val lookup = q"$decoder($index, row, session)"
              val oldLookup = q"implicitly[${c.prefix}.Decoder[Option[$tpe]]].apply($index, row, session)"
              FieldExpansion(lookup, oldLookup)
            }

          } else {
            val lookup = q"$decoder($index, row, session)"
            val oldLookup = q"$decoder($index, row, session)"
            FieldExpansion(lookup, oldLookup)
          }

        case Nested(_, tpe, params, optional) =>
          if (parentOptional || optional) {
            val groups: Seq[List[(Tree, Boolean)]] =
              params.map(_.map(v => expand(v, parentOptional = true) -> v.nestedAndOptional))

            val terms: Seq[List[FieldExpansion]] =
              groups.zipWithIndex.map {
                case (options: List[(Tree, Boolean)], idx1: Int) =>
                  options.zipWithIndex.map {
                    case ((_: Tree, opt: Boolean), idx2: Int) =>
                      val term = TermName(s"o_${idx1}_$idx2")
                      if (opt) {
                        val lookup = q"Some($term)"
                        val oldLookup = q"Some($term)"
                        FieldExpansion(lookup, oldLookup)
                      } else {
                        val lookup = q"$term"
                        val oldLookup = q"$term"
                        FieldExpansion(lookup, oldLookup)
                      }
                  }
              }

            groups.zipWithIndex.foldLeft(q"Some(new $tpe(...$terms))") {
              case (body: Tree, (options: List[(Tree, Boolean)], idx1: Int)) =>
                options.zipWithIndex.foldLeft(body) {
                  case (body, ((option, _), idx2)) =>
                    val o = q"val ${TermName(s"o_${idx1}_$idx2")} = $EmptyTree"
                    val lookup = q"$option.flatMap($o => $body)"
                    val oldLookup = q"$option.flatMap($o => $body)"
                    FieldExpansion(lookup, oldLookup)
                }
            }
          } else {
            val lookup = q"new $tpe(...${params.map(_.map(expand(_)))})"
            val oldLookup = q"new $tpe(...${params.map(_.map(expand(_)))})"
            FieldExpansion(lookup, oldLookup)
          }
      }
    q"(row: ${c.prefix}.ResultRow, session: ${c.prefix}.Session) => ${expand(value)}"
  }

  private def actionMeta[T](value: Value, method: String)(implicit t: WeakTypeTag[T]) = {
    val assignments =
      flatten(q"v", value)
        .zip(flatten(q"value", value))
        .map {
          case (vTree, valueTree) =>
            q"(v: $t) => $vTree -> $valueTree"
        }
    c.untypecheck {
      q"""
        new ${c.prefix}.${TypeName(method.capitalize + "Meta")}[$t] {
          private[this] val _expand =
            ${c.prefix}.quote((q: io.getquill.EntityQuery[$t], value: $t) => q.${TermName(method)}(..$assignments))
          def expand = _expand
        }
      """
    }
  }
}