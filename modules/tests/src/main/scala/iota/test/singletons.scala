package iota  //#=cats
package iotaz //#=scalaz
package test

import scala.language.dynamics
import scala.reflect.macros.whitebox.Context

import scala.Predef.augmentString

trait Literal {
  type T
}

object Literal {
  type Bound[A] = Literal { type T <: A }
}

object LiteralInt extends Dynamic {
  def selectDynamic(selector: String): Literal.Bound[Int] =
    macro MacrosLiterally.literalIntSelectDynamic
}

object LiteralString extends Dynamic {
  def selectDynamic(selector: String): Literal.Bound[String] =
    macro MacrosLiterally.literalStringSelectDynamic
}

final class MacrosLiterally(val c: Context) {
  import c.universe.{ Literal => ASTLiteral, _ }
  import internal.decorators._

  private[this] final val iotaPackage: Tree =
    q"_root_.iota"  //#=cats
    q"_root_.iotaz" //#=scalaz

  def literalIntSelectDynamic(selector: Tree): Tree = {
    val q"${value: String}" = selector
    val tpe = c.internal.constantType(Constant(value.toInt))
    val tree = tq"$iotaPackage.test.Literal { type T = $tpe }"
    ASTLiteral(Constant(())).setType(c.typecheck(tree, mode = c.TYPEmode).tpe)
  }

  def literalStringSelectDynamic(selector: Tree): Tree = {
    val q"${value: String}" = selector
    val tpe = c.internal.constantType(Constant(value))
    val tree = tq"$iotaPackage.test.Literal { type T = $tpe }"
    ASTLiteral(Constant(())).setType(c.typecheck(tree, mode = c.TYPEmode).tpe)
  }
}
