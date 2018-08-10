package iota  //#=cats
package iotaz //#=scalaz
package internal

import scala.reflect.macros.blackbox.Context

//#+cats
import cats.Traverse
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._ //#=2.12
//#-cats

//#+scalaz
import scalaz.Traverse
import scalaz.NonEmptyList
import scalaz.std.list._
//#-scalaz

private[iota]  //#=cats
final class ProductSeq(p: Product)
    extends collection.immutable.IndexedSeq[Any] {
  def apply(i: Int): Any = p.productElement(i)
  def length: Int = p.productArity

  // optimisations...
  override def toList: List[Any] = {
    var lst: List[Any] = Nil
    var i = length - 1
    while (i >= 0) {
      lst ::= apply(i)
      i -= 1
    }
    lst
  }
  override def foldRight[B](z: B)(op: (Any, B) ⇒ B): B = {
    var acc = z
    var i = length - 1
    while (i >= 0) {
      acc = op(apply(i), acc)
      i -= 1
    }
    acc
  }
  override def foldLeft[B](z: B)(op: (B, Any) ⇒ B): B = {
    var acc = z
    var i = 0
    val len = length
    while (i < len) {
      acc = op(acc, apply(i))
      i += 1
    }
    acc
  }
}

// an efficient Seq implementation for an , for use in a Prod
final class ArraySeq(p: Product) extends collection.immutable.IndexedSeq[Any] {
  def apply(i: Int): Any = p.productElement(i)
  def length: Int = p.productArity

  // optimisations...
  override def toList: List[Any] = {
    var lst: List[Any] = Nil
    var i = length - 1
    while (i >= 0) {
      lst ::= apply(i)
      i -= 1
    }
    lst
  }
  override def foldRight[B](z: B)(op: (Any, B) ⇒ B): B = {
    var acc = z
    var i = length - 1
    while (i >= 0) {
      acc = op(apply(i), acc)
      i -= 1
    }
    acc
  }
  override def foldLeft[B](z: B)(op: (B, Any) ⇒ B): B = {
    var acc = z
    var i = 0
    val len = length
    while (i < len) {
      acc = op(acc, apply(i))
      i += 1
    }
    acc
  }
}

private[iotaz] //#=scalaz
final class ProductMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def prodApply[L <: TList](args: c.Expr[Any]*)(
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[Prod[L]] = {

    val L = evL.tpe

//#+cats
    val pkg = q"_root_.iota.internal"
//#-cats
//#+scalaz
    val pkg = q"_root_.iotaz.internal"
//#-scalaz

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L).leftMap(NonEmptyList.one(_))
      argTypes  = args.toList.map(_.tree.tpe)
      _        <- require(argTypes.length == algebras.length,
                    s"Expected ${algebras.length} arguments but received ${argTypes.length}")
      _        <- Traverse[List].traverse(argTypes zip algebras)(tpes =>
                    require(tpes._1 <:< tpes._2,
                      s"Expected ${tpes._1} <:< ${tpes._2}").toAvowal).toEither
      seq       = if (argTypes.length <= 22) q"$pkg.ProductSeq((..$args))"
                  else q"_root_.scala.collection.immutable.IndexedSeq[_root_.scala.Any](..$args)"
   } yield q"${tb.iotaPackage}.Prod.unsafeApply[$L]($seq)")
  }

  private[this] def require(flag: Boolean, msg: => String): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).leftMap(NonEmptyList.one(_))

}
