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

final class ProductSeq(p: Product) extends OptimisedIndexedSeq[Any] {
  def apply(i: Int): Any = p.productElement(i)
  def length: Int = p.productArity
}

final class ArraySeq(p: collection.mutable.WrappedArray[Any])
    extends OptimisedIndexedSeq[Any] {
  def apply(i: Int): Any = p(i)
  def length: Int = p.length
}

sealed abstract class OptimisedIndexedSeq[A]
    extends collection.immutable.IndexedSeq[A] {

  // these things can live in scalaz-deriving... here for now
  // fusion of zip and map
  def zipmap[B, C](bs: OptimisedIndexedSeq[B])(f: (A, B) => C): List[C] = {
    var lst: List[C] = Nil
    var i = length - 1
    while (i >= 0) {
      lst ::= f(apply(i), bs(i))
      i -= 1
    }
    lst
  }
  def zipmap2[B1, B2, C](
    b1s: OptimisedIndexedSeq[B1],
    b2s: OptimisedIndexedSeq[B2]
  )(f: (A, B1, B2) => C): List[C] = {
    var lst: List[C] = Nil
    var i = length - 1
    while (i >= 0) {
      lst ::= f(apply(i), b1s(i), b2s(i))
      i -= 1
    }
    lst
  }

  // optimisations...
  override def toList: List[A] = {
    var lst: List[A] = Nil
    var i = length - 1
    while (i >= 0) {
      lst ::= apply(i)
      i -= 1
    }
    lst
  }
  override def foldRight[B](z: B)(op: (A, B) ⇒ B): B = {
    var acc = z
    var i = length - 1
    while (i >= 0) {
      acc = op(apply(i), acc)
      i -= 1
    }
    acc
  }
  override def foldLeft[B](z: B)(op: (B, A) ⇒ B): B = {
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

private[iota]  //#=cats
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
      seq       = if (argTypes.length == 0) q"_root_.scala.collection.immutable.Nil"
                  else if (argTypes.length == 1) q"new $pkg.ProductSeq(_root_.scala.Tuple1(..$args))"
                  else if (argTypes.length <= 22) q"new $pkg.ProductSeq((..$args))"
                  else q"new $pkg.ArraySeq(_root_.scala.Array[_root_.scala.Any](..$args))"
   } yield q"${tb.iotaPackage}.Prod.unsafeApply[$L]($seq)")
  }

  private[this] def require(flag: Boolean, msg: => String): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).leftMap(NonEmptyList.one(_))

}
