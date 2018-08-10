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

final class ArraySeq(p: Array[Any])
    extends OptimisedIndexedSeq[Any] {
  def apply(i: Int): Any = p(i)
  def length: Int = p.length
}

sealed abstract class OptimisedIndexedSeq[A]
    extends collection.immutable.IndexedSeq[A] {

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
                  // perf testing shows that ArraySeq is faster than ProductSeq
                  // for raw fields, but is faster for case classes.
                  else q"new $pkg.ArraySeq(_root_.scala.Array[_root_.scala.Any](..$args))"
   } yield q"${tb.iotaPackage}.Prod.unsafeApply[$L]($seq)")
  }

  //#+scalaz
  def prodGen[A, R <: iotaz.TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe

    val aSym = A.typeSymbol

    val Prod = weakTypeOf[iotaz.Prod[_]].typeSymbol

    if (aSym.isModuleClass) {
      q"""
       _root_.scalaz.Isomorphism.IsoSet[$A, $Prod[$R]](
         (a: $A) => ${Prod.companion}[$R](),
         (p: $Prod[$R]) => ${A.termSymbol}
       )
       """
    } else if (aSym.isClass) {
      val aSym = A.typeSymbol.asClass

      val accessors = A.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.toList

      val to =
        if (aSym.isCaseClass)
          q"(a: $A) => ${Prod.companion}.unsafeApply[$R](new _root_.iotaz.internal.ProductSeq(a))"
        else {
          val toParts = accessors.map(method => q"a.${method.name}")
          q"(a: $A) => ${Prod.companion}[$R](..$toParts)"
        }

      // inefficient if the underlying is a List...
      val fromParts = (accessors.zipWithIndex).map {
        case (method, i) =>
          q"p.values($i).asInstanceOf[${method.typeSignatureIn(A).resultType}]"
      }
      val from = q"""(p: $Prod[$R]) => ${aSym.companion}(..$fromParts): $A"""

      q"""_root_.scalaz.Isomorphism.IsoSet[$A, $Prod[$R]]($to, $from)"""
    } else
      c.abort(c.enclosingPosition, "macro only works for classes")
  }
  //#-scalaz

  private[this] def require(flag: Boolean, msg: => String): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).leftMap(NonEmptyList.one(_))

}
