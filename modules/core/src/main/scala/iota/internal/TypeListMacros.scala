package iota  //#=cats
package iotaz //#=scalaz
package internal

import scala.reflect.macros.whitebox.Context

private[iota]  //#=cats
private[iotaz] //#=scalaz
final class TypeListMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def materializeTListPos[L <: TList, A](
    implicit
      evL: c.WeakTypeTag[L],
      evA: c.WeakTypeTag[A]
  ): c.Expr[TList.Pos[L, A]] = {

    val L = evL.tpe
    val A = evA.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= A))
                    .filterOrElse(_ >= 0, s"$A is not a member of $L")
    } yield
      q"new ${tb.iotaPackage}.TList.Pos[$L, $A]{ override val index: scala.Int = $index }", true)
  }

  def materializeTListKPos[L <: TListK, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[_]]
  ): c.Expr[TListK.Pos[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListKTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new ${tb.iotaPackage}.TListK.Pos[$L, $F]{ override val index: scala.Int = $index }", true)
  }

  def materializeTListHPos[L <: TListH, F[_[_]]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[Nothing]]
  ): c.Expr[TListH.Pos[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListHTypes(L)
      index    <- Right(algebras.indexWhere(_ =:= F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
        q"new ${tb.iotaPackage}.TListH.Pos[$L, $F]{ override val index: scala.Int = $index }", true)
  }

  def materializeTListLength[L <: TList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TList.Length[L]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      length    = algebras.length
    } yield
        q"""new ${tb.iotaPackage}.TList.Length[$L] {
              type Value = ${internal.constantType(Constant(length))}
              override val value: Value = $length
            }""", true)
  }

  def materializeTListCompute[L <: TList, O <: TList](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TList.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L)
      tpe       = tb.buildTList(algebras)
    } yield
      q"new ${tb.iotaPackage}.TList.Compute[$L]{ override type Out = $tpe }", true)
  }

  def materializeTListKCompute[L <: TListK, O <: TListK](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TListK.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListKTypes(L)
      tpe       = tb.buildTListK(algebras)
    } yield
      q"new ${tb.iotaPackage}.TListK.Compute[$L]{ override type Out = $tpe }", true)
  }

  def materializeTListHCompute[L <: TListH, O <: TListH](
    implicit
      evL: c.WeakTypeTag[L]
  ): c.Expr[TListH.Compute.Aux[L, O]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      algebras <- tb.memoizedTListHTypes(L)
      tpe       = tb.buildTListH(algebras)
    } yield
        q"new ${tb.iotaPackage}.TListH.Compute[$L]{ override type Out = $tpe }", true)
  }

}
