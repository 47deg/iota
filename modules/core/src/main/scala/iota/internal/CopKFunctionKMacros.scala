package iota  //#=cats
package iotaz //#=scalaz
package internal

//#+cats
import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.either._ //#=2.12
//#-cats

//#+scalaz
import scalaz._
import scalaz.std.list._
//#-scalaz

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

final class CopKFunctionKMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  private[this] val NatTransName: String =
    "FunctionK"             //#=cats
    "NaturalTransformation" //#=scalaz

  private[this] val NatTransType: Tree =
    tq"_root_.cats.arrow.FunctionK"         //#=cats
    tq"_root_.scalaz.NaturalTransformation" //#=scalaz

  def of[F[a] <: CopK[_, a], G[_]](args: c.Expr[Any]*)(
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      _    <- guardAssumptions("F", F)
      _    <- guardAssumptions("G", G)

      copK <- tb.destructCopK(F).leftMap(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).leftMap(NonEmptyList.one(_))

      unorderedPairs <- Traverse[List].traverse(args.toList)(arg =>
        destructFunctionKInput(arg.tree.tpe, G).map((_, arg.tree))).toEither
      lookup = unorderedPairs.toMap

      arrs <- Traverse[List].traverse(tpes)(tpe =>
        lookup.get(tpe).toRight(s"Missing interpreter $NatTransName[$tpe, $G]").toAvowalNel).toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  def summon[F[a] <: CopK[_, a], G[_]](
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      _    <- guardAssumptions("F", F)
      _    <- guardAssumptions("G", G)

      copK <- tb.destructCopK(F).leftMap(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).leftMap(NonEmptyList.one(_))

      arrs <- Traverse[List].traverse(tpes)(tpe =>
                summonFunctionK(tpe, G)).toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  private[this] def guardAssumptions(
    name: String, T: Type
  ): Either[NonEmptyList[String], _] = T.resultType match {
    case _: ExistentialType => Left(NonEmptyList.one(
      s"type parameter $name was inferred to be existential type $T and must be specified"))
    case _ if T =:= typeOf[Nothing] => Left(NonEmptyList.one(
      s"type parameter $name was inferred to be Nothing and must be specified"))
    case _ => Right(())
  }

  private[this] def makeInterpreter(
    F: Type,
    L: Type,
    G: Type,
    arrs: List[Tree]
  ): Tree = {

    val handlers = arrs.zipWithIndex.map { case (arr, i) =>
      val name = TermName(s"arr$i")
      val pre = q"private[this] val $name = $arr.asInstanceOf[$NatTransType[_root_.scala.Any, $G]]"
      val handler = (fa: TermName) => q"$name($fa.value)"
      (pre, handler)
    }

    val name = TypeName(c.freshName(s"CopK$NatTransName"))
    val defn = tb.defineFastFunctionK(
      name, F, G,
      preamble = handlers.map(_._1),
      toIndex  = fa => q"$fa.index",
      handlers = handlers.map(_._2))

    q"""$defn; new $name"""
  }

  private[this] def summonFunctionK(F: Type, G: Type): AvowalNel[String, Tree] =
    Avowal
      .catching[TypecheckException](
        c.typecheck(
          q"_root_.scala.Predef.implicitly[$NatTransType[$F, $G]]"))
      .leftMap(t => NonEmptyList.one(t.msg))

  private[this] def destructFunctionKInput(tpe: Type, G: Type): AvowalNel[String, Type] =
    tpe match {
      case TypeRef(_, sym, f :: g :: Nil) if g =:= G => Avowal.yes(f)
      case RefinedType(anyRef :: tpe2 :: Nil, scope) => // TODO: check anyRef is scala.AnyRef
        destructFunctionKInput(tpe2.dealias, G)
      case _ =>
        Avowal.noNel(s"unable to destruct input $tpe as $NatTransName[?, $G]\n" +
          s"  underlying type tree: ${showRaw{tpe}} (class ${tpe.getClass})")
    }
}
