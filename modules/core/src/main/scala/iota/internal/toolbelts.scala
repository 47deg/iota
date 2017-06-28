/*
 * Copyright 2016-2017 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package iota
package internal

import cats.Applicative
import cats.Id
import cats.Eval
import cats.FlatMap
import cats.Foldable
import cats.Traverse
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._ //#=2.12
import cats.syntax.foldable._

import catryoshka._

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.reflect.api.Universe
import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.macros.blackbox.Context

trait Toolbelt {
  type Uu <: Universe
  val u: Uu
}

trait MacroToolbelt extends Toolbelt {
  type Cc <: Context
  val c: Cc

  final override type Uu = c.universe.type
  final override val u: Uu = c.universe
}

object IotaReflectiveToolbelt {
  def apply[U <: Universe](u: U): IotaReflectiveToolbelt[u.type] =
    new IotaReflectiveToolbelt[u.type](u)

  def apply(): IotaReflectiveToolbelt[runtimeUniverse.type] =
    apply(runtimeUniverse)
}

final class IotaReflectiveToolbelt[U <: Universe](val u: U)
    extends Toolbelt
    with TypeListTrees
    with TypeListParsers
    with TypeListEvaluators
    with TypeListBuilders
    with CoproductAPI { override type Uu = U }


private[internal] class IotaMacroToolbelt[C <: Context](val c: C)
    extends MacroToolbelt
    with TypeListTrees
    with TypeListParsers
    with TypeListEvaluators
    with TypeListBuilders
    with CoproductAPI
    with MacroAPI { override type Cc = C }

private[internal] object IotaMacroToolbelt {
  def apply[C <: Context](c: C): IotaMacroToolbelt[c.type] =
    new IotaMacroToolbelt[c.type](c)

  final class Cache {
    @volatile var underlying: Map[Any, Any] = Map.empty
  }

  final val typeListCache: Cache = new Cache()
}

// --
// - implementation

private[internal] sealed trait TypeListTrees { self: Toolbelt =>
  import u._

  sealed trait NodeF  [+A]
  case class   ConsF   [A](head: Type, tail: A) extends NodeF[A]
  case class   ConcatF [A](nodes: List[A])      extends NodeF[A]
  case class   ReverseF[A](node: A)             extends NodeF[A]
  case class   TakeF   [A](n: Int, node: A)     extends NodeF[A]
  case class   DropF   [A](n: Int, node: A)     extends NodeF[A]
  case object  NNilF                            extends NodeF[Nothing]

  object NodeF {
    implicit val nodeTraverse: Traverse[NodeF] = new Traverse[NodeF] {
      def traverse[G[_], A, B](fa: NodeF[A])(f: A => G[B])(implicit G: Applicative[G]): G[NodeF[B]] = fa match {
        case ConsF(hd, a) => G.map(f(a))(ConsF(hd, _))
        case ConcatF(as)  => G.map(Traverse[List].traverse(as)(f))(ConcatF(_))
        case ReverseF(a)  => G.map(f(a))(ReverseF(_))
        case TakeF(n, a)  => G.map(f(a))(TakeF(n, _))
        case DropF(n, a)  => G.map(f(a))(DropF(n, _))
        case NNilF        => G.pure(NNilF: NodeF[B])
      }

      def foldLeft[A, B](fa: NodeF[A], b: B)(f: (B, A) => B): B = fa match {
        case ConsF(_, a)  => f(b, a)
        case ConcatF(as)  => Foldable[List].foldLeft(as, b)(f)
        case ReverseF(a)  => f(b, a)
        case TakeF(_, a)  => f(b, a)
        case DropF(_, a)  => f(b, a)
        case NNilF        => b
      }

      def foldRight[A, B](fa: NodeF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case ConsF(_, a)  => f(a, lb)
        case ConcatF(as)  => Foldable[List].foldRight(as, lb)(f)
        case ReverseF(a)  => f(a, lb)
        case TakeF(_, a)  => f(a, lb)
        case DropF(_, a)  => f(a, lb)
        case NNilF        => lb
      }
    }
  }
}

private[internal] sealed trait TypeListParsers { self: Toolbelt with TypeListTrees =>
  import u._

  private[this] def literalInt(tpe: Type): Either[Id[String], Int] =
    tpe match {
      case ConstantType(Constant(value: Int)) => value.asRight
      case _ => s"Expected $tpe to be a literal integer".asLeft
    }

  private[internal] type Parser = CoalgebraM[Either[Id[String], ?], NodeF, Type]

  private[internal] def typeListParser(
    ConsSym   : Symbol,
    NilSym    : Symbol,
    ConcatSym : Symbol,
    ReverseSym: Symbol,
    TakeSym   : Symbol,
    DropSym   : Symbol
  ): Parser = tpe0 => {
    @tailrec def loop(tpe: Type): Either[Id[String], NodeF[Type]] = tpe.dealias match {
      case TypeRef(_, sym, args) =>
        sym.asType.toType.dealias.typeSymbol match {
          case ConsSym    => ConsF(args(0), args(1)).asRight
          case NilSym     => NNilF.asRight
          case ConcatSym  => ConcatF(args).asRight
          case ReverseSym => ReverseF(args(0)).asRight
          case TakeSym    => literalInt(args(0)).map(TakeF(_, args(1)))
          case DropSym    => literalInt(args(0)).map(DropF(_, args(1)))
          case sym        => s"Unexpected symbol $sym for type $tpe: ${showRaw(tpe)}".asLeft
        }
      case ExistentialType(_, res) => loop(res) // the irony...
      case _ => s"Unable to parse type $tpe: ${showRaw(tpe)}".asLeft
    }
    loop(tpe0)
  }

  private[this] def symbolOf[T](implicit evT: WeakTypeTag[T]): Symbol = evT.tpe.typeSymbol

  final lazy val tlistParser: Parser = typeListParser(
    NilSym     = symbolOf[iota.TNil],
    ConsSym    = symbolOf[iota.TCons[Nothing, Nothing]],
    ConcatSym  = symbolOf[iota.TList.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[iota.TList.Op.Reverse[Nothing]],
    TakeSym    = symbolOf[iota.TList.Op.Take[Nothing, Nothing]],
    DropSym    = symbolOf[iota.TList.Op.Drop[Nothing, Nothing]])

  final lazy val klistParser: Parser = typeListParser(
    NilSym     = symbolOf[iota.KNil],
    ConsSym    = symbolOf[iota.KCons[Nothing, Nothing]],
    ConcatSym  = symbolOf[iota.KList.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[iota.KList.Op.Reverse[Nothing]],
    TakeSym    = symbolOf[iota.KList.Op.Take[Nothing, Nothing]],
    DropSym    = symbolOf[iota.KList.Op.Drop[Nothing, Nothing]])
}

private[internal] sealed trait TypeListEvaluators { self: Toolbelt with TypeListTrees with TypeListParsers =>
  import u._

  final def evalTree: Algebra[NodeF, List[Type]] = {
    case ConsF(head, types) => head :: types
    case ConcatF(typeLists) => FlatMap[List].flatten(typeLists)
    case ReverseF(types)    => types.reverse
    case TakeF(n, types)    => types.take(n)
    case DropF(n, types)    => types.drop(n)
    case NNilF              => Nil
  }

  final def tlistTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe)(
      evalTree.generalizeM[Either[Id[String], ?]],
      tlistParser)

  final def klistTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe)(
      evalTree.generalizeM[Either[Id[String], ?]],
      klistParser)

  final def klistTypeConstructors(tpe: Type): Either[Id[String], List[Type]] =
    klistTypes(tpe).map(_.map(_.etaExpand.resultType))
}

private[internal] sealed trait TypeListBuilders { self: Toolbelt with TypeListTrees =>
  import u._

  class TypeListBuilder private[TypeListBuilders](
    consTpe: Type,
    nilTpe: Type
  ) {
    final def apply(tpes: List[Type]): Type =
      tpes.foldRight(nilTpe)(makeCons)

    private[this] lazy val (consPrefix, consSym) = consTpe match {
      case TypeRef(prefix, sym, _) => (prefix, sym)
      case _ => sys.error("internal iota initialization error")
    }

    private[this] def makeCons(head: Type, tail: Type): Type =
      internal.typeRef(consPrefix, consSym, head :: tail :: Nil)

    val bleep: Algebra[ConsF, Type] = cons =>
      internal.typeRef(consPrefix, consSym, cons.head :: cons.tail :: Nil)
  }

  final lazy val buildTList: TypeListBuilder =
    new TypeListBuilder(
      weakTypeOf[TCons[_, _]].typeConstructor,
      weakTypeOf[TNil])

  final lazy val buildKList: TypeListBuilder =
    new TypeListBuilder(
      weakTypeOf[KCons[Nothing, _]].typeConstructor,
      weakTypeOf[KNil])
}

private[internal] sealed trait CoproductAPI { self: Toolbelt =>
  import u._

  case class CopTypes(L: Type)
  case class CopKTypes(L: Type, A: Type)

  private[this] final lazy val CopTpe =
    typeOf[iota.Cop[Nothing]].etaExpand.resultType
  private[this] final lazy val CopKTpe =
    typeOf[iota.CopK[Nothing, Nothing]].etaExpand.resultType

  private[this] def resultType(sym: Symbol): Type =
    sym.asType.toType.etaExpand.resultType

  final def destructCop(tpe: Type): Either[Id[String], CopTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: Nil) if resultType(sym) <:< CopTpe => Right(CopTypes(l))
      case TypeRef(_, sym, Nil) => destructCop(sym.asType.toType)
      case t => Left(s"unexpected type $t ${showRaw(t)} when destructuring Cop $tpe")
    }

  final def destructCopK(tpe: Type): Either[Id[String], CopKTypes] =
    tpe.dealias match {
      case TypeRef(_, sym, l :: a :: Nil) if resultType(sym) <:< CopKTpe => Right(CopKTypes(l, a))
      case TypeRef(_, sym, Nil) => destructCopK(sym.asType.toType)
      case t => Left(s"unexpected type $t ${showRaw(t)} when destructuring CopK $tpe")
    }

  private[this] def toSymbol(tpe: Type): Symbol = tpe match {
    case TypeRef(_, sym, Nil) => sym
    case _                    => tpe.typeSymbol
  }

  final def defineFastFunctionK(
    className: TypeName,
    F: Type, G: Type,
    preamble: List[Tree],
    toIndex: TermName => Tree,
    handlers: List[TermName => Tree]
  ): Tree = {

    val fa = TermName("fa")
    val A  = TypeName("Ξ")
    val cases = handlers.zipWithIndex
      .map { case (h, i) => cq"$i => ${h(fa)}" }
    val toStringValue = s"FastFunctionK[$F, $G]<<generated>>"

    q"""
    class $className extends _root_.iota.internal.FastFunctionK[$F, $G] {
      ..$preamble
      override def apply[$A]($fa: ${toSymbol(F)}[$A]): ${toSymbol(G)}[$A] =
        (${toIndex(fa)}: @_root_.scala.annotation.switch) match {
          case ..$cases
          case i => throw new _root_.java.lang.Exception(
            s"iota internal error: index " + i + " out of bounds for " + this)
        }
      override def toString: String = $toStringValue
    }
    """
  }
}

private[internal] sealed trait MacroAPI { self: MacroToolbelt with TypeListEvaluators =>
  import u._

  lazy val showAborts =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowAborts], true).isEmpty

  lazy val showCache =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowCache], true).isEmpty

  lazy val showTrees =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowTrees], true).isEmpty

  def foldAbort[F[_]: Foldable, T](
    either: Either[F[String], Tree],
    isImplicit: Boolean = false
  ): c.Expr[T] =
    either fold (
      errors => {
        val error = errors.toList.mkString(", and\n")
        if (isImplicit && showAborts) c.echo(c.enclosingPosition, error)
        c.abort(c.enclosingPosition, error)
      },
      tree  => {
        if (showTrees) c.echo(c.enclosingPosition, showCode(tree))
        c.Expr[T](tree)
      })

  def memoize[A, B](cache: IotaMacroToolbelt.Cache)(
    a: A, f: A => B
  ): B = cache.underlying.synchronized {
    cache.underlying.get(a) match {
      case Some(b: B @unchecked) =>
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache: $b cached result $b")
        b
      case _ =>
        val b = f(a)
        cache.underlying += ((a, b))
        if (showCache)
          c.echo(c.enclosingPosition, s"ShowCache: $a computed result $b")
        b
    }
  }

  def memoizedTListTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlistTypes)

  def memoizedKListTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, klistTypes)
}
