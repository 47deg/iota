package iota  //#=cats
package iotaz //#=scalaz

import cats._  //#=cats
import scalaz._ //#=scalaz

import TListK.::

package object quasi {

  type Quasi [S[_], E, A] = quasiImpl.Quasi [S, E, A]
  type Concur[S[_], E, A] = quasiImpl.Concur[S, E, A]
  type Subseq[S[_], E, A] = quasiImpl.Subseq[S, E, A]

  implicit final class QuasiOps[S[_], E, A](val quasi: Quasi[S, E, A]) extends AnyVal {
    def concur: Concur[S, E, A] = quasiImpl.toConcur(quasi)
    def subseq: Subseq[S, E, A] = quasiImpl.toSubseq(quasi)
  }

  final implicit class ConcurOps[S[_], E, A](val concur: Concur[S, E, A]) extends AnyVal {
    def quasi: Quasi[S, E, A] = quasiImpl.fromConcur(concur)
    def subseq: Subseq[S, E, A] = quasi.subseq

    def ap[B](f: Concur[S, E, A => B]): Concur[S, E, B] =
      quasiImpl.ap(f.quasi)(concur.quasi).concur

    def map[B](f: A => B): Concur[S, E, B] = ap(Quasi.pure(f).concur)
  }

  final implicit class SubseqOps[S[_], E, A](val subseq: Subseq[S, E, A]) extends AnyVal {
    def quasi: Quasi[S, E, A] = quasiImpl.fromSubseq(subseq)
    def concur: Concur[S, E, A] = quasi.concur

    def map[B](f: A => B): Subseq[S, E, B] =
      flatMap(a => Quasi.pure(f(a)).subseq)

    def flatMap[B](f: A => Subseq[S, E, B]): Subseq[S, E, B] =
      quasiImpl.flatMap(subseq.quasi)(f.andThen(_.quasi)).subseq
  }

  implicit def subseqMonadError[S[_], E]: MonadError[Subseq[S, E, ?], E] = new MonadError[Subseq[S, E, ?], E] {
    def pure[A](a: A): Subseq[S, E, A] = Quasi.pure(a).subseq
    def flatMap[A, B](fa: Subseq[S, E, A])(f: A => Subseq[S, E, B]): Subseq[S, E, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Subseq[S, E, Either[A, B]]): Subseq[S, E, B] = ???

    def handleErrorWith[A](fa: Subseq[S, E, A])(f: E => Subseq[S, E, A]): Subseq[S, E, A] =
      quasiImpl.guard(fa.quasi, f.andThen(_.quasi)).subseq

    def raiseError[A](e: E): Subseq[S, E, A] =
      quasiImpl.raise(e).subseq
  }

  implicit def concurApplicative[S[_], E]: Applicative[Concur[S, E, ?]] = new Applicative[Concur[S, E, ?]] {
    def pure[A](a: A): Concur[S, E, A] = Quasi.pure(a).concur
    def ap[A, B](ff: Concur[S, E, A => B])(fa: Concur[S, E, A]): Concur[S, E, B] =
      fa.ap(ff)
  }

  implicit def subseqConcurParallel[S[_], E]: Parallel[Subseq[S, E, ?], Concur[S, E, ?]] =
    new Parallel[Subseq[S, E, ?], Concur[S, E, ?]] {
      val parallel: Subseq[S, E, ?] ~> Concur[S, E, ?] =
        λ[Subseq[S, E, ?] ~> Concur[S, E, ?]](_.quasi.concur)
      val sequential: Concur[S, E, ?] ~> Subseq[S, E, ?] =
        λ[Concur[S, E, ?] ~> Subseq[S, E, ?]](_.quasi.subseq)
      val applicative: Applicative[Concur[S, E, ?]] = Applicative[Concur[S, E, ?]]
      val monad: Monad[Subseq[S, E, ?]] = Monad[Subseq[S, E, ?]]
    }

  object Quasi {

    def pure[S[_], E, A](a: A): Quasi[S, E, A] = quasiImpl.pure(a)
    def liftF[S[_], E, A](value: S[A]): Quasi[S, E, A] = quasiImpl.suspend(value)

    def toConcur[S[_], E]: Quasi[S, E, ?] ~> Concur[S, E, ?] =
      λ[Quasi[S, E, ?] ~> Concur[S, E, ?]](_.concur)

    def toSubseq[S[_], E]: Quasi[S, E, ?] ~> Subseq[S, E, ?] =
      λ[Quasi[S, E, ?] ~> Subseq[S, E, ?]](_.subseq)

    def foldMap[S[_], Zm[_], Za[_], E, A]
      (quasi: Quasi[S, E, A])(f: S ~> Zm)(implicit Z: Parallel[Zm, Za], E: MonadError[Zm, E]): Zm[A] =
        quasiImpl.evaluator(
          f,
          Z.parallel,
          Z.sequential,
          Z.monad,
          Z.applicative,
          λ[λ[α => (Zm[α], E => Zm[α])] ~> Zm](n => E.handleErrorWith(n._1)(n._2)),
          λ[λ[α => E] ~> Zm](E.raiseError(_)))(quasi)
  }

  private[quasi] sealed trait QuasiImpl {
    type Quasi [S[_], E, A]
    type Concur[S[_], E, A]
    type Subseq[S[_], E, A]

    type Effects[S[_], E] =
      Pure    [         ?] ::
      Suspend [S,       ?] ::
      FlatMap [S, E, _, ?] ::
      Ap      [S, E, _, ?] ::
      Guard   [S, E,    ?] ::
      Raise   [S, E,    ?] ::
      TNilK

    type Pure[A] = A
    type Suspend[S[_], A] = S[A]
    final case class FlatMap[S[_], E, A, B](fa: Quasi[S, E, A], f: A => Quasi[S, E, B])
    final case class Ap[S[_], E, A, B](ff: Quasi[S, E, A => B], fa: Quasi[S, E, A])
    final case class Guard[S[_], E, A](fa: Quasi[S, E, A], f: E => Quasi[S, E, A])
    type Raise[S[_], E, A] = E

    def toRaw[S[_], E, A](quasi: Quasi[S, E, A]): CopK[Effects[S, E], A]
    def fromRaw[S[_], E, A](copK: CopK[Effects[S, E], A]): Quasi[S, E, A]

    def toConcur[S[_], E, A](quasi: Quasi[S, E, A]): Concur[S, E, A]
    def fromConcur[S[_], E, A](subseq: Concur[S, E, A]): Quasi[S, E, A]

    def toSubseq[S[_], E, A](quasi: Quasi[S, E, A]): Subseq[S, E, A]
    def fromSubseq[S[_], E, A](subseq: Subseq[S, E, A]): Quasi[S, E, A]

    def pure   [S[_], E, A](a: A): Quasi[S, E, A]
    def suspend[S[_], E, A](value: S[A]): Quasi[S, E, A]
    def flatMap[S[_], E, A, B](fa: Quasi[S, E, A])(f: A => Quasi[S, E, B]): Quasi[S, E, B]
    def ap     [S[_], E, A, B](ff: Quasi[S, E, A => B])(fa: Quasi[S, E, A]): Quasi[S, E, B]
    def guard  [S[_], E, A](fa: Quasi[S, E, A], f: E => Quasi[S, E, A]): Quasi[S, E, A]
    def raise  [S[_], E, A](e: E): Quasi[S, E, A]

    type Evaluator[S[_], M[_], E] = Quasi[S, E, ?] ~> M

    def evaluator[S[_], Zm[_], Za[_], E](
      f         : S  ~> Zm,
      parallel  : Zm ~> Za,
      sequential: Za ~> Zm,
      Zm        : Monad[Zm],
      Za        : Applicative[Za],
      guard     : λ[α => (Zm[α], E => Zm[α])] ~> Zm,
      raise     : λ[α => E] ~> Zm
    ): Evaluator[S, Zm, E]
  }

  private[quasi] val quasiImpl: QuasiImpl = new QuasiImpl {
    type Quasi [S[_], E, A] = CopK[Effects[S, E], A]
    type Concur[S[_], E, A] = CopK[Effects[S, E], A]
    type Subseq[S[_], E, A] = CopK[Effects[S, E], A]

    def toRaw[S[_], E, A](quasi: Quasi[S, E, A]): CopK[Effects[S, E], A] = quasi
    def fromRaw[S[_], E, A](copK: CopK[Effects[S, E], A]): Quasi[S, E, A] = copK

    def toConcur[S[_], E, A](quasi: Quasi[S, E, A]): Concur[S, E, A] = quasi
    def fromConcur[S[_], E, A](subseq: Concur[S, E, A]): Quasi[S, E, A] = subseq

    def toSubseq[S[_], E, A](quasi: Quasi[S, E, A]): Subseq[S, E, A] = quasi
    def fromSubseq[S[_], E, A](subseq: Subseq[S, E, A]): Quasi[S, E, A] = subseq

    def pure[S[_], E, A](a: A): Quasi[S, E, A] =
      CopK.unsafeApply[Effects[S, E], Pure, A](0, a)

    def suspend[S[_], E, A](value: S[A]): Quasi[S, E, A] =
      CopK.unsafeApply[Effects[S, E], Suspend[S, ?], A](1, value)

    def flatMap[S[_], E, A, B](fa: Quasi[S, E, A])(f: A => Quasi[S, E, B]): Quasi[S, E, B] =
      CopK.unsafeApply[Effects[S, E], FlatMap[S, E, A, ?], B](2, FlatMap[S, E, A, B](fa, f))

    def ap[S[_], E, A, B](ff: Quasi[S, E, A => B])(fa: Quasi[S, E, A]): Quasi[S, E, B] =
      CopK.unsafeApply[Effects[S, E], Ap[S, E, A, ?], B](3, Ap[S, E, A, B](ff, fa))

    def guard[S[_], E, A](fa: Quasi[S, E, A], f: E => Quasi[S, E, A]): Quasi[S, E, A] =
      CopK.unsafeApply[Effects[S, E], Guard[S, E, ?], A](4, Guard[S, E, A](fa, f))

    def raise[S[_], E, A](e: E): Quasi[S, E, A] =
      CopK.unsafeApply[Effects[S, E], Raise[S, E, ?], A](5, e)

    def evaluator[S[_], Zm[_], Za[_], E](
      f         : S  ~> Zm,
      parallel  : Zm ~> Za,
      sequential: Za ~> Zm,
      Zm        : Monad[Zm],
      Za        : Applicative[Za],
      guard     : λ[α => (Zm[α], E => Zm[α])] ~> Zm,
      raise     : λ[α => E] ~> Zm
    ): Evaluator[S, Zm, E] = new Evaluator[S, Zm, E] {
      def apply[A](quasi: Quasi[S, E, A]): Zm[A] =
        Zm.tailRecM(quasi)(q => (q.index: @scala.annotation.switch) match {
          case 0 =>
            val a: A = q.value.asInstanceOf[A]
            Zm.pure(Right(a))
          case 1 =>
            val sa: S[A] = q.value.asInstanceOf[S[A]]
            Zm.map(f(sa))(Right(_))
          case 2 =>
            val n: FlatMap[S, E, Any, A] = q.value.asInstanceOf[FlatMap[S, E, Any, A]]
            Zm.map(this(n.fa))(z => Left(n.f(z)))
          case 3 =>
            val n: Ap[S, E, Any, A] = q.value.asInstanceOf[Ap[S, E, Any, A]]
            Zm.map(
              sequential(Za.ap(
                parallel(this(n.ff)))(
                parallel(this(n.fa))))
            )(Right(_))
          case 4 =>
            val n: Guard[S, E, A] = q.value.asInstanceOf[Guard[S, E, A]]
            Zm.map(guard((this(n.fa), n.f.andThen(this(_)))))(Right(_))
          case 5 =>
            val e: E = q.value.asInstanceOf[E]
            Zm.map(raise(e))(Right(_))
          case _ =>
            sys.error("unreachable internal state")
        })
    }

  }

}

// example
//#+cats
import cats.implicits._
package quasi {

  object Example {

    def main(args: Array[String]): Unit = {

      trait MathOp[A]
      case class ConstInt(value: Int) extends MathOp[Int]
      case class Add(x: Int, y: Int) extends MathOp[Int]
      case class Div(x: Int, y: Int) extends MathOp[Int]
      case class Neg(x: Int) extends MathOp[Int]

      trait Math[F[_]] { underlying =>
        def const(value: Int): F[Int]
        def add(x: Int, y: Int): F[Int]
        def div(x: Int, y: Int): F[Int]
        def neg(x: Int): F[Int]

        final def mapK[G[_]](f: F ~> G): Math[G] = new Math[G] {
          def const(value: Int): G[Int] = f(underlying.const(value))
          def add(x: Int, y: Int): G[Int] = f(underlying.add(x, y))
          def div(x: Int, y: Int): G[Int] = f(underlying.div(x, y))
          def neg(x: Int): G[Int] = f(underlying.neg(x))
        }
      }

      object Math {
        def quasi: Math[Quasi[MathOp, Throwable, ?]] = new Math[Quasi[MathOp, Throwable, ?]] {
          def const(value: Int): Quasi[MathOp, Throwable, Int] = Quasi.liftF(ConstInt(value))
          def add(x: Int, y: Int): Quasi[MathOp, Throwable, Int] = Quasi.liftF(Add(x, y))
          def div(x: Int, y: Int): Quasi[MathOp, Throwable, Int] = Quasi.liftF(Div(x, y))
          def neg(x: Int): Quasi[MathOp, Throwable, Int] = Quasi.liftF(Neg(x))
        }

        def concur: Math[Concur[MathOp, Throwable, ?]] = quasi.mapK[Concur[MathOp, Throwable, ?]](Quasi.toConcur)
        def subseq: Math[Subseq[MathOp, Throwable, ?]] = quasi.mapK[Subseq[MathOp, Throwable, ?]](Quasi.toSubseq)
      }

      import scala.util.Try
      implicit val parallelTry: Parallel[Try, Try] = Parallel.identity

      val interp: MathOp ~> Try = λ[MathOp ~> Try] {
        case ConstInt(value) => Try(value)
        case Add(x, y) => Try(x + y)
        case Div(x, y) => Try(x / y)
        case Neg(x) => Try(-x)
      }

      val math = Math.subseq

      val program0 = for {
        x <- math.const(1)
        y <- math.const(2)
        z <- math.add(x, y)
      } yield z + 10

      val program1 = for {
        a <- math.const(100)
        b <- math.neg(a)
      } yield a + b

      val program2 = for {
        foo <- math.const(0)
        bar <- List(program0, program1).parSequence
      } yield bar.foldLeft(foo)(_ / _)

      val program3 = program2.handleErrorWith(_ => math.const(-100))

      scala.Predef.println("program:")
      scala.Predef.println(program3)

      val res = Quasi.foldMap(program3.quasi)(interp)
      scala.Predef.println("res:")
      scala.Predef.println(res)

    }

  }
}
//#-cats
