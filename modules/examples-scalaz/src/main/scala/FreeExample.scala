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

import iotaz._

import scalaz._
import scalaz.effect._
import scalaz.syntax.apply._

import scala.Predef._

sealed trait TerminalOp[A]
object TerminalOp {
  case object ReadLine extends TerminalOp[String]
  final case class WriteLine(msg: String) extends TerminalOp[Unit]
}

sealed trait CounterOp[A]
object CounterOp {
  final case object Reset     extends CounterOp[Unit]
  final case object Increment extends CounterOp[Unit]
  final case object Decrement extends CounterOp[Unit]
  final case object Read      extends CounterOp[Long]
}

object FreeExample extends SafeApp {
  import TListK.::

  type Algebra[A]     = CopK[TerminalOp :: CounterOp :: TNilK, A]
  type FreeProgram[A] = Free[Algebra, A]
  type Program[A]     = StateT[IO, Long, A]

  val terminalToIO: TerminalOp ~> IO =
    λ[TerminalOp ~> IO] {
      case TerminalOp.ReadLine       => IO.readLn
      case TerminalOp.WriteLine(msg) => IO.putStrLn(msg)
    }
  val counterToState: CounterOp ~> State[Long, ?] =
    λ[CounterOp ~> State[Long, ?]] {
      case CounterOp.Reset     => State.put(0)
      case CounterOp.Increment => State.modify(_ + 1)
      case CounterOp.Decrement => State.modify(_ - 1)
      case CounterOp.Read      => State.get
    }

  val stateToProgram = λ[State[Long, ?] ~> Program](_.mapT(v => IO(v)))
  val ioToProgram    = λ[IO ~> Program](io => StateT(s => io.map(s -> _)))
  val interpreter: Algebra ~> Program =
    CopK.NaturalTransformation.of[Algebra, Program](
      counterToState andThen stateToProgram,
      terminalToIO andThen ioToProgram)

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(
    implicit ev: CopK.Inject[F, Algebra]
  ) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  val terminate: FreeProgram[Unit] = Free.point(())
  lazy val freeProgram: FreeProgram[Unit] =
    for {
      value <- CounterOp.Read.liftFree
      _     <- TerminalOp.WriteLine(s"counter is $value").liftFree
      _     <- TerminalOp.WriteLine("command [+/-/0/exit]").liftFree
      input <- TerminalOp.ReadLine.liftFree
      _ <- input.trim.toLowerCase match {
        case "exit" => TerminalOp.WriteLine("See ya!").liftFree *> terminate
        case "+"    => CounterOp.Increment.liftFree *> freeProgram
        case "-"    => CounterOp.Decrement.liftFree *> freeProgram
        case "0"    => CounterOp.Reset.liftFree *> freeProgram
        case _      => TerminalOp.WriteLine("Invalid command!").liftFree *> freeProgram
      }
    } yield ()

  override val runc: IO[Unit] =
    freeProgram.foldMap(interpreter).run(0).map(_ => ())
}
