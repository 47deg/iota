package iota
package bench

import scala.annotation.tailrec
import scala.Predef._

object BenchBoiler {

  def main(args: Array[String]): Unit = {
    args.headOption match {
      case Some(path) =>
        val output = generate()
        val file = new java.io.File(path)
        file.getParentFile().mkdirs()
        file.createNewFile()
        val pw = new java.io.FileWriter(file)
        try {
          pw.write(output)
        } finally {
          pw.close()
        }
      case None       => println("expected single path argument")
    }
  }

  def generate(): String = {
    val names = (1 to 10).map(n => s"_$n").toList
    generate(names)
  }


  def generate(names: List[String]): String = {

    val catsAlgebras = foldTemplate(names, catsAlgebraHeadTemplate, catsAlgebraTailTemplate)
    val catsEvals    = foldTemplate(names, catsEvalHeadTemplate, catsEvalTailTemplate)

    val iotaAlgebras = foldTemplate(names, iotaAlgebraHeadTemplate, iotaAlgebraTailTemplate)
    val iotaEvals    =
      names.zipWithIndex.map {
        case (name, i) => iotaEvalTemplate(name, names.take(i + 1)) }

    val ops = names.map(opsTemplate)

    val genAlgebras =
      names.map(name => genAlgebraTemplate(name, names.headOption.toList))

    def benchdef(name: String, value: String): String =
      s"|  @Benchmark def $name: Any = $value"

    val benchmarks =
      names.map(name =>
        benchdef(s"cats$name", s"Cats.eval$name(Cats.genAlgebra$name.sample.get)")) :::
      names.map(name =>
        benchdef(s"iota$name", s"Iota.eval$name(Iota.genAlgebra$name.sample.get)"))

    s"""
      |package iota_bench
      |
      |import iota._
      |import cats._
      |import cats.data.{ State => _, _ }
      |import cats.free._
      |
      |import org.scalacheck._
      |
      |import java.util.concurrent.TimeUnit
      |import org.openjdk.jmh.annotations._
      |
      |@State(Scope.Thread)
      |@BenchmarkMode(Array(Mode.Throughput))
      |@OutputTimeUnit(TimeUnit.SECONDS)
      |class IotaBench {
      |
        ${benchmarks.mkString("\n")}
      |
      |}
      |
      |object Cats {
      |  import Ops._
      |
        ${catsAlgebras.mkString("\n")}

        ${catsEvals.mkString("\n")}
      |
      |  private[this] implicit class InjectGenOps[F[_]](gf: Gen[F[_]]) {
      |    def inj[G[_]](implicit ev: Inject[F, G]): Gen[G[_]] = gf.map(f => ev.inj(f))
      |  }
      |
        ${genAlgebras.mkString("\n")}
      |}
      |
      |object Iota {
      |  import Ops._
      |  import TListK.::
      |
        ${iotaAlgebras.mkString("\n")}

        ${iotaEvals.mkString("\n")}
      |
      |  private[this] implicit class InjectGenOps[F[_]](gf: Gen[F[_]]) {
      |    def inj[G[_] <: CopK[_, _]](implicit ev: CopK.Inject[F, G]): Gen[G[_]] = gf.map(f => ev.inj(f))
      |  }
      |
        ${genAlgebras.mkString("\n")}
      |}
      |
      |package Ops {
        ${ops.mkString("\n")}
      |}
    """.stripMargin

  }

  def foldTemplate(
    names: List[String],
    fHead: String => String,
    fTail: (String, String) => String
  ): List[String] = names match {
    case head :: tail =>
      @tailrec def loop(p: String, rem: List[String], acc: List[String]): List[String] =
        rem match {
          case h :: t => loop(h, t, fTail(p, h) :: acc)
          case Nil    => acc
        }
      loop(head, tail, fHead(head) :: Nil).reverse
    case Nil => Nil
  }

  def genAlgebraTemplate(
    algebraName: String,
    opNames: List[String]
  ): String = {

    def injOp(opName: String): String =
      s"${opName}Op.gen.inj[Algebra$algebraName]"

    val value = opNames match {
      case opName :: Nil => injOp(opName)
      case _             => s"""Gen.oneOf(${opNames.map(injOp).mkString(", ")})"""
    }
    s"|  val genAlgebra$algebraName: Gen[Algebra$algebraName[_]] = $value"
  }


  def catsAlgebraHeadTemplate(
    name: String
  ): String =
    s"|  type Algebra$name[A] = ${name}Op[A]"

  def catsAlgebraTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"|  type Algebra$name[A] = Coproduct[${name}Op, Algebra$prevName, A]"

  def catsEvalHeadTemplate(
    name: String
  ): String =
    s"|  val eval${name}: Algebra${name} ~> Id = ${name}Op.eval"

  def catsEvalTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"|  val eval$name: Algebra$name ~> Id = ${name}Op.eval or eval$prevName"

  def iotaAlgebraHeadTemplate(
    name: String
  ): String =
    s"""|  type TListK$name      = ${name}Op :: TNilK
        |  type Algebra$name[A] = CopK[TListK$name, A]"""

  def iotaAlgebraTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"""|  type TListK$name      = ${name}Op :: TListK$prevName
        |  type Algebra$name[A] = CopK[TListK$name, A]"""

  def iotaEvalTemplate(
    name: String,
    opNames: List[String]
  ): String =
    s"""|  val eval$name: Algebra$name ~> Id = CopK.FunctionK.summon"""


  def opsTemplate(
    name: String
  ): String =
s"""|  sealed trait ${name}Op[A]
    |  object ${name}Op {
    |    case class Op1(v: Int) extends ${name}Op[Int]
    |    implicit val eval: ${name}Op ~> Id = λ[${name}Op ~> Id] { case ${name}Op.Op1(v) => v + 1 }
    |    val gen: Gen[${name}Op[_]] = Gen.const(Op1(9000))
    |  }"""
}
