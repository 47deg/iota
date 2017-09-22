import sbt._
import sbt.Keys._
import sbt.internal.util.complete.DefaultParsers._
import sbt.internal.util.complete.{DefaultParsers, Parser}
import sbt.internal.CommandStrings._
import sbt.Def.{ ScopedKey, Setting }

/** Assigns the correct version of TLS scala if you're scala org
  * is set to "org.typelevel".
  */
object AutoTLSPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def projectSettings = Seq(
    commands ~= { existing => Seq(overrideSwitchCommand) ++ existing })

  def overrideSwitchCommand: Command =
    Command.arb(requireSession(switchParser), switchHelp)(switchCommandImpl _)

  def switchCommandImpl(state: State, args: Switch): State = {
    val version = args.version match {
      case NamedScalaVersion(v, _) => v
      case _ =>sys.error("failure")
    }

    val (fixedState, _) = updateState(state, version)

    args.command.toList ::: fixedState
  }

  // based off of SBT's Cross.switchVersion, and sbt-doge...
  // except a lof of the logic has been ripped out, because we don't ever set
  // scala home paths in the iota build
  private def updateState(state: State, version: String): (State, String) = {
    val x = Project.extract(state)
    import x._
    val aggs = aggregate(state)

    val exludeCurrentAndAgg = excludeProjects((currentRef :: aggs.toList).toSet)

    state.log.info("Setting version to " + version)

    val updated: Seq[Setting[_]] = session.mergeSettings
      .filter(exludeCurrentAndAgg)
      .collect { case x if hasKey(x, scalaVersion.key) =>
        val scope = x.key.scope
        SettingKey(scalaVersion.key) in scope := {
          if ((scalaOrganization in scope).value == "org.typelevel")
            tlsVersion(version)
          else version
        }
      }

    val fixedSession = session.appendRaw(updated)
    (BuiltinCommands.reapply(fixedSession, structure, state), version)
   }

  private[this] def aggregate(state: State): Seq[ProjectRef] = {
    val x = Project.extract(state)
    import x._
    currentProject.aggregate
  }

  private[this] def tlsVersion(version: String): String = version match {
    case "2.12.3"  => "2.12.3-bin-typelevel-4"
    case "2.11.11" => "2.11.11-bin-typelevel-4"
    case other     => other
  }

  private[this] def hasKey(s: Setting[_], searchKey: AttributeKey[_]): Boolean =
    s.key match {
      case ScopedKey(Scope(_, Zero, Zero, _), key) if key == searchKey => true
      case _ => false
    }

  private[this] def excludeProjects(projs: Set[Reference]): Setting[_] => Boolean =
    _.key match {
      case ScopedKey(Scope(Select(pr), _, _, _), _) if projs.contains(pr) => true
      case _ => false
    }

  /** The code below this comment is copy pasted from
   * https://github.com/sbt/sbt/blob/v1.0.1/main/src/main/scala/sbt/Cross.scala
   * with the following license https://github.com/sbt/sbt/blob/v1.0.1/LICENSE.
   * This code is copied because there is not an easy way to achieve the same
   * effect, to override the switchCommand, due to the required methods being
   * private in Cross.scala.
   */
  private def spacedFirst(name: String) = opOrIDSpaced(name) ~ any.+

  private case class Switch(version: ScalaVersion, verbose: Boolean, command: Option[String])
  private trait ScalaVersion {
    def force: Boolean
  }
  private case class NamedScalaVersion(name: String, force: Boolean) extends ScalaVersion
  private case class ScalaHomeVersion(home: File, resolveVersion: Option[String], force: Boolean)
      extends ScalaVersion

  private def switchParser(state: State): Parser[Switch] = {
    import DefaultParsers._
    def versionAndCommand(spacePresent: Boolean) = {
      val x = Project.extract(state)
      import x._
      val knownVersions = crossVersions(x, currentRef)
      val version = token(StringBasic.examples(knownVersions: _*)).map { arg =>
        val force = arg.endsWith("!")
        val versionArg = if (force) arg.dropRight(1) else arg
        versionArg.split("=", 2) match {
          case Array(home) if new File(home).exists() =>
            ScalaHomeVersion(new File(home), None, force)
          case Array(v) => NamedScalaVersion(v, force)
          case Array(v, home) =>
            ScalaHomeVersion(new File(home), Some(v).filterNot(_.isEmpty), force)
        }
      }
      val spacedVersion = if (spacePresent) version else version & spacedFirst(SwitchCommand)
      val verbose = Parser.opt(token(Space ~> "-v"))
      val optionalCommand = Parser.opt(token(Space ~> matched(state.combinedParser)))
      (spacedVersion ~ verbose ~ optionalCommand).map {
        case v ~ verbose ~ command =>
          Switch(v, verbose.isDefined, command)
      }
    }

    token(SwitchCommand ~> OptSpace) flatMap { sp =>
      versionAndCommand(sp.nonEmpty)
    }
  }

  private def crossVersions(extracted: Extracted, proj: ProjectRef): Seq[String] = {
    import extracted._
    (crossScalaVersions in proj get structure.data) getOrElse {
      // reading scalaVersion is a one-time deal
      (scalaVersion in proj get structure.data).toSeq
    }
  }

  private def requireSession[T](p: State => Parser[T]): State => Parser[T] =
    s => if (s get sessionSettings isEmpty) failure("No project loaded") else p(s)
}
