import sbt._
import sbt.Keys._

import Def.{ ScopedKey, Setting }

/** Assigns the correct version of TLS scala if you're scala org
  * is set to "org.typelevel".
  */
object AutoTLSPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def projectSettings = Seq(
    commands ~= { existing => Seq(overrideSwitchCommand) ++ existing })

  def overrideSwitchCommand: Command =
    Command.arb(Cross.requireSession(Cross.switchParser), CommandStrings.switchHelp)(switchCommandImpl)

  def switchCommandImpl(state: State, args: (String, String)): State = {
    val (arg, command) = args
    val (fixedState, version) = updateState(state, arg)

    if (!command.isEmpty) command :: fixedState
    else fixedState
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

}
