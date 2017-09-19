import sbt._
import sbt.Keys._

/** Assigns the correct version of TLS scala if you're scala org
  * is set to "org.typelevel".
  */
object AutoTLSPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def projectSettings =
    Seq(commands ~= { existing =>
      Seq(AutoTLSCross.overrideSwitchCommand) ++ existing
    })
}
