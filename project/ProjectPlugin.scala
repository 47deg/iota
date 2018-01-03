import sbt.Keys._
import sbt._
import freestyle.FreestylePlugin
import sbtorgpolicies.model._
import sbtorgpolicies.OrgPoliciesKeys.orgBadgeListSetting
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.templates.badges._
import sbtorgpolicies.templates._
import sbtorgpolicies.runnable.syntax._
import dependencies.DependenciesPlugin.autoImport._
import scoverage.ScoverageKeys._
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import tut.TutPlugin.autoImport._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = FreestylePlugin

  object autoImport {

    def module(
      modName: String, hideFolder: Boolean = false, prefixSuffix: String = ""
    ): CrossProject =
      CrossProject(
        s"$modName$prefixSuffix",
        file(s"""modules/${if (hideFolder) "." else ""}$modName$prefixSuffix"""),
        CrossType.Pure
      )
        .settings(moduleName := s"iota${prefixSuffix}-$modName")

    def jvmModule(modName: String): Project =
      Project(modName, file(s"""modules/$modName"""))
        .settings(moduleName := s"iota-$modName")
  }

  lazy val commandAliases: Seq[Def.Setting[_]] = addCommandAlias("tutReadme", ";project readme;tut;project root")

  override def projectSettings: Seq[Def.Setting[_]] = commandAliases ++ Seq(

    name := "iota",
    orgProjectName := "Iota",
    description := "fast product/coproduct types",
    startYear := Option(2016),

    orgBadgeListSetting := List(
      TravisBadge.apply(_),
      MavenCentralBadge.apply(_),
      LicenseBadge.apply(_),
      ScalaLangBadge.apply(_),
      ScalaJSBadge.apply(_),
      GitHubIssuesBadge.apply(_)
    ),

    orgScriptTaskListSetting := List(
      orgValidateFiles.asRunnableItem,
      "clean".asRunnableItemFull,
      "compile".asRunnableItemFull,
      "test".asRunnableItemFull,
      "tutReadme".asRunnableItem
    ),
    orgUpdateDocFilesSetting +=
      (baseDirectory in LocalRootProject).value / "modules" / "readme" / "src" / "main" / "tut",
    orgEnforcedFilesSetting := Nil,

    orgAfterCISuccessTaskListSetting := List(
      depUpdateDependencyIssues.asRunnableItem,
      orgPublishReleaseTask.asRunnableItem(allModules = true, aggregated = false, crossScalaVersions = true),
      orgUpdateDocFiles.asRunnableItem
    ),

    headerCreate in Compile := Nil,
    headerCreate in Test := Nil,
    orgMaintainersSetting += Dev("andyscott", Some("Andy Scott (twitter: [@andygscott](https://twitter.com/andygscott))"), Some("andy.g.scott@gmail.com")),

    coverageFailOnMinimum := false,
    fork in Test := !isScalaJSProject.value,
    parallelExecution in Test := false,
    outputStrategy := Some(StdoutOutput),
    connectInput in run := true,
    cancelable in Global := true,

    crossScalaVersions :=  List("2.11.12", "2.12.4"),
    scalaVersion       := "2.12.4"
  )

}
