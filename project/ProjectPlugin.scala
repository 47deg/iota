import sbt.Keys._
import sbt._
import freestyle.FreestylePlugin
import sbtorgpolicies.model._
import sbtorgpolicies.OrgPoliciesKeys.orgBadgeListSetting
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.templates.badges._
import sbtorgpolicies.templates._
import sbtorgpolicies.runnable.syntax._
import scoverage.ScoverageKeys._
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
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

    lazy val readmeSettings: Seq[Def.Setting[_]] = Seq(
      scalacOptions in Tut := Nil,
      tutSourceDirectory :=
        (baseDirectory in LocalRootProject).value / "modules" / "readme" / "src" / "main" / "tut",
      tutTargetDirectory := baseDirectory.value.getParentFile.getParentFile
    )
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
    orgEnforcedFilesSetting ~= (_ filterNot (_ == ScalafmtFileType)),

    orgMaintainersSetting += Dev("andyscott", Some("Andy Scott (twitter: [@andygscott](https://twitter.com/andygscott))"), Some("andy.g.scott@gmail.com")),

    coverageFailOnMinimum := false,
    fork in Test := !isScalaJSProject.value,
    parallelExecution in Test := false,
    outputStrategy := Some(StdoutOutput),
    connectInput in run := true,
    cancelable in Global := true,

    crossScalaVersions :=  List("2.11.11", "2.12.3"),
    scalaVersion       := "2.12.3",

    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Yno-predef",
      "-Ypartial-unification"),

    scalacOptions := (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => scalacOptions.value
      case _             => scalacOptions.value.filter(_ != "-Xfatal-warnings")
    }),

    scalacOptions in (Compile, doc) :=
      (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),

    scalacOptions in (Compile, console) ~= (_.filterNot(Set(
      "-Ywarn-unused:imports",
      "-Ywarn-unused-import",
      "-Xfatal-warnings"
    ))),

    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
  )

}
