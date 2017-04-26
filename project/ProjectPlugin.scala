import sbt.Keys._
import sbt._
import sbtorgpolicies.model._
import sbtorgpolicies.OrgPoliciesKeys.orgBadgeListSetting
import sbtorgpolicies.OrgPoliciesPlugin
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.templates.badges._
import sbtorgpolicies.templates._
import sbtorgpolicies.runnable.syntax._
import scoverage.ScoverageKeys._
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import tut.Plugin._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = OrgPoliciesPlugin

  object autoImport {

    def module(modName: String): CrossProject =
      CrossProject(modName, file(s"""modules/$modName"""), CrossType.Pure)
        .settings(moduleName := s"iota-$modName")

    def jvmModule(modName: String): Project =
      Project(modName, file(s"""modules/$modName"""))
        .settings(moduleName := s"iota-$modName")

    lazy val commonCrossDeps = Seq(
      %%("cats-core"),
      %%("cats-free"),
      %%("scalacheck") % "test",
      %%("scheckShapeless") % "test"
    )

    lazy val crossVersionSharedSources: Seq[Setting[_]] =
      Seq(Compile, Test).map { sc =>
        (unmanagedSourceDirectories in sc) ++= {
          (unmanagedSourceDirectories in sc ).value.flatMap { dir: File =>
            CrossVersion.partialVersion(scalaVersion.value) match {
              case Some((2, y)) => Some(new File(dir.getPath + "_2.11"))
              case _            => None
            }
          }
        }
      }

    lazy val readmeSettings: Seq[Def.Setting[_]] = tutSettings ++ Seq(
      tutScalacOptions := Nil,
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

    orgMaintainersSetting += Dev("andyscott", Some("Andy Scott")),
    orgBadgeListSetting := List(
      TravisBadge.apply(_),
      MavenCentralBadge.apply(_),
      LicenseBadge.apply(_),
      ScalaLangBadge.apply(_),
      ScalaJSBadge.apply(_),
      GitHubIssuesBadge.apply(_)
    ),
    orgSupportedScalaJSVersion := Some("0.6.15"),
    orgScriptTaskListSetting := List(
      orgValidateFiles.asRunnableItem,
      "clean".asRunnableItemFull,
      "compile".asRunnableItemFull,
      "test".asRunnableItemFull,
      "tutReadme".asRunnableItem
    ),
    orgEnforcedFilesSetting ~= (_ filterNot (_ == ScalafmtFileType)),

    coverageFailOnMinimum := false,
    fork in run := true,
    fork in Test := !isScalaJSProject.value,
    parallelExecution in Test := false,
    outputStrategy := Some(StdoutOutput),
    connectInput in run := true,
    cancelable in Global := true,

    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Yno-predef",
      "-Ypartial-unification"),

    scalacOptions in (Compile, doc) :=
      (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings")
  )

}
