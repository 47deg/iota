lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(testsJVM, testsJS)
  .aggregate(examplesJVM, examplesJS)
  .aggregate(bench)

lazy val core = module("core", hideFolder = true)
  .settings(macroSettings)
  .settings(yax(file("modules/core/src/main/scala"), Compile,
    yaxScala = true))
  .crossDepSettings(
    %%("cats-core", V.cats),
    %%("cats-free", V.cats)
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val tests = module("tests", hideFolder = true)
  .dependsOn(core)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(yax(file("modules/tests/src/test/scala"), Test,
    yaxPlatform = true))
  .settings(
    scalaOrganization := "org.typelevel",
    scalaVersion      := "2.12.3-bin-typelevel-4",
    scalacOptions     += "-Yliteral-types")
  .settings(
    libraryDependencies := libraryDependencies.value.map { d =>
      if (d.name != "scalajs-compiler") d
      else d.cross(CrossVersion.patch) })
  .crossDepSettings(
    %%("scalacheck")      % "test",
    %%("shapeless")       % "test",
    %%("scheckShapeless") % "test")

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

lazy val examples = module("examples")
  .dependsOn(core)
  .settings(noPublishSettings)
  .settings(macroSettings)

lazy val examplesJVM = examples.jvm
lazy val examplesJS  = examples.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(readmeSettings)
  .settings(macroSettings)

lazy val bench = jvmModule("bench")
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM)
  .configs(Codegen)
  .settings(inConfig(Codegen)(Defaults.configSettings))
  .settings(classpathConfiguration in Codegen := Compile)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(libraryDependencies ++= Seq(
    %%("scalacheck")))
//   .settings(inConfig(Compile)(
//     sourceGenerators += Def.task {
//       val path = (sourceManaged in(Compile, compile)).value / "bench.scala"
//       (runner in (Codegen, run)).value.run(
//         "iota.bench.BenchBoiler",
//         Attributed.data((fullClasspath in Codegen).value),
//         path.toString :: Nil,
//         streams.value.log)
//       path :: Nil
//     }
//   ))

lazy val Codegen = config("codegen").hide

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.asc")
pgpSecretRing := file(s"$gpgFolder/secring.asc")

lazy val V = new {
  val cats = "1.0.0-MF"
}

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)))
