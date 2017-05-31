lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(examplesJVM, examplesJS)
  .aggregate(bench)

lazy val core = module("core", true)
  .settings(scalaMacroDependencies)
  .settings(yax(file("modules/core/src/main/scala"), Compile,
    yaxScala = true))
  .settings(yax(file("modules/core/src/test/scala"), Test,
    yaxPlatform = true))
  .crossDepSettings(
    %%("cats-core"),
    %%("cats-free"),
    %%("scalacheck")      % "test",
    %%("shapeless")       % "test",
    %%("scheckShapeless") % "test")

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val examples = module("examples")
  .dependsOn(core)
  .settings(noPublishSettings)

lazy val examplesJVM = examples.jvm
lazy val examplesJS  = examples.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(readmeSettings)

lazy val bench = jvmModule("bench")
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM)
  .configs(Codegen)
  .settings(inConfig(Codegen)(Defaults.configSettings))
  .settings(classpathConfiguration in Codegen := Compile)
  .settings(noPublishSettings)
  .settings(libraryDependencies ++= Seq(
    %%("cats-free"),
    %%("scalacheck")))
  .settings(inConfig(Compile)(
    sourceGenerators += Def.task {
      val path = ((sourceManaged in (Compile, compile)).value / "bench.scala")
      (runner in (Codegen, run)).value.run(
        "iota.bench.BenchBoiler",
        Attributed.data((fullClasspath in Codegen).value),
        path.toString :: Nil,
        streams.value.log)
      path :: Nil
    }
  ))

lazy val Codegen = config("codegen").hide

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.asc")
pgpSecretRing := file(s"$gpgFolder/secring.asc")
