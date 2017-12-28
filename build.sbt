lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(testsJVM, testsJS)
  .aggregate(examplesCatsJVM, examplesCatsJS)
  .aggregate(examplesScalazJVM, examplesScalazJS)
  .aggregate(bench)
  .aggregate(corezJVM, corezJS)
  .aggregate(testszJVM, testszJS)
  .aggregate(readme, docs)

lazy val core = module("core", hideFolder = true)
  .settings(macroSettings)
  .settings(yax(file("modules/core/src/main/scala"), Compile,
    flags    = "cats" :: Nil,
    yaxScala = true))
  .crossDepSettings(
    %%("cats-core", "1.0.0"),
    %%("cats-free", "1.0.0"))

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val corez = module("core", hideFolder = true, prefixSuffix = "z")
  .settings(macroSettings)
  .settings(yax(file("modules/core/src/main/scala"), Compile,
    flags    = "scalaz" :: Nil,
    yaxScala = true))
  .crossDepSettings(
    "org.scalaz" %% "scalaz-core" % "7.2.15")

lazy val corezJVM = corez.jvm
lazy val corezJS  = corez.js

lazy val tests = module("tests", hideFolder = true)
  .dependsOn(core)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(yax(file("modules/tests/src/main/scala"), Compile,
    flags       = "cats" :: Nil,
    yaxPlatform = true))
  .settings(yax(file("modules/tests/src/test/scala"), Test,
    flags       = "cats" :: Nil,
    yaxPlatform = true))
  .crossDepSettings(
    %%("scalacheck")      % "test",
    %%("shapeless")       % "test",
    %%("scheckShapeless") % "test")

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

lazy val testsz = module("tests", hideFolder = true, prefixSuffix = "z")
  .dependsOn(corez)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(yax(file("modules/tests/src/main/scala"), Compile,
    flags       = "scalaz" :: Nil,
    yaxPlatform = true))
  .settings(yax(file("modules/tests/src/test/scala"), Test,
    flags       = "scalaz" :: Nil,
    yaxPlatform = true))
  .crossDepSettings(
    %%("scalacheck")      % "test",
    %%("shapeless")       % "test",
    %%("scheckShapeless") % "test")

lazy val testszJVM = testsz.jvm
lazy val testszJS  = testsz.js

lazy val examplesCats = module("examples-cats")
  .dependsOn(core)
  .settings(noPublishSettings)
  .settings(macroSettings)

lazy val examplesCatsJVM = examplesCats.jvm
lazy val examplesCatsJS  = examplesCats.js

lazy val examplesScalaz = module("examples-scalaz")
  .dependsOn(corez)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .crossDepSettings(
    "org.scalaz" %% "scalaz-effect" % "7.2.15")

lazy val examplesScalazJVM = examplesScalaz.jvm
lazy val examplesScalazJS  = examplesScalaz.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .dependsOn(corezJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(
    scalacOptions in Tut := Nil,
    tutTargetDirectory := (baseDirectory in LocalRootProject).value)

lazy val docs = jvmModule("docs")
  .dependsOn(coreJVM)
  .dependsOn(corezJVM)
  .enablePlugins(TutPlugin)
  .settings(noPublishSettings)
  .settings(macroSettings)
  .settings(
    scalacOptions in Tut := Nil,
    tutTargetDirectory := (baseDirectory in LocalRootProject).value / "docs")
  .settings(libraryDependencies +=
    "org.scalaz" %% "scalaz-effect" % "7.2.15")

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
  .settings(inConfig(Compile)(
    sourceGenerators += Def.task {
      val path = (sourceManaged in(Compile, compile)).value / "bench.scala"
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

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)))
