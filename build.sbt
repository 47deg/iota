lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(bench)

lazy val core = module("core")
  .settings(scalaMacroDependencies)
  .settings(crossVersionSharedSources)
  .crossDepSettings(commonCrossDeps: _*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val readme = jvmModule("readme")
  .dependsOn(coreJVM)
  .settings(noPublishSettings)
  .settings(readmeSettings)

lazy val bench = jvmModule("bench")
  .enablePlugins(JmhPlugin)
  .dependsOn(coreJVM)
  .settings(noPublishSettings)
  .settings(libraryDependencies ++= Seq(%%("scheckShapeless")))