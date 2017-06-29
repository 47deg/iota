resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("releases"))
addSbtPlugin("io.frees" % "sbt-freestyle" % "0.1.0")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC6")