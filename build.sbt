val circeVersion = "0.14.5"
lazy val root = project
  .in(file("."))
  .settings(
    name := "graalinterpreter",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.1",
    libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "23.0.1",
    libraryDependencies += "org.graalvm.truffle" % "truffle-dsl-processor" % "23.0.1",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    graalVMNativeImageOptions ++= Seq(
      "-H:+AllowDeprecatedBuilderClassesOnImageClasspath"
    ),
    fork := true
  )
  .enablePlugins(GraalVMNativeImagePlugin)
lazy val app = (project in file("app"))
  .settings(
    assembly / mainClass := Some("graalinterpreter.Main")
  )
assemblyMergeStrategy in assembly := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
