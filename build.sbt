val circeVersion = "0.14.5"
lazy val root = project
  .in(file("."))
  .settings(
    name := "taglessinterpreter",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.0",
    // https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-api
    libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "23.0.1",
// https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-dsl-processor
    libraryDependencies += "org.graalvm.truffle" % "truffle-dsl-processor" % "23.0.1",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
