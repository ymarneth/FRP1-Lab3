ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

Test / testOptions += Tests.Argument("-oD", "-u", "target/test-reports")

lazy val root = (project in file("."))
  .settings(
    name := "Lab3Prjct"
  )
