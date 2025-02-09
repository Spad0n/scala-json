ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "scala-json",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )