ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "1.0.0"
ThisBuild / organization := "io.github.ablearthy"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "td-tl-parser",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"
  )
