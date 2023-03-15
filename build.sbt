lazy val scala212 = "2.12.16"
lazy val scala213 = "2.13.10"

ThisBuild / scalaVersion := scala213
ThisBuild / version := "1.0.0"
ThisBuild / organization := "io.github.ablearthy"

lazy val supportedScalaVersions = Seq(scala212, scala213)

lazy val root = (project in file("."))
  .settings(
    name := "td-tl-parser",
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"
  )
