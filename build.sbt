lazy val scala212 = "2.12.16"
lazy val scala213 = "2.13.10"

inThisBuild(
  List(
    organization := "io.github.ablearthy",
    homepage := Some(url("https://github.com/ablearthy/td-tl-parser")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "ablearthy",
        "Able Arthy",
        "ablearthy@gmail.com",
        url("https://github.com/ablearthy")
      )
    )
  )
)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val supportedScalaVersions = Seq(scala212, scala213)

lazy val root = (project in file("."))
  .settings(
    name := "td-tl-parser",
    crossScalaVersions := supportedScalaVersions,
    scalaVersion := scala213,
    version := "1.0.0",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"
  )
