import Dependencies._
import sbt.Project

inThisBuild(List(
  organization := "com.github.leigh-perry",
  homepage := Some(url("https://github.com/leigh-perry/conduction")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "lperry",
      "Leigh Perry",
      "lperry.breakpoint@gmail.com",
      url("https://github.com/leigh-perry")
    )
  )
))

val projectName = "conduction"

lazy val commonSettings =
  ProjectDefaults.settings ++
    Seq(
      name := projectName,
      //organization := "com.leighperry",
      scalaVersion := "2.12.9"
    )

val tests = "compile->compile;test->test"

lazy val config =
  module(
    id = "config",
    deps =
      Seq(
        cats,
        catsEffect,
        log4catsSlf4j % "test",
        minitest % "test",
        minitestLaws % "test",
        scalacheck % "test",
        catsLaws % "test",
        //"com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % Test
      )
    )

lazy val root =
  (project in file("."))
    .aggregate(config)
    .dependsOn(config)
    .settings(commonSettings)
    .settings(
      aggregate in update := false,
      updateOptions := updateOptions.value.withCachedResolution(true),
      mainClass in Compile := (mainClass in `config` in Compile).value,
      fullClasspath in Runtime ++= (fullClasspath in `config` in Runtime).value
    )

def module(id: String, settings: Seq[Def.Setting[_]] = commonSettings, deps: Seq[ModuleID] = Vector()): Project = {
  Project(id = id, base = file(id))
    .settings(settings)
    .settings(
      name := s"$projectName-$id",
      libraryDependencies ++= deps ++ Seq("org.scala-lang" % "scala-reflect" % "2.12.9")
    )
}
