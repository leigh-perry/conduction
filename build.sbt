import Dependencies._

val Scala_213 = "2.13.1"
val Scala_212 = "2.12.10"
//val Scala_211 = "2.11.12"

////

val projectName = "Conduction"

inThisBuild(
  List(
    organization := "com.github.leigh-perry",
    homepage := Some(url("https://github.com/leigh-perry/${projectName.toLowerCase}")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers :=
      List(
        Developer(
          "leigh-perry",
          "Leigh Perry",
          "lperry.breakpoint@gmail.com",
          url("https://leigh-perry.github.io")
        )
      )
  )
)

lazy val compilerPlugins =
  List(
    compilerPlugin("org.typelevel" %% "kind-projector" % Version.kindProjectorVersion)
  )

lazy val commonSettings =
  Seq(
    scalaVersion := Scala_213,
    scalacOptions ++= commonScalacOptions(scalaVersion.value),
    fork in Test := true,
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    name := projectName,
    updateOptions := updateOptions.value.withGigahorse(false),
    libraryDependencies ++=
      Seq(
        log4catsSlf4j % "test",
        minitest % "test",
        minitestLaws % "test",
        scalacheck % "test",
        catsLaws % "test"
      ) ++ compilerPlugins
  )

lazy val crossBuiltCommonSettings = commonSettings ++ Seq(crossScalaVersions := Seq(Scala_212, Scala_213))

lazy val core =
  module("core")
    .settings(
      libraryDependencies ++=
        Seq(
          cats,
          catsEffect
        )
    )

lazy val allModules = List(core)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true, crossScalaVersions := List())
    .aggregate((allModules).map(x => x: ProjectReference): _*)

////

def module(moduleName: String): Project =
  Project(moduleName, file("modules/" + moduleName))
    .settings(crossBuiltCommonSettings)
    .settings(name += s"-$moduleName")

def versionDependentExtraScalacOptions(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => Seq("-Yno-adapted-args", "-Xfuture", "-Ypartial-unification")
    case _ => Nil
  }

def commonScalacOptions(scalaVersion: String) =
  Seq(
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    //"-Xfatal-warnings",
    "-deprecation",
    "-Xlint:-unused,_"
  ) ++
    versionDependentExtraScalacOptions(scalaVersion)
