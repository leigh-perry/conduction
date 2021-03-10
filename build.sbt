import Dependencies._

val Scala_212 = "2.12.13"
val Scala_212 = "2.12.10"
val Scala_211 = "2.11.12"
val Scala_111213 = Seq(Scala_211, Scala_212, Scala_213)
val Scala_1213 = Seq(Scala_212, Scala_213)

////

val projectName = "conduction"

inThisBuild(
  List(
    organization := "com.github.leigh-perry",
    homepage := Some(url(s"https://github.com/leigh-perry/${projectName.toLowerCase}")),
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
    compilerPlugin("org.typelevel" %% "kind-projector" % Version.kindProjector cross CrossVersion.full)
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
        minitest % "test",
        minitestLaws % "test",
        scalacheck % "test",
        catsLaws % "test"
      ) ++ compilerPlugins
  )

lazy val crossBuiltCommonSettings = commonSettings

lazy val core =
  module("core")
    .settings(crossScalaVersions := Scala_1213) // was Scala_111213
    .settings(
      libraryDependencies ++=
        Seq(
          cats,
          catsEffect
        )
    )

lazy val `conduction-magnolia` =
  module("magnolia")
    .settings(crossScalaVersions := Scala_1213)
    .settings(
      libraryDependencies ++=
        Seq(
          cats,
          catsEffect,
          magnolia
        )
    )
    .dependsOn(core % "compile->compile;test->test")

lazy val `conduction-shapeless` =
  module("shapeless")
    .settings(crossScalaVersions := Seq(Scala_213))  // TODO 2.12
    .settings(
      libraryDependencies ++=
        Seq(
          cats,
          catsEffect,
          shapeless
        )
    )
    .dependsOn(core % "compile->compile;test->test")

lazy val allModules = List(core, `conduction-magnolia`, `conduction-shapeless`)

lazy val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true, crossScalaVersions := List())
    .aggregate((allModules).map(x => x: ProjectReference): _*)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fmtcheck", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

////

def module(moduleName: String): Project =
  Project(moduleName, file("modules/" + moduleName))
    .settings(crossBuiltCommonSettings)
    .settings(name += s"-$moduleName") // for artifact naming

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
    "-deprecation"
    //"-Xlint:-unused,_"
  ) ++
    versionDependentExtraScalacOptions(scalaVersion)
