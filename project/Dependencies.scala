import sbt._

object Dependencies {
  object Version {
    val scala = "2.13.0"

    val cats = "2.0.0"
    val catsEffect = "2.0.0"

    val log4cats = "1.0.0"

    val minitest = "2.7.0"
    val scalacheck = "1.14.1"
  }

  val cats = "org.typelevel" %% "cats-core" % Version.cats
  val catsLaws = "org.typelevel" %% "cats-laws" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect

  val log4catsSlf4j = "io.chrisdavenport" %% "log4cats-slf4j" % Version.log4cats

  val minitest = "io.monix" %% "minitest" % Version.minitest
  val minitestLaws = "io.monix" %% "minitest-laws" % Version.minitest
  val scalacheck = "org.scalacheck" %% "scalacheck" % Version.scalacheck
}
