import sbt._

object Dependencies {
  object Version {
    val kindProjector = "0.11.3"

    val cats = "2.4.2"
    val catsEffect = "2.4.2"

    // NB: later versions give error: macro implementations cannot have implicit parameters other than WeakTypeTag evidences
    val magnolia = "0.17.0"
    val shapeless = "2.3.3"

    val minitest = "2.9.2"
    val scalacheck = "1.15.3"
  }

  val cats = "org.typelevel" %% "cats-core" % Version.cats
  val catsLaws = "org.typelevel" %% "cats-laws" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect

  val magnolia = "com.propensive" %% "magnolia" % Version.magnolia
  val shapeless = "com.chuusai" %% "shapeless" % Version.shapeless

  val minitest = "io.monix" %% "minitest" % Version.minitest
  val minitestLaws = "io.monix" %% "minitest-laws" % Version.minitest
  val scalacheck = "org.scalacheck" %% "scalacheck" % Version.scalacheck
}
