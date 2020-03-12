package com.leighperry.conduction.config

import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.FunctorTests
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.{ Arbitrary, Gen }
import org.typelevel.discipline.Laws

object ConfigSupportLawsTest extends SimpleTestSuite with Checkers {

  final case class TestContext()

  private def checkAll(name: String)(ruleSet: TestContext => Laws#RuleSet) = {
    implicit val ctx = TestContext()

    for ((id, prop) <- ruleSet(ctx).all.properties)
      test(name + "." + id) {
        check(prop)
      }
  }

  ////

  implicit def arbitraryConversion[A: Conversion: Arbitrary]: Arbitrary[Conversion[A]] =
    Arbitrary(Gen.const(Conversion[A]))

  implicit def eqConversion[T: Arbitrary: Eq]: Eq[Conversion[T]] =
    new Eq[Conversion[T]] {
      override def eqv(x: Conversion[T], y: Conversion[T]): Boolean =
        Arbitrary.arbitrary[String].sample.fold(false) {
          s => x.of(s) == y.of(s)
        }
    }

  checkAll("Conversion Functor laws") {
    _ => FunctorTests[Conversion].functor[Int, Long, String]
  }
}
