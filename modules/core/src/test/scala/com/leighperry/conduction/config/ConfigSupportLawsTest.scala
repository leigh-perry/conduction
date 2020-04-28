package com.leighperry.conduction.config

import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.FunctorTests
import cats.syntax.either._
import cats.{Eq, Id}
import com.leighperry.conduction.config.testsupport.TestSupport
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws

object ConfigSupportLawsTest extends SimpleTestSuite with Checkers with TestSupport {

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
          s => x.of(s) === y.of(s)
        }
    }

  checkAll("`Conversion` Functor laws") {
    _ => FunctorTests[Conversion].functor[Int, Long, String]
  }

  ////

  implicit def arbitraryConfigured[A: Configured[Id, *]: Arbitrary]: Arbitrary[Configured[Id, A]] =
    Arbitrary(Gen.const(Configured[Id, A]))

  private val testEnv = Environment.fromMap[Id](Map())

  implicit def eqConfigured[T: Arbitrary: Eq]: Eq[Configured[Id, T]] =
    new Eq[Configured[Id, T]] {
      override def eqv(x: Configured[Id, T], y: Configured[Id, T]): Boolean =
        Arbitrary.arbitrary[String].sample.fold(false) {
          s => x.value(s).run(testEnv).shouldBeValidatedNec(y.value(s).run(testEnv))
        }
    }

// TODO doesn't work for 2.12
//  checkAll("`Configured` Functor laws") {
//    _ => FunctorTests[Configured[Id, *]].functor[Int, Long, String]
//  }

  // TODO applicative laws for Configured

}
