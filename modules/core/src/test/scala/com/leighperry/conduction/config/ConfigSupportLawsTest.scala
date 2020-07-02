package com.leighperry.conduction.config

import cats.instances.int._
import cats.instances.long._
import cats.instances.string._
import cats.instances.tuple._
import cats.laws.discipline.{ ApplicativeTests, FunctorTests }
import cats.syntax.either._
import cats.syntax.functor._
import cats.{ Applicative, Eq, Functor, Id }
import com.leighperry.conduction.config.testsupport.TestSupport
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.{ Arbitrary, Gen }
import org.typelevel.discipline.Laws

object ConfigSupportLawsTest extends SimpleTestSuite with Checkers with TestSupport {

  import Arbitraries._

  final case class TestContext()

  private def checkAll(name: String)(ruleSet: TestContext => Laws#RuleSet) = {
    implicit val ctx = TestContext()

    for ((id, prop) <- ruleSet(ctx).all.properties)
      test(name + "." + id) {
        check(prop)
      }
  }

  ////

  checkAll("`Conversion` Functor laws") {
    _ => FunctorTests[Conversion].functor[Int, Long, String]
  }

  // This is needed for 2.12
  implicit val instanceApplicative: Applicative[Configured[Id, *]] = Configured.applicativeConfigured[Id]

  checkAll("`Configured` Functor laws") {
    _ => FunctorTests[Configured[Id, *]].functor[Int, Long, String]
  }

  checkAll("`Configured` Applicative laws") {
    _ => ApplicativeTests[Configured[Id, *]].applicative[Int, Long, String]
  }

}

////

object Arbitraries extends TestSupport {

  implicit def cinstArbitraryConversion[A: Conversion: Arbitrary]: Arbitrary[Conversion[A]] =
    Arbitrary(Gen.const(Conversion[A]))

  implicit def cinstEqConversion[T: Arbitrary: Eq]: Eq[Conversion[T]] =
    new Eq[Conversion[T]] {
      override def eqv(x: Conversion[T], y: Conversion[T]): Boolean =
        Arbitrary.arbitrary[String].sample.fold(false) {
          s => x.of(s) === y.of(s)
        }
    }

  ////

  implicit def cinstArbitraryConfigured[A](implicit F: Configured[Id, A]): Arbitrary[Configured[Id, A]] =
    Arbitrary(Gen.const(F))

  private val testEnv = Environment.fromMap[Id](Map())

  implicit def cinstEqConfigured[T: Arbitrary: Eq]: Eq[Configured[Id, T]] =
    new Eq[Configured[Id, T]] {
      override def eqv(x: Configured[Id, T], y: Configured[Id, T]): Boolean =
        Arbitrary.arbitrary[String].sample.fold(false) {
          s => x.value(s).run(testEnv).shouldBeValidatedNec(y.value(s).run(testEnv))
        }
    }

  implicit def cinstArbFAtoB[A: Arbitrary, B](
    implicit arbA2B: Arbitrary[A => B],
    F: Configured[Id, A]
  ): Arbitrary[Configured[Id, A => B]] =
    Arbitrary {
      arbA2B.arbitrary.flatMap {
        (a2b: A => B) =>
          Gen
            .const(F)
            .map(_.map(_ => a2b))
      }
    }

}
