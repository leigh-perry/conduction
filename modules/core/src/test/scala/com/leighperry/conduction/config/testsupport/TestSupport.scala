package com.leighperry.conduction.config.testsupport

import cats.Eq
import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.eq._
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.{ Arbitrary, Gen, Prop, Shrink }

final class TestSupportOps[A](val actual: A) {
  def shouldBe(expected: A): Boolean = {
    val result = expected == actual
    if (!result) {
      println(s"       => FAIL: expected[$expected]")
      println(s"                  actual[$actual]")
    }
    result
  }

  def shouldSatisfy(f: A => Boolean): Boolean = {
    val result = f(actual)
    if (!result) {
      println(s"       => FAIL:   doesn't satisfy, actual: [$actual]")
    }
    result
  }

}

trait ToTestSupportOps {
  implicit def instanceTestSupport[A](actual: A): TestSupportOps[A] =
    new TestSupportOps[A](actual)
}

////

final class TestSupportNecOps[A](val actual: NonEmptyChain[A]) {

  def shouldBeNec(expected: NonEmptyChain[A]): Boolean =
    expected.length == actual.length &&
      expected.forall(a => actual.exists(_ == a))

}

trait ToTestSupportNecOps {
  implicit def instanceTestSupport[A](actual: NonEmptyChain[A]): TestSupportNecOps[A] =
    new TestSupportNecOps[A](actual)
}

////

final class TestSupportEqOps[A: Eq](val actual: A) {
  def shouldBeEq(expected: A): Boolean = {
    val result = expected === actual
    if (!result) {
      println(s"       => FAIL: expected[$expected]")
      println(s"                  actual[$actual]")
    }
    result
  }

  def assertIsEq(expected: A): Unit = {
    shouldBeEq(expected)
    assert(actual === expected)
  }
}

trait ToTestSupportEqOps {
  implicit def instanceTestSupportEq[A: Eq](actual: A): TestSupportEqOps[A] =
    new TestSupportEqOps[A](actual)
}

////

final class TestSupportIOOps[E, A](val io: IO[A]) {
  def shouldBeIO(expected: A): IO[Boolean] =
    io.flatMap {
      actual =>
        val result = expected == actual
        if (!result) {
          IO.delay(
              println(
                s"""       => FAIL: expected[$expected]
                   |                  actual[$actual]""".stripMargin
              )
            )
            .map(_ => result)
        } else IO(result)
    }

  def shouldSatisfyIO(f: A => Boolean): IO[Boolean] =
    io.flatMap {
      actual =>
        val result = f(actual)
        if (!result) {
          IO.delay(println(s"       => FAIL:   doesn't satisfy, actual: [$actual]"))
            .map(_ => result)
        } else IO(result)
    }
}

trait ToTestSupportIOOps {
  implicit def instanceTestSupportIO[E, A](io: IO[A]): TestSupportIOOps[E, A] =
    new TestSupportIOOps[E, A](io)
}

////

trait TestSupportGens {
  def genBoolean: Gen[Boolean] =
    Gen.posNum[Int].map(_ % 2 == 0)

  def genMultiline(genTestString: Gen[String]): Gen[(List[String], String)] =
    for {
      scount <- Gen.chooseNum[Int](0, 20)
      strings <- Gen.listOfN(scount, genTestString)
    } yield strings -> strings.flatMap(s => List("\n", s, "\n")).mkString("\n")

  def genSymbol(minLength: Int, maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(minLength, maxLength)
      chars <- Gen.listOfN(n, Gen.alphaChar).map(_.mkString)
    } yield chars.mkString

  def genNonEmptyString(maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(1, maxLength)
      chars <- Gen.listOfN(n, genFor[Char])
    } yield chars.mkString

  def genFor[A: Arbitrary]: Gen[A] =
    implicitly[Arbitrary[A]].arbitrary
}

////

trait TestSupportScalacheck {

  // ScalaCheck forAll except for IO
  def forAllIO[T1, P](
    g1: Gen[T1]
  )(f: T1 => IO[P])(implicit p: P => Prop, s1: Shrink[T1], pp1: T1 => Pretty): Prop =
    forAll(g1) {
      t1 =>
        f(t1).unsafeRunSync()
    }

  //  // TODO apply params
  //  val parametersForGeneratedTests: Test.Parameters =
  //    Test.Parameters.default.withMinSuccessfulTests(1)

  def simpleTest(condition: => Boolean): Prop =
    forAll(implicitly[Arbitrary[Int]].arbitrary) {
      _ =>
        condition
    }

  def simpleTestIO[T1, P](condition: => IO[Boolean]): Prop =
    forAll(implicitly[Arbitrary[Int]].arbitrary) {
      _ =>
        condition.unsafeRunSync()
    }

}

////

trait TestSupport
  extends ToTestSupportOps
  with ToTestSupportNecOps
  with ToTestSupportEqOps
  with ToTestSupportIOOps
  with TestSupportGens
  with TestSupportScalacheck

object testsupportinstances extends TestSupport
