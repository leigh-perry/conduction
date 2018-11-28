package com.leighperry.conduction.config.testsupport

import cats.Eq
import cats.syntax.eq._
import minitest.api.Asserts

import scala.language.implicitConversions

final class TestSupportOps[A](val actual: A) extends Asserts {
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
      println(s"       => FAIL:   doesn't satisfy: [$actual]")
    }
    result
  }

  def assertIs(expected: A): Unit = {
    shouldBe(expected)
    assertEquals(actual, expected)
  }

  def assertSatisfies(f: A => Boolean): Unit = {
    assert(shouldSatisfy(f))
  }
}

trait ToTestSupportOps {
  implicit def `Ops for TestSupport`[A](actual: A): TestSupportOps[A] =
    new TestSupportOps[A](actual)
}

////

final class TestSupportEqOps[A: Eq](val actual: A) extends Asserts {
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
  implicit def `Ops for TestSupport Eq`[A: Eq](actual: A): TestSupportEqOps[A] =
    new TestSupportEqOps[A](actual)
}

trait TestSupport
  extends ToTestSupportOps
    with ToTestSupportEqOps

object testsupportinstances
  extends TestSupport
