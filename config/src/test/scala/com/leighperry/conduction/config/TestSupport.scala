package com.iag.mobai.shared

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
}

trait ToTestSupportOps {
  implicit def `Ops for TestSupport`[A](actual: A): TestSupportOps[A] =
    new TestSupportOps[A](actual)
}

trait TestSupport
  extends ToTestSupportOps

object testsupportinstances
  extends TestSupport
