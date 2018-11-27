package com.leighperry.conduction.config

import cats.data.ValidatedNec
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.validated._
import com.iag.mobai.shared.TestSupport
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Gen

object ConfigSupportTest
  extends SimpleTestSuite
    with Checkers
    with TestSupport {

  private val nonEmptyString: Gen[String] =
    for {
      scount <- Gen.chooseNum[Int](1, 20)
      chars <- Gen.listOfN(scount, Gen.alphaChar)
    } yield chars.mkString


  test("Primitive String values") {
    check2 {
      (k: String, v: String) =>
        Configured[String]
          .value(Environment.withMap(Map(k -> v)), k)
          .shouldBe(v.validNec)
    }
  }

  test("Primitive Int values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Int]
          .value(Environment.withMap(Map(k -> v.toString)), k)
          .shouldBe(v.validNec)
    }
  }

  test("Optional String values") {
    check2 {
      (k: String, v: String) =>
        Configured[Option[String]]
          .value(Environment.withMap(Map(k -> v)), k)
          .shouldBe(v.some.validNec)
    }
  }

  test("Optional Int values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Option[Int]]
          .value(Environment.withMap(Map(k -> v.toString)), k)
          .shouldBe(v.some.validNec)
    }
  }
}


object Testing {

  import configuredinstances._

  final case class Endpoint(host: String, port: Int)

  object Endpoint {
    implicit val `Configured for Endpoint`: Configured[Endpoint] =
      new Configured[Endpoint] {
        override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Endpoint] = (
          Configured[String].valueSuffixed(env, name, "HOST"),
          Configured[Int].valueSuffixed(env, name, "PORT")
        ).mapN(Endpoint.apply)
      }
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit val `Configured for TwoEndpoints`: Configured[TwoEndpoints] =
      new Configured[TwoEndpoints] {
        override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, TwoEndpoints] = (
          Configured[Endpoint].valueSuffixed(env, name, "EP1"),
          Configured[Endpoint].valueSuffixed(env, name, "EP2"),
        ).mapN(TwoEndpoints.apply)
      }
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit val `Configured for ThreeEndpoints`: Configured[ThreeEndpoints] =
      new Configured[ThreeEndpoints] {
        override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, ThreeEndpoints] = (
          Configured[Endpoint].valueSuffixed(env, name, "EP1"),
          Configured[Endpoint].valueSuffixed(env, name, "EP2"),
          Configured[Endpoint].valueSuffixed(env, name, "EP3"),
        ).mapN(ThreeEndpoints.apply)
      }
  }


  def main(args: Array[String]): Unit = {
    val env: Environment =
      Environment.withDebugMap(
        Environment.withMap(
          Map(
            "A_DOUBLE" -> "1.23",
            "A_DOUBLE_OPT" -> "1.23",
            "LP1_HOST" -> "lp1-host",
            "LP1_PORT" -> "1",
            "MULTI_EP1_HOST" -> "multi-ep1-host",
            "MULTI_EP1_PORT" -> "2",
            "MULTI_EP2_HOST" -> "multi-ep2-host",
            "MULTI_EP2_PORT" -> "3",
            "MULTI_EP3_HOST" -> "multi-ep3-host",
            "MULTI_EP3_PORT" -> "4",

            "CHOICE_C2_HOST" -> "choice-c2-host",
            "CHOICE_C2_PORT" -> "6",

            "CHOICE_C1_HOST" -> "choice-c1-host",
            "CHOICE_C1_PORT" -> "5",
            "CHOICE_C2_C1_HOST" -> "choice-c2-c1-host",
            "CHOICE_C2_C1_PORT" -> "7",
            "CHOICE_C2_C2_HOST" -> "choice-c2-c2-host",
            "CHOICE_C2_C2_PORT" -> "8",

            "CHOICE_OPT_C2_C1_HOST" -> "choice-opt-c2-c1-host",
            "CHOICE_OPT_C2_C1_PORT" -> "9",

            "INTLIST_COUNT" -> "3",
            "INTLIST_0" -> "1000",
            "INTLIST_1" -> "1001",
            "INTLIST_2" -> "1002",

            "EPLIST_COUNT" -> "2",
            "EPLIST_0_HOST" -> "eplist0-host",
            "EPLIST_0_PORT" -> "2",
            "EPLIST_1_HOST" -> "eplist1-host",
            "EPLIST_1_PORT" -> "3",

            "TEPLIST_COUNT" -> "2",
            "TEPLIST_0_EP1_HOST" -> "teplist0-ep1-host",
            "TEPLIST_0_EP1_PORT" -> "7",
            "TEPLIST_0_EP2_HOST" -> "multilist-ep1-host0",
            "TEPLIST_0_EP2_PORT" -> "7",
            "TEPLIST_1_EP1_HOST" -> "teplist1-ep1-host",
            "TEPLIST_1_EP1_PORT" -> "7",
            "TEPLIST_1_EP2_HOST" -> "multilist-ep2-host1",
            "TEPLIST_1_EP2_PORT" -> "7",
          )
        )
      )

    println(Configured[Double].value(env, "A_DOUBLE"))
    println(Configured[Option[Double]].value(env, "A_DOUBLE"))
    println(Configured[Option[Double]].value(env, "Q_DOUBLE"))

    println(Configured[Endpoint].value(env, "LP1"))
    println(Configured[TwoEndpoints].value(env, "MULTI"))
    println(Configured[ThreeEndpoints].value(env, "MULTI"))
    println(Configured[Either[Endpoint, Endpoint]].value(env, "CHOICE"))
    println(Configured[Either[Endpoint, Either[Endpoint, Endpoint]]].value(env, "CHOICE"))
    println(Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]].value(env, "CHOICE"))

    println(Configured[List[Int]].value(env, "INTLIST"))
    println(Configured[List[Endpoint]].value(env, "EPLIST"))
    println(Configured[List[TwoEndpoints]].value(env, "TEPLIST"))
  }

}
