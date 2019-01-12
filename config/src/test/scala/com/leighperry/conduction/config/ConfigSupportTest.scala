package com.leighperry.conduction.config

import cats.data.{Validated, ValidatedNec}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.testsupport.TestSupport
import minitest.SimpleTestSuite
import minitest.laws.Checkers

object ConfigSupportTest
  extends SimpleTestSuite
    with Checkers
    with TestSupport {

  import Environment._
  import configuredinstances._

  test("Primitive String values") {
    check2 {
      (k: String, v: String) =>
        Configured[String]
          .value(fromMap(Map(k -> v)), k)
          .shouldBe(v.validNec)
    }
  }

  test("Primitive Int values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Int]
          .value(fromMap(Map(k -> v.toString)), k)
          .shouldBe(v.validNec)
    }
  }

  test("Present Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val ok = s"${k}_OPT"
        Configured[Option[String]]
          .value(fromMap(Map(ok -> v)), k)
          .shouldBe(v.some.validNec)
    }
  }

  test("Present Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(fromMap(Map(ok -> v.toString)), k)
          .shouldBe(v.some.validNec)
    }
  }

  test("Missing Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val ok = s"${k}_OPT"
        Configured[Option[String]]
          .value(fromMap(Map(ok -> v)), s"${k}a")
          .shouldBe(None.validNec)
    }
  }

  test("Missing Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(fromMap(Map(ok -> v.toString)), s"${k}a")
          .shouldBe(None.validNec)
    }
  }

  test("Misconfigured Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(fromMap(Map(ok -> s"${v.toString}x")), k)
          .shouldSatisfy {
            case Validated.Invalid(nec) =>
              nec.length.shouldBe(1) &&
                nec.forall(_.isInstanceOf[ConfiguredError.InvalidValue])
            case _ => false
          }
    }
  }

  ////

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

  test("Present valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Double].value(fromMap(Map(k -> v)), "A_DOUBLE")
      .assertIs(1.23.validNec)
  }

  test("Missing valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Double].value(fromMap(Map(k -> v)), "MISSING")
      .assertIs(ConfiguredError.MissingValue("MISSING").invalidNec)
  }

  test("Invalid valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    Configured[Double].value(fromMap(Map(k -> v)), k)
      .assertIs(ConfiguredError.InvalidValue(k, v).invalidNec)
  }


  val env: Environment =
  //Environment.withDebug(
    fromMap(
      Map(
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
      //)
    )

  test("Present valid Configured[Endpoint]") {
    Configured[Endpoint].value(env, "LP1")
      .assertIs(Endpoint("lp1-host", 1).validNec)
  }

  test("Present valid Configured[TwoEndpoints]") {
    Configured[TwoEndpoints].value(env, "MULTI")
      .assertIs(TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec)
  }

  test("Present valid Configured[ThreeEndpoints]") {
    Configured[ThreeEndpoints].value(env, "MULTI")
      .assertIs(
        ThreeEndpoints(
          Endpoint("multi-ep1-host", 2),
          Endpoint("multi-ep2-host", 3),
          Endpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  test("Present valid Configured[Either[Endpoint, Endpoint]]") {
    Configured[Either[Endpoint, Endpoint]].value(env, "CHOICE")
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Missing Configured[Either[Endpoint, Endpoint]]") {
    Configured[Either[Endpoint, Endpoint]].value(env, "CHOICE")
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Present valid Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Either[Endpoint, Either[Endpoint, Endpoint]]].value(env, "CHOICE")
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.validNec)
  }

  test("Missing Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Either[Endpoint, Either[Endpoint, Endpoint]]].value(env, "CHOICEx")
      // NonEmptyChain doesn't support ==
      //      .assertIsEq(
      //        NonEmptyChain(
      //          ConfiguredError.MissingValue("CHOICEx_C1_HOST"),
      //          ConfiguredError.MissingValue("CHOICEx_C1_PORT"),
      //          ConfiguredError.MissingValue("CHOICEx_C2_C1_HOST"),
      //          ConfiguredError.MissingValue("CHOICEx_C2_C1_PORT"),
      //          ConfiguredError.MissingValue("CHOICEx_C2_C2_HOST"),
      //          ConfiguredError.MissingValue("CHOICEx_C2_C2_PORT")
      //        ).invalid
      //      )
      .assertSatisfies(
      _.fold(
        e => {
          e.length == 6 &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C1_HOST")) &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C1_PORT")) &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C2_C1_HOST")) &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C2_C1_PORT")) &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C2_C2_HOST")) &&
            e.exists(_ == ConfiguredError.MissingValue("CHOICEx_C2_C2_PORT"))
        },
        _ => false
      )
    )
  }

  test("Present valid Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]].value(env, "CHOICE")
      .assertIs(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  test("Missing Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]].value(env, "CHOICEx")
      .assertIs(None.validNec)
  }

  test("Present valid Configured[List[Int]]") {
    Configured[List[Int]].value(env, "INTLIST")
      .assertIs(List(1000, 1001, 1002).validNec)
  }

  test("Missing Configured[List[Int]]") {
    Configured[List[Int]].value(env, "INTLISTx")
      .assertIs(ConfiguredError.MissingValue("INTLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[List[Endpoint]]") {
    Configured[List[Endpoint]].value(env, "EPLIST")
      .assertIs(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  test("Missing Configured[List[Endpoint]]") {
    Configured[List[Endpoint]].value(env, "EPLISTx")
      .assertIs(ConfiguredError.MissingValue("EPLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[List[TwoEndpoints]]") {
    Configured[List[TwoEndpoints]].value(env, "TEPLIST")
      .assertIs(
        List(
          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  test("Missing Configured[List[TwoEndpoints]]") {
    Configured[List[TwoEndpoints]].value(env, "TEPLISTx")
      .assertIs(ConfiguredError.MissingValue("TEPLISTx_COUNT").invalidNec)
  }

}
