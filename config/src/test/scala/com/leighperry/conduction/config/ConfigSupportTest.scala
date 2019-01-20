package com.leighperry.conduction.config

import cats.data.Validated
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
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
          .value(k)
          .run(fromMap(Map(k -> v)))
          .shouldBe(v.validNec)
    }
  }

  test("Primitive Int values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Int]
          .value(k)
          .run(fromMap(Map(k -> v.toString)))
          .shouldBe(v.validNec)
    }
  }

  test("Present Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val ok = s"${k}_OPT"
        Configured[Option[String]]
          .value(k)
          .run(fromMap(Map(ok -> v)))
          .shouldBe(v.some.validNec)
    }
  }

  test("Present Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(k)
          .run(fromMap(Map(ok -> v.toString)))
          .shouldBe(v.some.validNec)
    }
  }

  test("Missing Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val ok = s"${k}_OPT"
        Configured[Option[String]]
          .value(s"${k}a")
          .run(fromMap(Map(ok -> v)))
          .shouldBe(None.validNec)
    }
  }

  test("Missing Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(s"${k}a")
          .run(fromMap(Map(ok -> v.toString)))
          .shouldBe(None.validNec)
    }
  }

  test("Misconfigured Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val ok = s"${k}_OPT"
        Configured[Option[Int]]
          .value(k)
          .run(fromMap(Map(ok -> s"${v.toString}x")))
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
    implicit val `Configured for Endpoint`: Configured[Endpoint] = (
      Configured[String].suffixed("HOST"),
      Configured[Int].suffixed("PORT")
    ).mapN(Endpoint.apply)
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit val `Configured for TwoEndpoints`: Configured[TwoEndpoints] = (
      Configured[Endpoint].suffixed("EP1"),
      Configured[Endpoint].suffixed("EP2")
    ).mapN(TwoEndpoints.apply)
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit val `Configured for ThreeEndpoints`: Configured[ThreeEndpoints] = (
      Configured[Endpoint].suffixed("EP1"),
      Configured[Endpoint].suffixed("EP2"),
      Configured[Endpoint].suffixed("EP3"),
    ).mapN(ThreeEndpoints.apply)
  }

  test("Present valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Double]
      .value("A_DOUBLE")
      .run(fromMap(Map(k -> v)))
      .assertIs(1.23.validNec)
  }

  test("Missing valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Double]
      .value("MISSING")
      .run(fromMap(Map(k -> v)))
      .assertIs(ConfiguredError.MissingValue("MISSING").invalidNec)
  }

  test("Invalid valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    Configured[Double].value(k)
      .run(fromMap(Map(k -> v)))
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

        "SOME_INT" -> "567",
      )
      //)
    )

  test("Present valid Configured[Endpoint]") {
    Configured[Endpoint]
      .value("LP1")
      .run(env)
      .assertIs(Endpoint("lp1-host", 1).validNec)
  }

  test("Present valid Configured[TwoEndpoints]") {
    Configured[TwoEndpoints]
      .value("MULTI")
      .run(env)
      .assertIs(TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec)
  }

  test("Present valid Configured[ThreeEndpoints]") {
    Configured[ThreeEndpoints]
      .value("MULTI")
      .run(env)
      .assertIs(
        ThreeEndpoints(
          Endpoint("multi-ep1-host", 2),
          Endpoint("multi-ep2-host", 3),
          Endpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  test("Present valid Configured[Either[Endpoint, Endpoint]]") {
    Configured[Either[Endpoint, Endpoint]]
      .value("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Missing Configured[Either[Endpoint, Endpoint]]") {
    Configured[Either[Endpoint, Endpoint]]
      .value("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Present valid Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]
      .value("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.validNec)
  }

  test("Missing Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Either[Endpoint, Either[Endpoint, Endpoint]]]
      .value("CHOICEx")
      .run(env)
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
    Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]
      .value("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  test("Missing Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    Configured[Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]
      .value("CHOICEx")
      .run(env)
      .assertIs(None.validNec)
  }

  test("Present valid Configured[List[Int]]") {
    Configured[List[Int]]
      .value("INTLIST")
      .run(env)
      .assertIs(List(1000, 1001, 1002).validNec)
  }

  test("Missing Configured[List[Int]]") {
    Configured[List[Int]]
      .value("INTLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("INTLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[List[Endpoint]]") {
    Configured[List[Endpoint]]
      .value("EPLIST")
      .run(env)
      .assertIs(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  test("Missing Configured[List[Endpoint]]") {
    Configured[List[Endpoint]]
      .value("EPLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("EPLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[List[TwoEndpoints]]") {
    Configured[List[TwoEndpoints]]
      .value("TEPLIST")
      .run(env)
      .assertIs(
        List(
          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  test("Missing Configured[List[TwoEndpoints]]") {
    Configured[List[TwoEndpoints]]
      .value("TEPLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("TEPLISTx_COUNT").invalidNec)
  }

  test("Configured should handle newtypes") {
    Configured[Int]
      .map(i => s"int[$i]")
      .value("SOME_INT")
      .run(env)
      .assertIs("int[567]".validNec)
  }

}
