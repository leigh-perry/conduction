package com.leighperry.conduction.config

import cats.{Id, Monad}
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

  test("Primitive String values") {
    check2 {
      (k: String, v: String) =>
        Configured[Id, String](k)
          .run(fromMap(Map(k -> v)))
          .shouldBe(v.validNec)
    }
  }

  test("Primitive Int values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Id, Int](k)
          .run(fromMap(Map(k -> v.toString)))
          .shouldBe(v.validNec)
    }
  }

  test("Present Option[String] values") {
    check2 {
      (k: String, v: String) =>
        Configured[Id, Option[String]](k)
          .run(fromMap(Map(s"${k}_OPT" -> v)))
          .shouldBe(v.some.validNec)
    }
  }

  test("Present Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Id, Option[Int]](k)
          .run(fromMap(Map(s"${k}_OPT" -> v.toString)))
          .shouldBe(v.some.validNec)
    }
  }

  test("Missing Option[String] values") {
    check2 {
      (k: String, v: String) =>
        Configured[Id, Option[String]](s"${k}a")
          .run(fromMap(Map(s"${k}_OPT" -> v)))
          .shouldBe(None.validNec)
    }
  }

  test("Missing Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Id, Option[Int]](s"${k}a")
          .run(fromMap(Map(s"${k}_OPT" -> v.toString)))
          .shouldBe(None.validNec)
    }
  }

  test("Misconfigured Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        Configured[Id, Option[Int]](k)
          .run(fromMap(Map(s"${k}_OPT" -> s"${v.toString}x")))
          .shouldSatisfy {
            case Validated.Invalid(nec) =>
              nec.length.shouldBe(1) &&
                nec.forall(_.isInstanceOf[ConfiguredError.InvalidValue])
            case _ => false
          }
    }
  }

  test("Present valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Id, Double]("A_DOUBLE")
      .run(fromMap(Map(k -> v)))
      .assertIs(1.23.validNec)
  }

  test("Missing valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    Configured[Id, Double]("MISSING")
      .run(fromMap(Map(k -> v)))
      .assertIs(ConfiguredError.MissingValue("MISSING").invalidNec)
  }

  test("Invalid valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    Configured[Id, Double](k)
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

  test("Present valid Configured[Id, Endpoint]") {
    Configured[Id, Endpoint]("LP1")
      .run(env)
      .assertIs(Endpoint("lp1-host", 1).validNec)
  }

  test("Present valid Configured[Id, TwoEndpoints]") {
    Configured[Id, TwoEndpoints]("MULTI")
      .run(env)
      .assertIs(TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec)
  }

  test("Present valid Configured[Id, ThreeEndpoints]") {
    Configured[Id, ThreeEndpoints]("MULTI")
      .run(env)
      .assertIs(
        ThreeEndpoints(
          Endpoint("multi-ep1-host", 2),
          Endpoint("multi-ep2-host", 3),
          Endpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  test("Present valid Configured[Id, Either[Endpoint, Endpoint]]") {
    Configured[Id, Either[Endpoint, Endpoint]]("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Missing Configured[Id, Either[Endpoint, Endpoint]]") {
    Configured[Id, Either[Endpoint, Endpoint]]("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Present valid Configured[Id, Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Id, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.validNec)
  }

  test("Missing Configured[Id, Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    Configured[Id, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICEx")
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

  test("Present valid Configured[Id, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    Configured[Id, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
      .run(env)
      .assertIs(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  test("Missing Configured[Id, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    Configured[Id, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICEx")
      .run(env)
      .assertIs(None.validNec)
  }

  test("Present valid Configured[Id, List[Int]]") {
    Configured[Id, List[Int]]("INTLIST")
      .run(env)
      .assertIs(List(1000, 1001, 1002).validNec)
  }

  test("Missing Configured[Id, List[Int]]") {
    Configured[Id, List[Int]]("INTLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("INTLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[Id, List[Endpoint]]") {
    Configured[Id, List[Endpoint]]("EPLIST")
      .run(env)
      .assertIs(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  test("Missing Configured[Id, List[Endpoint]]") {
    Configured[Id, List[Endpoint]]("EPLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("EPLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[Id, List[TwoEndpoints]]") {
    Configured[Id, List[TwoEndpoints]]("TEPLIST")
      .run(env)
      .assertIs(
        List(
          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  test("Missing Configured[Id, List[TwoEndpoints]]") {
    Configured[Id, List[TwoEndpoints]]("TEPLISTx")
      .run(env)
      .assertIs(ConfiguredError.MissingValue("TEPLISTx_COUNT").invalidNec)
  }

  test("Configured should handle newtypes") {
    Configured[Id, Int]
      .map(i => s"int[$i]")
      .value("SOME_INT")
      .run(env)
      .assertIs("int[567]".validNec)
  }

  ////

  final case class Endpoint(host: String, port: Int)

  object Endpoint {
    implicit def `Configured for Endpoint`[F[_]](implicit F: Monad[F]): Configured[F, Endpoint] = (
      Configured[F, String].withSuffix("HOST"),
      Configured[F, Int].withSuffix("PORT")
    ).mapN(Endpoint.apply)
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit def `Configured for TwoEndpoints`[F[_]](implicit F: Monad[F]): Configured[F, TwoEndpoints] = (
      Configured[F, Endpoint].withSuffix("EP1"),
      Configured[F, Endpoint].withSuffix("EP2")
    ).mapN(TwoEndpoints.apply)
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit def `Configured for ThreeEndpoints`[F[_]](implicit F: Monad[F]): Configured[F, ThreeEndpoints] = (
      Configured[F, Endpoint].withSuffix("EP1"),
      Configured[F, Endpoint].withSuffix("EP2"),
      Configured[F, Endpoint].withSuffix("EP3"),
    ).mapN(ThreeEndpoints.apply)
  }

}
