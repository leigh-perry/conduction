package com.leighperry.conduction.config

import cats.Applicative
import cats.data.Validated
import cats.effect.IO
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
        val io =
          for {
            map <- fromMap[IO](Map(k -> v))
            c <- Configured[IO, String](k).run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(v.validNec)
    }
  }

  test("Primitive Int values") {
    check2 {
      (k: String, v: Int) =>
        val io =
          for {
            map <- fromMap[IO](Map(k -> v.toString))
            c <- Configured[IO, Int](k).run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(v.validNec)
    }
  }

  test("Present Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val io =
          for {
            map <- fromMap[IO](Map(s"${k}_OPT" -> v))
            c <- Configured[IO, Option[String]](k).run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(v.some.validNec)
    }
  }

  test("Present Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val io =
          for {
            map <- fromMap[IO](Map(s"${k}_OPT" -> v.toString))
            c <- Configured[IO, Option[Int]](k).run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(v.some.validNec)
    }
  }

  test("Missing Option[String] values") {
    check2 {
      (k: String, v: String) =>
        val io =
          for {
            map <- fromMap[IO](Map(s"${k}_OPT" -> v))
            c <- Configured[IO, Option[String]](s"${k}a").run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(None.validNec)
    }
  }

  test("Missing Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val io =
          for {
            map <- fromMap[IO](Map(s"${k}_OPT" -> v.toString))
            c <- Configured[IO, Option[Int]](s"${k}a").run(map)
          } yield c

        io.unsafeRunSync()
          .shouldBe(None.validNec)
    }
  }

  test("Misconfigured Option[Int] values") {
    check2 {
      (k: String, v: Int) =>
        val io =
          for {
            map <- fromMap[IO](Map(s"${k}_OPT" -> s"${v.toString}x"))
            c <- Configured[IO, Option[Int]](k).run(map)
          } yield c

        io.unsafeRunSync()
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
    val io =
      for {
        map <- fromMap[IO](Map(k -> v))
        c <- Configured[IO, Double]("A_DOUBLE").run(map)
      } yield c

    io.unsafeRunSync()
      .assertIs(1.23.validNec)
  }

  test("Missing valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23"
    val io =
      for {
        map <- fromMap[IO](Map(k -> v))
        c <- Configured[IO, Double]("MISSING").run(map)
      } yield c

    io.unsafeRunSync()
      .assertIs(ConfiguredError.MissingValue("MISSING").invalidNec)
  }

  test("Invalid valid Double") {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    val io =
      for {
        map <- fromMap[IO](Map(k -> v))
        c <- Configured[IO, Double](k).run(map)
      } yield c

    io.unsafeRunSync()
      .assertIs(ConfiguredError.InvalidValue(k, v).invalidNec)
  }

  ////

  val params: Map[String, String] =
    Map(
      "LP1_HOST" -> "lp1-host",
      "LP1_PORT" -> "1",
      "MULTI_EP1_HOST" -> "multi-ep1-host",
      "MULTI_EP1_PORT" -> "2",
      "MULTI_EP2_HOST" -> "multi-ep2-host",
      "MULTI_EP2_PORT" -> "3",
      "MULTI_EP3_HOST" -> "multi-ep3-host",
      "MULTI_EP3_PORT" -> "4",

      "CHOICE_C1_HOST" -> "choice-c1-host",
      "CHOICE_C1_PORT" -> "5",

      "CHOICE2_C1_C1_HOST" -> "choice-c1-c1-host",
      "CHOICE2_C1_C1_PORT" -> "7",
      "CHOICE2_C1_C2_HOST" -> "choice-c1-c2-host",
      "CHOICE2_C1_C2_PORT" -> "8",
      "CHOICE2_C2_HOST" -> "choice2-c2-host",
      "CHOICE2_C2_PORT" -> "5",

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

      "SOME_INT" -> "567"
    )

  private def envIO: IO[Environment] =
    Environment.fromMap[IO](params)
      .flatMap(inner => Environment.logging[IO](inner /*, println*/))

  //  private def updatedEnv(mods: (String, String)*): Environment =
  //    Environment.logging[IO](Environment.fromMap(mods.foldLeft(params)((m, t) => m.updated(t._1, t._2))))

  private def reducedEnvIO(keys: String*): IO[Environment] =
    Environment.fromMap[IO](params.filterNot(t => keys.contains(t._1)))
      .flatMap(inner => Environment.logging[IO](inner /*, println*/))

  ////

  test("Present valid Configured[IO, Endpoint]") {
    envIO.flatMap(env => Configured[IO, Endpoint]("LP1").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("lp1-host", 1).validNec)
  }

  test("Present valid Configured[IO, TwoEndpoints]") {
    envIO.flatMap(env => Configured[IO, TwoEndpoints]("MULTI").run(env))
      .unsafeRunSync()
      .assertIs(TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec)
  }

  test("Present valid Configured[IO, ThreeEndpoints]") {
    envIO.flatMap(env => Configured[IO, ThreeEndpoints]("MULTI").run(env))
      .unsafeRunSync()
      .assertIs(
        ThreeEndpoints(
          Endpoint("multi-ep1-host", 2),
          Endpoint("multi-ep2-host", 3),
          Endpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  test("Present valid Configured[IO, Either[Endpoint, Endpoint]]") {
    envIO.flatMap(env => Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Present valid Configured[IO, Either[Endpoint, Endpoint]] via `or` syntax") {
    envIO.flatMap(env => Configured[IO, Endpoint].or(Configured[IO, Endpoint]).value("CHOICE").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Missing Configured[IO, Either[Endpoint, Endpoint]]") {
    envIO.flatMap(env => Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  test("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint]") {
    envIO.flatMap(env => Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("CHOICE2").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  //      "CHOICE2_C1_C1_HOST" -> "choice-c1-c1-host",
  //      "CHOICE2_C1_C1_PORT" -> "7",
  //      "CHOICE2_C1_C2_HOST" -> "choice-c1-c2-host",
  //      "CHOICE2_C1_C2_PORT" -> "8",
  //      "CHOICE2_C2_HOST" -> "choice-c1-host",
  //      "CHOICE2_C2_PORT" -> "5",
  test("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax") {
    envIO
      .flatMap(
        env => {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or[Endpoint](Configured[IO, Endpoint]).or[Endpoint](Configured[IO, Endpoint])
          cfg
            .value("CHOICE2")
            .run(env)
        }
      )
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  test("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/right via `or` syntax") {
    reducedEnvIO("CHOICE2_C1_C1_HOST", "CHOICE2_C1_C1_PORT")
      .flatMap(
        env => {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
          cfg
            .value("CHOICE2")
            .run(env)
        }
      )
      .unsafeRunSync()
      .assertIs(Endpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec)
  }

  test("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] right via `or` syntax") {
    reducedEnvIO("CHOICE2_C1_C1_HOST", "CHOICE2_C1_C1_PORT", "CHOICE2_C1_C2_HOST", "CHOICE2_C1_C2_PORT")
      .flatMap(
        env => {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
          cfg
            .value("CHOICE2")
            .run(env)
        }
      )
      .unsafeRunSync()
      .assertIs(Endpoint("choice2-c2-host", 5).asRight.validNec)
  }

  test("Missing Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]") {
    envIO.flatMap(env => Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICEx").run(env))
      .unsafeRunSync()
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

  test("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    envIO.flatMap(env => Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE").run(env))
      .unsafeRunSync()
      .assertIs(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  test("Missing Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") {
    envIO.flatMap(env => Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICEx").run(env))
      .unsafeRunSync()
      .assertIs(None.validNec)
  }

  test("Present valid Configured[IO, List[Int]]") {
    envIO.flatMap(env => Configured[IO, List[Int]]("INTLIST").run(env))
      .unsafeRunSync()
      .assertIs(List(1000, 1001, 1002).validNec)
  }

  test("Missing Configured[IO, List[Int]]") {
    envIO.flatMap(env => Configured[IO, List[Int]]("INTLISTx").run(env))
      .unsafeRunSync()
      .assertIs(ConfiguredError.MissingValue("INTLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[IO, List[Endpoint]]") {
    envIO.flatMap(env => Configured[IO, List[Endpoint]]("EPLIST").run(env))
      .unsafeRunSync()
      .assertIs(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  test("Missing Configured[IO, List[Endpoint]]") {
    envIO.flatMap(env => Configured[IO, List[Endpoint]]("EPLISTx").run(env))
      .unsafeRunSync()
      .assertIs(ConfiguredError.MissingValue("EPLISTx_COUNT").invalidNec)
  }

  test("Present valid Configured[IO, List[TwoEndpoints]]") {
    envIO.flatMap(env => Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env))
      .unsafeRunSync()
      .assertIs(
        List(
          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  test("Missing Configured[IO, List[TwoEndpoints]]") {
    envIO.flatMap(env => Configured[IO, List[TwoEndpoints]]("TEPLISTx").run(env))
      .unsafeRunSync()
      .assertIs(ConfiguredError.MissingValue("TEPLISTx_COUNT").invalidNec)
  }

  test("Configured should handle newtypes") {
    envIO.flatMap(
      env =>
        Configured[IO, Int]
          .map(i => s"int[$i]")
          .value("SOME_INT")
          .run(env)
    ).unsafeRunSync()
      .assertIs("int[567]".validNec)
  }

  ////

  final case class Endpoint(host: String, port: Int)

  object Endpoint {
    implicit def configuredInstance[F[_]](implicit F: Applicative[F]): Configured[F, Endpoint] = (
      Configured[F, String].withSuffix("HOST"),
      Configured[F, Int].withSuffix("PORT")
    ).mapN(Endpoint.apply)
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit def configuredInstance[F[_]](implicit F: Applicative[F]): Configured[F, TwoEndpoints] = (
      Configured[F, Endpoint].withSuffix("EP1"),
      Configured[F, Endpoint].withSuffix("EP2")
    ).mapN(TwoEndpoints.apply)
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit def configuredInstance[F[_]](implicit F: Applicative[F]): Configured[F, ThreeEndpoints] = (
      Configured[F, Endpoint].withSuffix("EP1"),
      Configured[F, Endpoint].withSuffix("EP2"),
      Configured[F, Endpoint].withSuffix("EP3")
    ).mapN(ThreeEndpoints.apply)
  }

}
