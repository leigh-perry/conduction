package com.leighperry.conduction.config

import cats.Monad
import cats.data.{ NonEmptyChain, Validated }
import cats.effect.IO
import cats.syntax.contravariantSemigroupal._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.testsupport.EnvGenerators._
import com.leighperry.conduction.config.testsupport.TestSupport
import org.scalacheck.{ Arbitrary, Properties }

object ConfigSupportTest extends Properties("Config support") with TestSupport {

  import Environment._

  final case class KV[V](key: String, v: V)

  def genKV[V: Arbitrary] =
    for {
      key <- genSymbol(1, 20)
      v <- genFor[V]
    } yield KV(key, v)

  property("Primitive String values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v))
      Configured[IO, String](k)
        .run(map)
        .map(c => c.shouldBe(v.validNec))
  }

  property("Primitive Int values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v.toString))
      Configured[IO, Int](k)
        .run(map)
        .map(c => c.shouldBe(v.validNec))
  }

  property("Present Option[String] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val entries = Map(s"${k}_OPT" -> v)
      val map = fromMap[IO](entries)
      Configured[IO, Option[String]](k)
        .run(map)
        .map {
          c =>
            c.shouldBe(v.some.validNec) &&
            Configured[IO, Option[String]].description(k).shouldBe(entries.keys.toList)
        }
  }

  property("Present Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val entries = Map(s"${k}_OPT" -> v.toString)
      val map = fromMap[IO](entries)
      Configured[IO, Option[Int]](k)
        .run(map)
        .map {
          c =>
            c.shouldBe(v.some.validNec) &&
            Configured[IO, Option[Int]].description(k).shouldBe(entries.keys.toList)
        }
  }

  property("Missing Option[String] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v))
      Configured[IO, Option[String]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Missing Option[Int] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v.toString))
      Configured[IO, Option[Int]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Misconfigured Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> s"${v}x"))
      Configured[IO, Option[Int]](k)
        .run(map)
        .map(
          c =>
            c.shouldSatisfy {
              case Validated.Invalid(nec) =>
                nec.shouldBeNec(
                  NonEmptyChain(
                    ConfiguredError.invalidValue(s"${k}_OPT", s"${v}x")
                  )
                )
              case _ => false
            }
        )
  }

  property("Present valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val entries = Map(k -> v)
    val map = fromMap[IO](entries)
    Configured[IO, Double]("A_DOUBLE").run(map).map {
      c =>
        c.shouldBe(1.23.validNec) &&
        Configured[IO, Double].description(k).shouldBe(entries.keys.toList)
    }
  }

  property("Missing valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double]("MISSING")
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.missingValue("MISSING").invalidNec))
  }

  property("Invalid valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double](k)
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.invalidValue(k, v).invalidNec))
  }

  ////

  property("Present valid Configured[IO, Endpoint]") = forAllIO(
    genEnvIO(
      Map(
        "LP1_HOST" -> "lp1-host",
        "LP1_PORT" -> "1"
      ),
      "test01"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Endpoint]("LP1").run(env)
      } yield c.shouldBe(Endpoint("lp1-host", 1).validNec) &&
        Configured[IO, Endpoint].description("LP1").shouldBe(List("LP1_HOST", "LP1_PORT"))
  }

  property("Present valid Configured[IO, TwoEndpoints]") = forAllIO(
    genEnvIO(
      Map(
        "MULTI_EP1_HOST" -> "multi-ep1-host",
        "MULTI_EP1_PORT" -> "2",
        "MULTI_EP2_HOST" -> "multi-ep2-host",
        "MULTI_EP2_PORT" -> "3"
      ),
      "test02"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, TwoEndpoints]("MULTI").run(env)
      } yield c.shouldBe(TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec) &&
        Configured[IO, TwoEndpoints]
          .description("MULTI")
          .shouldBe(List("MULTI_EP1_HOST", "MULTI_EP1_PORT", "MULTI_EP2_HOST", "MULTI_EP2_PORT"))

  }

  property("Present valid Configured[IO, ThreeEndpoints]") = forAllIO(
    genEnvIO(
      Map(
        "MULTI_EP1_HOST" -> "multi-ep1-host",
        "MULTI_EP1_PORT" -> "2",
        "MULTI_EP2_HOST" -> "multi-ep2-host",
        "MULTI_EP2_PORT" -> "3",
        "MULTI_EP3_HOST" -> "multi-ep3-host",
        "MULTI_EP3_PORT" -> "4"
      ),
      "test03"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, ThreeEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        ThreeEndpoints(
          Endpoint("multi-ep1-host", 2),
          Endpoint("multi-ep2-host", 3),
          Endpoint("multi-ep3-host", 4)
        ).validNec
      ) &&
        Configured[IO, ThreeEndpoints]
          .description("MULTI")
          .shouldBe(
            List(
              "MULTI_EP1_HOST",
              "MULTI_EP1_PORT",
              "MULTI_EP2_HOST",
              "MULTI_EP2_PORT",
              "MULTI_EP3_HOST",
              "MULTI_EP3_PORT"
            )
          )
  }

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]]") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test04"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid) &&
        Configured[IO, Either[Endpoint, Endpoint]]
          .description("CHOICE")
          .shouldBe(List("CHOICE_C1_HOST", "CHOICE_C1_PORT", "CHOICE_C2_HOST", "CHOICE_C2_PORT"))
  }

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]] via `or` syntax") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test05"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Endpoint].or(Configured[IO, Endpoint]).value("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid) &&
        Configured[IO, Endpoint]
          .or(Configured[IO, Endpoint])
          .description("CHOICE")
          .shouldBe(List("CHOICE_C1_HOST", "CHOICE_C1_PORT", "CHOICE_C2_HOST", "CHOICE_C2_PORT"))
  }

  property("Missing Configured[IO, Either[Endpoint, Endpoint]]") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test06"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint]") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_C1_HOST" -> "choice-c1-c1-host",
        "CHOICE_C1_C1_PORT" -> "7"
      ),
      "test07"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec) &&
        Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]
          .description("CHOICE")
          .shouldBe(
            List(
              "CHOICE_C1_C1_HOST",
              "CHOICE_C1_C1_PORT",
              "CHOICE_C1_C2_HOST",
              "CHOICE_C1_C2_PORT",
              "CHOICE_C2_HOST",
              "CHOICE_C2_PORT"
            )
          )
  }

  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax"
  ) = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_C1_HOST" -> "choice-c1-c1-host",
        "CHOICE_C1_C1_PORT" -> "7"
      ),
      "test08"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint]
              .or[Endpoint](Configured[IO, Endpoint])
              .or[Endpoint](Configured[IO, Endpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec) &&
        Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]
          .description("CHOICE")
          .shouldBe(
            List(
              "CHOICE_C1_C1_HOST",
              "CHOICE_C1_C1_PORT",
              "CHOICE_C1_C2_HOST",
              "CHOICE_C1_C2_PORT",
              "CHOICE_C2_HOST",
              "CHOICE_C2_PORT"
            )
          )
  }

  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/right via `or` syntax"
  ) = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
        "CHOICE_C1_C2_PORT" -> "8",
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5",
        "CHOICE_OPT_C2_C1_HOST" -> "choice-opt-c2-c1-host",
        "CHOICE_OPT_C2_C1_PORT" -> "9"
      ),
      "test09"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(Endpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec) &&
        Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]
          .description("CHOICE")
          .shouldBe(
            List(
              "CHOICE_C1_C1_HOST",
              "CHOICE_C1_C1_PORT",
              "CHOICE_C1_C2_HOST",
              "CHOICE_C1_C2_PORT",
              "CHOICE_C2_HOST",
              "CHOICE_C2_PORT"
            )
          )
  }

  property("Present valid Configured[IO, Either[Either[,]], Endpoint] right via `or` syntax") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5"
      ),
      "test10"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(Endpoint("choice2-c2-host", 5).asRight.validNec) &&
        Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]
          .description("CHOICE")
          .shouldBe(
            List(
              "CHOICE_C1_C1_HOST",
              "CHOICE_C1_C1_PORT",
              "CHOICE_C1_C2_HOST",
              "CHOICE_C1_C2_PORT",
              "CHOICE_C2_HOST",
              "CHOICE_C2_PORT"
            )
          )
  }

  private val separator = "_"

  def keyOf(parts: String*): Key =
    parts.mkString(separator)

  property("Missing Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]") =
    forAllIO(genEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICE").run(env)
        } yield c.shouldSatisfy(
          _.fold(
            e =>
              e.shouldBeNec(
                NonEmptyChain(
                  ConfiguredError.missingValue(keyOf("CHOICE", "C1", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C1", "PORT")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C1", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C1", "PORT")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C2", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C2", "PORT"))
                )
              ),
            _ => false
          )
        )
    }

  property("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_OPT_C2_C1_HOST" -> "choice-opt-c2-c1-host",
        "CHOICE_OPT_C2_C1_PORT" -> "9"
      ),
      "test11"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
          .run(env)
      } yield c.shouldBe(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid) &&
        Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]
          .description("CHOICE")
          .shouldBe(
            List(
              "CHOICE_OPT_C1_HOST",
              "CHOICE_OPT_C1_PORT",
              "CHOICE_OPT_C2_C1_HOST",
              "CHOICE_OPT_C2_C1_PORT",
              "CHOICE_OPT_C2_C2_HOST",
              "CHOICE_OPT_C2_C2_PORT"
            )
          )
  }

  property("Missing Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    forAllIO(genEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
            .run(env)
        } yield c.shouldBe(None.validNec)
    }

  property("Present valid Configured[IO, List[Int]]") = forAllIO(
    genEnvIO(
      Map(
        "INTLIST_COUNT" -> "3",
        "INTLIST_0" -> "1000",
        "INTLIST_1" -> "1001",
        "INTLIST_2" -> "1002"
      ),
      "test12"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(List(1000, 1001, 1002).validNec) &&
        Configured[IO, List[Int]]
          .description("INTLIST")
          .shouldBe(List("INTLIST_COUNT", "INTLIST_n"))
  }

  property("Missing Configured[IO, List[Int]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("INTLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[Endpoint]]") = forAllIO(
    genEnvIO(
      Map(
        "EPLIST_COUNT" -> "2",
        "EPLIST_0_HOST" -> "eplist0-host",
        "EPLIST_0_PORT" -> "2",
        "EPLIST_1_HOST" -> "eplist1-host",
        "EPLIST_1_PORT" -> "3"
      ),
      "test13"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
      } yield c.shouldBe(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec) &&
        Configured[IO, List[Endpoint]]
          .description("EPLIST")
          .shouldBe(List("EPLIST_COUNT", "EPLIST_n_HOST", "EPLIST_n_PORT"))
  }

  property("Missing Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("EPLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[TwoEndpoints]]") = forAllIO(
    genEnvIO(
      Map(
        "TEPLIST_COUNT" -> "2",
        "TEPLIST_0_EP1_HOST" -> "teplist0-ep1-host",
        "TEPLIST_0_EP1_PORT" -> "7",
        "TEPLIST_0_EP2_HOST" -> "multilist-ep1-host0",
        "TEPLIST_0_EP2_PORT" -> "7",
        "TEPLIST_1_EP1_HOST" -> "teplist1-ep1-host",
        "TEPLIST_1_EP1_PORT" -> "7",
        "TEPLIST_1_EP2_HOST" -> "multilist-ep2-host1",
        "TEPLIST_1_EP2_PORT" -> "7",
        "SOMEINT" -> "567"
      ),
      "test14"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(
        List(
          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
        ).validNec
      ) &&
        Configured[IO, List[TwoEndpoints]]
          .description("TEPLIST")
          .shouldBe(
            List(
              "TEPLIST_COUNT",
              "TEPLIST_n_EP1_HOST",
              "TEPLIST_n_EP1_PORT",
              "TEPLIST_n_EP2_HOST",
              "TEPLIST_n_EP2_PORT"
            )
          )

  }

  property("Missing Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("TEPLIST", "COUNT")).invalidNec)
  }

  property("Configured should handle newtypes") = forAllIO(
    genEnvIO(
      Map(
        "SOMEINT" -> "567"
      ),
      "test15"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Int].map(i => s"int[$i]").value("SOMEINT").run(env)
      } yield c.shouldBe("int[567]".validNec) &&
        Configured[IO, Int]
          .description("SOMEINT")
          .shouldBe(List("SOMEINT"))
  }

  ////

  final case class Endpoint(host: String, port: Int)

  object Endpoint {
    implicit def configuredInstance[F[_]](implicit F: Monad[F]): Configured[F, Endpoint] =
      (
        Configured[F, String].withSuffix("HOST"),
        Configured[F, Int].withSuffix("PORT")
      ).mapN(Endpoint.apply)
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit def configuredInstance[F[_]](implicit F: Monad[F]): Configured[F, TwoEndpoints] =
      (
        Configured[F, Endpoint].withSuffix("EP1"),
        Configured[F, Endpoint].withSuffix("EP2")
      ).mapN(TwoEndpoints.apply)
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit def configuredInstance[F[_]](
      implicit F: Monad[F]
    ): Configured[F, ThreeEndpoints] =
      (
        Configured[F, Endpoint].withSuffix("EP1"),
        Configured[F, Endpoint].withSuffix("EP2"),
        Configured[F, Endpoint].withSuffix("EP3")
      ).mapN(ThreeEndpoints.apply)
  }

}
