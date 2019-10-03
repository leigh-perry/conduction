package com.leighperry.conduction.config

import cats.Monad
import cats.data.Validated
import cats.effect.IO
import cats.syntax.contravariantSemigroupal._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.testsupport.TestSupport
import org.scalacheck.{ Arbitrary, Gen, Properties }

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
      val map = fromMap[IO](Map(k -> v))
      Configured[IO, Option[String]](k)
        .run(map)
        .map(c => c.shouldBe(v.some.validNec))
  }

  property("Present Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v.toString))
      Configured[IO, Option[Int]](k)
        .run(map)
        .map(c => c.shouldBe(v.some.validNec))
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
      val map = fromMap[IO](Map(k -> s"${v.toString}x"))
      Configured[IO, Option[Int]](k)
        .run(map)
        .map(
          c =>
            c.shouldSatisfy {
              case Validated.Invalid(nec) =>
                nec.length.shouldBe(1) && nec.forall(_.isInstanceOf[ConfiguredError.InvalidValue])
              case _ => false
            }
        )
  }

  property("Present valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val map = fromMap[IO](Map(k -> v))
    for {
      c <- Configured[IO, Double]("A_DOUBLE").run(map)
    } yield c.shouldBe(1.23.validNec)
  }

  property("Missing valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double]("MISSING")
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.MissingValue("MISSING").invalidNec))
  }

  property("Invalid valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double](k)
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.InvalidValue(k, v).invalidNec))
  }

  ////

  private def envIO(params: Map[String, String]): IO[Environment[IO]] =
    IO(logging[IO](fromMap[IO](params), silencer[IO]))

  private def propertyFileIO(propertiesFilename: String): IO[Environment[IO]] =
    for {
      fp <- IO(getClass.getClassLoader.getResource(s"$propertiesFilename.properties"))
      ep <- fromPropertiesFile[IO](fp.getFile)
      q <- IO(logging[IO](ep, silencer[IO]))
    } yield q

  private def genEnvIO(
    params: Map[String, String],
    propertiesFilename: String
  ): Gen[IO[Environment[IO]]] =
    Gen.oneOf(Gen.const(envIO(params)), Gen.const(propertyFileIO(propertiesFilename)))

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
      } yield c.shouldBe(Endpoint("lp1-host", 1).validNec)
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
      } yield c.shouldBe(
        TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec
      )
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
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
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
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
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
        "CHOICE_C1_C1_PORT" -> "7",
        "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
        "CHOICE_C1_C2_PORT" -> "8",
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5"
      ),
      "test07"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax"
  ) = forAllIO(
    genEnvIO(
      Map(
        "CHOICE_C1_C1_HOST" -> "choice-c1-c1-host",
        "CHOICE_C1_C1_PORT" -> "7",
        "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
        "CHOICE_C1_C2_PORT" -> "8",
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5"
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
      } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
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
      } yield c.shouldBe(Endpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec)
  }

  property("Present valid Configured[IO, Either[Either[,]], Endpoint] right via `or` syntax") =
    forAllIO(
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
        } yield c.shouldBe(Endpoint("choice2-c2-host", 5).asRight.validNec)
    }

  property("Missing Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]") =
    forAllIO(genEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICE").run(env)
        } yield c.shouldSatisfy(
          _.fold(
            e => {
              e.length == 6 &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C1_HOST")) &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C1_PORT")) &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C2_C1_HOST")) &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C2_C1_PORT")) &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C2_C2_HOST")) &&
                e.exists(_ == ConfiguredError.MissingValue("CHOICE_C2_C2_PORT"))
            },
            _ => false
          )
        )
    }

  property("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    forAllIO(
      genEnvIO(
        Map(
          "CHOICE_C2_C1_HOST" -> "choice-opt-c2-c1-host",
          "CHOICE_C2_C1_PORT" -> "9"
        ),
        "test11"
      )
    ) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
            .run(env)
        } yield c.shouldBe(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
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
      } yield c.shouldBe(List(1000, 1001, 1002).validNec)
  }

  property("Missing Configured[IO, List[Int]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue("INTLIST_COUNT").invalidNec)
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
      } yield c.shouldBe(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  property("Missing Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue("EPLIST_COUNT").invalidNec)
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
        "SOME_INT" -> "567"
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
      )
  }

  property("Missing Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue("TEPLIST_COUNT").invalidNec)
  }

  property("Configured should handle newtypes") = forAllIO(
    genEnvIO(
      Map(
        "SOME_INT" -> "567"
      ),
      "test15"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Int].map(i => s"int[$i]").value("SOME_INT").run(env)
      } yield c.shouldBe("int[567]".validNec)
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
