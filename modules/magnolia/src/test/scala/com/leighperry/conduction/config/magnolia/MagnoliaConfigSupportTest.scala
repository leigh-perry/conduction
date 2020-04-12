package com.leighperry.conduction.config.magnolia

import cats.data.{ NonEmptyChain, Validated }
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.magnolia.AutoConfigInstancesIO._
import com.leighperry.conduction.config.testsupport.EnvGenerators._
import com.leighperry.conduction.config.testsupport.TestSupport
import com.leighperry.conduction.config.{ Configured, ConfiguredError, Environment }
import org.scalacheck.{ Arbitrary, Properties }

object MagnoliaConfigSupportTest extends Properties("Config support") with TestSupport {

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
      val map = fromMap[IO](Map(s"${k}_OPT" -> v))
      Configured[IO, Option[String]](k)
        .run(map)
        .map(c => c.shouldBe(v.some.validNec))
  }

  property("Present Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> v.toString))
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

  property("Present valid Configured[IO, MagnoliaEndpoint]") = forAllIO(
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
        c <- Configured[IO, MagnoliaEndpoint]("LP1").run(env)
      } yield c.shouldBe(MagnoliaEndpoint("lp1-host", 1).validNec)
  }

  property("Present valid Configured[IO, TwoMagnoliaEndpoints]") = forAllIO(
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
        c <- Configured[IO, TwoMagnoliaEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        TwoMagnoliaEndpoints(MagnoliaEndpoint("multi-ep1-host", 2), MagnoliaEndpoint("multi-ep2-host", 3)).validNec
      )
  }

  property("Present valid Configured[IO, ThreeMagnoliaEndpoints]") = forAllIO(
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
        c <- Configured[IO, ThreeMagnoliaEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        ThreeMagnoliaEndpoints(
          MagnoliaEndpoint("multi-ep1-host", 2),
          MagnoliaEndpoint("multi-ep2-host", 3),
          MagnoliaEndpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  property("Present valid Configured[IO, Either[MagnoliaEndpoint, MagnoliaEndpoint]]") = forAllIO(
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
        c <- Configured[IO, Either[MagnoliaEndpoint, MagnoliaEndpoint]]("CHOICE").run(env)
      } yield c.shouldBe(MagnoliaEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[MagnoliaEndpoint, MagnoliaEndpoint]] via `or` syntax") = forAllIO(
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
        c <- Configured[IO, MagnoliaEndpoint].or(Configured[IO, MagnoliaEndpoint]).value("CHOICE").run(env)
      } yield c.shouldBe(MagnoliaEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Missing Configured[IO, Either[MagnoliaEndpoint, MagnoliaEndpoint]]") = forAllIO(
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
        c <- Configured[IO, Either[MagnoliaEndpoint, MagnoliaEndpoint]]("CHOICE").run(env)
      } yield c.shouldBe(MagnoliaEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint]], MagnoliaEndpoint]") =
    forAllIO(
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
          c <- Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint], MagnoliaEndpoint]]("CHOICE").run(env)
        } yield c.shouldBe(MagnoliaEndpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
    }

  property(
    "Present valid Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint]], MagnoliaEndpoint] left/left via `or` syntax"
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
          val cfg: Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint], MagnoliaEndpoint]] =
            Configured[IO, MagnoliaEndpoint]
              .or[MagnoliaEndpoint](Configured[IO, MagnoliaEndpoint])
              .or[MagnoliaEndpoint](Configured[IO, MagnoliaEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(MagnoliaEndpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  property(
    "Present valid Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint]], MagnoliaEndpoint] left/right via `or` syntax"
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
          val cfg: Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint], MagnoliaEndpoint]] =
            Configured[IO, MagnoliaEndpoint].or(Configured[IO, MagnoliaEndpoint]).or(Configured[IO, MagnoliaEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(MagnoliaEndpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec)
  }

  property("Present valid Configured[IO, Either[Either[,]], MagnoliaEndpoint] right via `or` syntax") = forAllIO(
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
          val cfg: Configured[IO, Either[Either[MagnoliaEndpoint, MagnoliaEndpoint], MagnoliaEndpoint]] =
            Configured[IO, MagnoliaEndpoint].or(Configured[IO, MagnoliaEndpoint]).or(Configured[IO, MagnoliaEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(MagnoliaEndpoint("choice2-c2-host", 5).asRight.validNec)
  }

  private val separator = "_"

  def keyOf(parts: String*): Key =
    parts.mkString(separator)

  property("Missing Configured[IO, Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]") =
    forAllIO(genEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]("CHOICE").run(env)
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

  property(
    "Present valid Configured[IO, Option[Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]]"
  ) = forAllIO(
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
        c <- Configured[IO, Option[Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]]("CHOICE")
          .run(env)
      } yield c.shouldBe(MagnoliaEndpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  property("Missing Configured[IO, Option[Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]]") =
    forAllIO(genEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[MagnoliaEndpoint, Either[MagnoliaEndpoint, MagnoliaEndpoint]]]]("CHOICE")
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
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("INTLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[MagnoliaEndpoint]]") = forAllIO(
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
        c <- Configured[IO, List[MagnoliaEndpoint]]("EPLIST").run(env)
      } yield c.shouldBe(List(MagnoliaEndpoint("eplist0-host", 2), MagnoliaEndpoint("eplist1-host", 3)).validNec)
  }

  property("Missing Configured[IO, List[MagnoliaEndpoint]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[MagnoliaEndpoint]]("EPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("EPLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[TwoMagnoliaEndpoints]]") = forAllIO(
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
        c <- Configured[IO, List[TwoMagnoliaEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(
        List(
          TwoMagnoliaEndpoints(MagnoliaEndpoint("teplist0-ep1-host", 7), MagnoliaEndpoint("multilist-ep1-host0", 7)),
          TwoMagnoliaEndpoints(MagnoliaEndpoint("teplist1-ep1-host", 7), MagnoliaEndpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  property("Missing Configured[IO, List[TwoMagnoliaEndpoints]]") = forAllIO(genEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoMagnoliaEndpoints]]("TEPLIST").run(env)
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
      } yield c.shouldBe("int[567]".validNec)
  }

  ////

  final case class MagnoliaEndpoint(host: String, port: Int)
  final case class TwoMagnoliaEndpoints(ep1: MagnoliaEndpoint, ep2: MagnoliaEndpoint)
  final case class ThreeMagnoliaEndpoints(ep1: MagnoliaEndpoint, ep2: MagnoliaEndpoint, ep3: MagnoliaEndpoint)

}
