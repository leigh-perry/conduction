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
      val map = fromMap[IO](Map(s"${k}_OPT" -> v))
      Configured[IO, Option[String]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Missing Option[Int] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> v.toString))
      Configured[IO, Option[Int]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Misconfigured Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> s"${v.toString}x"))
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

  private def envIO: IO[Environment[IO]] =
    IO(Environment.logging[IO](Environment.fromMap[IO](params), silencer[IO]))

  //  private def updatedEnv(mods: (String, String)*): Environment =
  //    Environment.logging[IO](Environment.fromMap(mods.foldLeft(params)((m, t) => m.updated(t._1, t._2))))

  private def reducedEnvIO(keys: String*): IO[Environment[IO]] =
    IO(
      Environment.logging[IO](
        Environment.fromMap[IO](params.filterNot(t => keys.contains(t._1))),
        silencer[IO]
      )
    )

  ////

  property("Present valid Configured[IO, Endpoint]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, Endpoint]("LP1").run(env)
    } yield c.shouldBe(Endpoint("lp1-host", 1).validNec)
  }

  property("Present valid Configured[IO, TwoEndpoints]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, TwoEndpoints]("MULTI").run(env)
    } yield c.shouldBe(
      TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec
    )
  }

  property("Present valid Configured[IO, ThreeEndpoints]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, ThreeEndpoints]("MULTI").run(env)
    } yield c.shouldBe(
      ThreeEndpoints(
        Endpoint("multi-ep1-host", 2),
        Endpoint("multi-ep2-host", 3),
        Endpoint("multi-ep3-host", 4)
      ).validNec
    )
  }

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
    } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]] via `or` syntax") =
    simpleTestIO {
      for {
        env <- envIO
        c <- Configured[IO, Endpoint].or(Configured[IO, Endpoint]).value("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
    }

  property("Missing Configured[IO, Either[Endpoint, Endpoint]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
    } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint]") =
    simpleTestIO {
      for {
        env <- envIO
        c <- Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("CHOICE2").run(env)
      } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
    }

  //      "CHOICE2_C1_C1_HOST" -> "choice-c1-c1-host",
  //      "CHOICE2_C1_C1_PORT" -> "7",
  //      "CHOICE2_C1_C2_HOST" -> "choice-c1-c2-host",
  //      "CHOICE2_C1_C2_PORT" -> "8",
  //      "CHOICE2_C2_HOST" -> "choice-c1-host",
  //      "CHOICE2_C2_PORT" -> "5",
  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax"
  ) = simpleTestIO {
    for {
      env <- envIO
      c <- {
        val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
          Configured[IO, Endpoint]
            .or[Endpoint](Configured[IO, Endpoint])
            .or[Endpoint](Configured[IO, Endpoint])
        cfg
          .value("CHOICE2")
          .run(env)
      }
    } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/right via `or` syntax"
  ) = simpleTestIO {
    for {
      env <- reducedEnvIO("CHOICE2_C1_C1_HOST", "CHOICE2_C1_C1_PORT")
      c <- {
        val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
          Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
        cfg
          .value("CHOICE2")
          .run(env)
      }
    } yield c.shouldBe(Endpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec)
  }

  property("Present valid Configured[IO, Either[Either[,]], Endpoint] right via `or` syntax") =
    simpleTestIO {
      for {
        env <- reducedEnvIO(
          "CHOICE2_C1_C1_HOST",
          "CHOICE2_C1_C1_PORT",
          "CHOICE2_C1_C2_HOST",
          "CHOICE2_C1_C2_PORT"
        )
        c <- {
          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
            Configured[IO, Endpoint].or(Configured[IO, Endpoint]).or(Configured[IO, Endpoint])
          cfg
            .value("CHOICE2")
            .run(env)
        }
      } yield c.shouldBe(Endpoint("choice2-c2-host", 5).asRight.validNec)
    }

  property("Missing Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICEx").run(env)
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
    } yield c.shouldSatisfy(
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

  property("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    simpleTestIO {
      for {
        env <- envIO
        c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE").run(env)
      } yield c.shouldBe(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
    }

  property("Missing Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    simpleTestIO {
      for {
        env <- envIO
        c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICEx")
          .run(env)
      } yield c.shouldBe(None.validNec)
    }

  property("Present valid Configured[IO, List[Int]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[Int]]("INTLIST").run(env)
    } yield c.shouldBe(List(1000, 1001, 1002).validNec)
  }

  property("Missing Configured[IO, List[Int]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[Int]]("INTLISTx").run(env)
    } yield c.shouldBe(ConfiguredError.MissingValue("INTLISTx_COUNT").invalidNec)
  }

  property("Present valid Configured[IO, List[Endpoint]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
    } yield c.shouldBe(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  property("Missing Configured[IO, List[Endpoint]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[Endpoint]]("EPLISTx").run(env)
    } yield c.shouldBe(ConfiguredError.MissingValue("EPLISTx_COUNT").invalidNec)
  }

  property("Present valid Configured[IO, List[TwoEndpoints]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env)
    } yield c.shouldBe(
      List(
        TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
        TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
      ).validNec
    )
  }

  property("Missing Configured[IO, List[TwoEndpoints]]") = simpleTestIO {
    for {
      env <- envIO
      c <- Configured[IO, List[TwoEndpoints]]("TEPLISTx").run(env)
    } yield c.shouldBe(ConfiguredError.MissingValue("TEPLISTx_COUNT").invalidNec)
  }

  property("Configured should handle newtypes") = simpleTestIO {
    for {
      env <- envIO
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
