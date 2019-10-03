package com.leighperry.conduction.config.hocon

import cats.Monad
import cats.effect.IO
import cats.syntax.contravariantSemigroupal._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.testsupport.TestSupport
import com.leighperry.conduction.config.{ Configured, ConfiguredError, Environment }
import org.scalacheck.{ Gen, Properties }

object HoconEnvironmentTest extends Properties("Hocon support") with TestSupport {

  import Environment._

  private def hoconFileIO(propertiesFilename: String): Gen[IO[Environment[IO]]] =
    for {
      fp <- IO(getClass.getClassLoader.getResource(propertiesFilename))
      ep <- HoconEnvironment.fromHoconFile[IO](fp.getFile)
      q <- IO(logging[IO](ep, silencer[IO]))
    } yield q

  private def genEnvIO(propertiesFilename: String): Gen[IO[Environment[IO]]] =
    hoconFileIO(propertiesFilename)

  ////

  property("Present valid Configured[IO, Endpoint]") = forAllIO(genEnvIO("test01.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Endpoint]("lp1").run(env)
      } yield c.shouldBe(Endpoint("lp1-host", 1).validNec)
  }

//  property("Present valid Configured[IO, TwoEndpoints]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, TwoEndpoints]("multi").run(env)
//      } yield c.shouldBe(
//        TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec
//      )
//  }
//
//  property("Present valid Configured[IO, ThreeEndpoints]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, ThreeEndpoints]("multi").run(env)
//      } yield c.shouldBe(
//        ThreeEndpoints(
//          Endpoint("multi-ep1-host", 2),
//          Endpoint("multi-ep2-host", 3),
//          Endpoint("multi-ep3-host", 4)
//        ).validNec
//      )
//  }
//
//  property("Present valid Configured[IO, Either[Endpoint, Endpoint]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, Either[Endpoint, Endpoint]]("choice").run(env)
//      } yield c.shouldBe(Endpoint("choice-C1-host", 5).asLeft.valid)
//  }
//
//  property("Present valid Configured[IO, Either[Endpoint, Endpoint]] via `or` syntax") =
//    forAllIO(genEnvIO) {
//      e =>
//        for {
//          env <- e
//          c <- Configured[IO, Endpoint].or(Configured[IO, Endpoint]).value("choice").run(env)
//        } yield c.shouldBe(Endpoint("choice-C1-host", 5).asLeft.valid)
//    }
//
//  property("Missing Configured[IO, Either[Endpoint, Endpoint]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, Either[Endpoint, Endpoint]]("choice").run(env)
//      } yield c.shouldBe(Endpoint("choice-C1-host", 5).asLeft.valid)
//  }
//
//  property("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint]") =
//    forAllIO(genEnvIO) {
//      e =>
//        for {
//          env <- e
//          c <- Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("choice2").run(env)
//        } yield c.shouldBe(Endpoint("choice-C1-C1-host", 7).asLeft.asLeft.validNec)
//    }
//
//  property(
//    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax"
//  ) = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- {
//          val cfg: Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]] =
//            Configured[IO, Endpoint]
//              .or[Endpoint](Configured[IO, Endpoint])
//              .or[Endpoint](Configured[IO, Endpoint])
//          cfg
//            .value("choice2")
//            .run(env)
//        }
//      } yield c.shouldBe(Endpoint("choice-C1-C1-host", 7).asLeft.asLeft.validNec)
//  }
//
//  property("Missing Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]") =
//    forAllIO(genEnvIO) {
//      e =>
//        for {
//          env <- e
//          c <- Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("choicex").run(env)
//        } yield c.shouldSatisfy(
//          _.fold(
//            e => {
//              e.length == 6 &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C1_host")) &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C1_port")) &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C2_C1_host")) &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C2_C1_port")) &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C2_C2_host")) &&
//                e.exists(_ == ConfiguredError.MissingValue("choicex_C2_C2_port"))
//            },
//            _ => false
//          )
//        )
//    }
//
//  property("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
//    forAllIO(genEnvIO) {
//      e =>
//        for {
//          env <- e
//          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("choice")
//            .run(env)
//        } yield c.shouldBe(Endpoint("choice-opt-C2-C1-host", 9).asLeft.asRight.some.valid)
//    }
//
//  property("Missing Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
//    forAllIO(genEnvIO) {
//      e =>
//        for {
//          env <- e
//          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("choicex")
//            .run(env)
//        } yield c.shouldBe(None.validNec)
//    }
//
//  property("Present valid Configured[IO, List[Int]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[Int]]("intlist").run(env)
//      } yield c.shouldBe(List(1000, 1001, 1002).validNec)
//  }
//
//  property("Missing Configured[IO, List[Int]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[Int]]("intlistx").run(env)
//      } yield c.shouldBe(ConfiguredError.MissingValue("intlistx_COUNT").invalidNec)
//  }
//
//  property("Present valid Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[Endpoint]]("eplist").run(env)
//      } yield c.shouldBe(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
//  }
//
//  property("Missing Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[Endpoint]]("eplistx").run(env)
//      } yield c.shouldBe(ConfiguredError.MissingValue("eplistx_COUNT").invalidNec)
//  }
//
//  property("Present valid Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[TwoEndpoints]]("teplist").run(env)
//      } yield c.shouldBe(
//        List(
//          TwoEndpoints(Endpoint("teplist0-ep1-host", 7), Endpoint("multilist-ep1-host0", 7)),
//          TwoEndpoints(Endpoint("teplist1-ep1-host", 7), Endpoint("multilist-ep2-host1", 7))
//        ).validNec
//      )
//  }
//
//  property("Missing Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, List[TwoEndpoints]]("teplistx").run(env)
//      } yield c.shouldBe(ConfiguredError.MissingValue("teplistx_COUNT").invalidNec)
//  }
//
//  property("Configured should handle newtypes") = forAllIO(genEnvIO) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, Int].map(i => s"int[$i]").value("some_int").run(env)
//      } yield c.shouldBe("int[567]".validNec)
//  }

  ////

  final case class Endpoint(host: String, port: Int)

  object Endpoint {
    implicit def configuredInstance[F[_]](implicit F: Monad[F]): Configured[F, Endpoint] =
      (
        Configured[F, String].withSuffix("host"),
        Configured[F, Int].withSuffix("port")
      ).mapN(Endpoint.apply)
  }

  final case class TwoEndpoints(ep1: Endpoint, ep2: Endpoint)

  object TwoEndpoints {
    implicit def configuredInstance[F[_]](implicit F: Monad[F]): Configured[F, TwoEndpoints] =
      (
        Configured[F, Endpoint].withSuffix("ep1"),
        Configured[F, Endpoint].withSuffix("ep2")
      ).mapN(TwoEndpoints.apply)
  }

  final case class ThreeEndpoints(ep1: Endpoint, ep2: Endpoint, ep3: Endpoint)

  object ThreeEndpoints {
    implicit def configuredInstance[F[_]](
      implicit F: Monad[F]
    ): Configured[F, ThreeEndpoints] =
      (
        Configured[F, Endpoint].withSuffix("ep1"),
        Configured[F, Endpoint].withSuffix("ep2"),
        Configured[F, Endpoint].withSuffix("ep3")
      ).mapN(ThreeEndpoints.apply)
  }

}
