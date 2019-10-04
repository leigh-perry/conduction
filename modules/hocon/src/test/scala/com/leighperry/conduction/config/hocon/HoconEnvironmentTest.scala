package com.leighperry.conduction.config.hocon

import cats.Monad
import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.contravariantSemigroupal._
import cats.syntax.either._
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
        c <- Configured[IO, Endpoint]("LP1").run(env)
      } yield c.shouldBe(Endpoint("lp1-host", 1).validNec)
  }

  property("Present valid Configured[IO, TwoEndpoints]") = forAllIO(genEnvIO("test02.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, TwoEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        TwoEndpoints(Endpoint("multi-ep1-host", 2), Endpoint("multi-ep2-host", 3)).validNec
      )
  }

  property("Present valid Configured[IO, ThreeEndpoints]") = forAllIO(genEnvIO("test03.conf")) {
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

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]]") =
    forAllIO(genEnvIO("test04.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
        } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
    }

  property("Present valid Configured[IO, Either[Endpoint, Endpoint]] via `or` syntax") =
    forAllIO(genEnvIO("test05.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Endpoint].or(Configured[IO, Endpoint]).value("CHOICE").run(env)
        } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
    }

  property("Missing Configured[IO, Either[Endpoint, Endpoint]]") =
    forAllIO(genEnvIO("test06.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Endpoint, Endpoint]]("CHOICE").run(env)
        } yield c.shouldBe(Endpoint("choice-c1-host", 5).asLeft.valid)
    }

  property("Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint]") =
    forAllIO(genEnvIO("test07.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Either[Endpoint, Endpoint], Endpoint]]("CHOICE").run(env)
        } yield c.shouldBe(Endpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
    }

  property(
    "Present valid Configured[IO, Either[Either[Endpoint, Endpoint]], Endpoint] left/left via `or` syntax"
  ) = forAllIO(genEnvIO("test08.conf")) {
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
  ) = forAllIO(genEnvIO("test09.conf")) {
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
    forAllIO(genEnvIO("test10.conf")) {
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
    forAllIO(genEnvIO("empty.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Endpoint, Either[Endpoint, Endpoint]]]("CHOICE").run(env)
        } yield c.shouldSatisfy(
          _.fold(
            e =>
              e.shouldBeNec(
                NonEmptyChain(
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C1", "HOST")),
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C1", "PORT")),
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C2", "C1", "HOST")),
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C2", "C1", "PORT")),
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C2", "C2", "HOST")),
                  ConfiguredError.MissingValue(NonEmptyChain("CHOICE", "C2", "C2", "PORT"))
                )
              ),
            _ => false
          )
        )
    }

  property("Present valid Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    forAllIO(genEnvIO("test11.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
            .run(env)
        } yield c.shouldBe(Endpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
    }

  property("Missing Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]") =
    forAllIO(genEnvIO("empty.conf")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[Endpoint, Either[Endpoint, Endpoint]]]]("CHOICE")
            .run(env)
        } yield c.shouldBe(None.validNec)
    }

  property("Present valid Configured[IO, List[Int]]") = forAllIO(genEnvIO("test12.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(List(1000, 1001, 1002).validNec)
  }

  property("Missing Configured[IO, List[Int]]") = forAllIO(genEnvIO("empty.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue(NonEmptyChain("INTLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO("test13.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
      } yield c.shouldBe(List(Endpoint("eplist0-host", 2), Endpoint("eplist1-host", 3)).validNec)
  }

  property("Missing Configured[IO, List[Endpoint]]") = forAllIO(genEnvIO("empty.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Endpoint]]("EPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue(NonEmptyChain("EPLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO("test14.conf")) {
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

  property("Missing Configured[IO, List[TwoEndpoints]]") = forAllIO(genEnvIO("empty.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.MissingValue(NonEmptyChain("TEPLIST", "COUNT")).invalidNec)
  }

  property("Configured should handle newtypes") = forAllIO(genEnvIO("test15.conf")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Int].map(i => s"int[$i]").value("SOMEINT").run(env)
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
