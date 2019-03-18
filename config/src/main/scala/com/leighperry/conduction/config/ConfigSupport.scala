package com.leighperry.conduction.config

import cats.data.{Kleisli, ValidatedNec}
import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Applicative, Functor, Monad, Show}

/**
  * Support for reading detailed, nested configuration from environment variables etc
  */

// Low level conversion from String
trait Conversion[A] {
  def of(s: String): Either[String, A]
}

object Conversion {
  def apply[A](implicit F: Conversion[A]): Conversion[A] = F

  implicit val conversionInt: Conversion[Int] =
    (s: String) => eval(s, _.toInt)

  implicit val conversionLong: Conversion[Long] =
    (s: String) => eval(s, _.toLong)

  implicit val conversionDouble: Conversion[Double] =
    (s: String) => eval(s, _.toDouble)

  implicit val conversionBoolean: Conversion[Boolean] =
    (s: String) => eval(s, _.toBoolean)

  implicit val conversionString: Conversion[String] =
    (s: String) => s.asRight

  implicit val functorConversion: Functor[Conversion] =
    new Functor[Conversion] {
      override def map[A, B](fa: Conversion[A])(f: A => B): Conversion[B] =
        (s: String) => fa.of(s).map(f)
    }

  private def eval[A](s: String, f: String => A): Either[String, A] = {
    Either.catchNonFatal(f(s))
      .leftMap(_ => s)
  }
}

////

trait ConfiguredError

object ConfiguredError {
  final case class MissingValue(name: String) extends ConfiguredError
  final case class InvalidValue(name: String, value: String) extends ConfiguredError

  implicit val show: Show[ConfiguredError] =
    Show.show {
      case MissingValue(name) => s"Missing value: $name"
      case InvalidValue(name, value) => s"Invalid value for $name: $value"
    }
}

////

trait Environment {
  def get(key: String): Option[String]
}

object Environment {

  import scala.collection.JavaConverters._

  def fromEnvVars[F[_] : Sync]: F[Environment] =
    Sync[F].delay(System.getenv.asScala.toMap)
      .map(
        envvars =>
          new Environment {
            override def get(key: String): Option[String] =
              envvars.get(key)
          }
      )

  def fromMap[F[_] : Sync](map: Map[String, String]): F[Environment] =
    Sync[F].delay(
      new Environment {
        override def get(key: String): Option[String] =
          map.get(key)
      }
    )

  val printer: String => Unit = (s: String) => println(s"         $s")
  val silencer: String => Unit = (_: String) => ()

  def logging[F[_] : Sync](inner: Environment, log: String => Unit = silencer): F[Environment] =
    Sync[F].delay(
      new Environment {
        override def get(key: String): Option[String] = {
          val value = inner.get(key)
          value.fold(log(s"Not configured: $key"))(v => log(s"export $key=$v"))
          value
        }
      }
    )
}

////

trait Configured[F[_], A] {
  self =>

  def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]]

  ////

  def withSuffix(suffix: String): Configured[F, A] =
    new Configured[F, A] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
        self.value(name = s"${name}_$suffix")
    }

  def andThen[B](f: A => ValidatedNec[ConfiguredError, B])(implicit F: Functor[F]): Configured[F, B] =
    new Configured[F, B] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, B]] =
        self.value(name)
          .mapF(_.map(_.andThen(f)))
    }

  def or[B](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, Either[A, B]] =
    new Configured[F, Either[A, B]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Either[A, B]]] =
        self.value(s"${name}_C1")
          .flatMap {
            _.fold(
              errors1 =>
                cb.value(s"${name}_C2")
                  .map {
                    _.fold(
                      errors2 => (errors1 ++ errors2).invalid[Either[A, B]],
                      b => b.asRight[A].valid
                    )
                  },
              a => Kleisli(_ => a.asLeft[B].validNec[ConfiguredError].pure[F])
            )
          }
    }

}

object Configured {
  def apply[F[_], A](implicit F: Configured[F, A]): Configured[F, A] = F

  def apply[F[_], A](name: String)(
    implicit F: Configured[F, A]
  ): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
    F.value(name)

  ////

  implicit def configuredA[F[_] : Applicative, A: Conversion]: Configured[F, A] =
    new Configured[F, A] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
        Kleisli {
          env =>
            env.get(name)
              .map {
                s =>
                  Conversion[A].of(s)
                    .fold(
                      error => ConfiguredError.InvalidValue(name, error).invalidNec[A],
                      _.validNec[ConfiguredError]
                    )
              }.getOrElse(ConfiguredError.MissingValue(name).invalidNec[A])
              .pure[F]
        }
    }

  implicit def configuredOption[F[_], A](implicit F: Functor[F], A: Configured[F, A]): Configured[F, Option[A]] =
    new Configured[F, Option[A]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Option[A]]] =
        Configured[F, A](s"${name}_OPT")
          .map {
            _.fold(
              c => if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec else c.invalid,
              a => a.some.valid
            )
          }
    }

  implicit def configuredList[F[_], A](implicit F: Monad[F], A: Configured[F, A]): Configured[F, List[A]] =
    new Configured[F, List[A]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, List[A]]] =
        Configured[F, Int](s"${name}_COUNT")
          .flatMap {
            _.fold(
              c => Kleisli(_ => c.invalid[List[A]].pure[F]),
              n => {
                List.tabulate(n)(identity)
                  .traverse {
                    i =>
                      Configured[F, A](s"${name}_$i")
                  }.map {
                  list: List[ValidatedNec[ConfiguredError, A]] =>
                    list.sequence
                }
              }
            )
          }
    }

  implicit def configuredEither[F[_], A, B](
    implicit F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    A or B

  ////

  implicit def applicativeConfigured[F[_]](implicit F: Applicative[F]): Applicative[Configured[F, ?]] =
    new Applicative[Configured[F, ?]] {
      override def pure[A](a: A): Configured[F, A] =
        new Configured[F, A] {
          override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
            Kleisli(_ => a.validNec[ConfiguredError].pure[F])
        }

      override def ap[A, B](ff: Configured[F, A => B])(ca: Configured[F, A]): Configured[F, B] =
        new Configured[F, B] {
          override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, B]] =
            Kleisli {
              env =>
                (
                  ca.value(name).run(env),
                  ff.value(name).run(env)
                ).tupled
                  .map {
                    _.mapN {
                      (a, a2b) =>
                        a2b(a)
                    }
                  }
            }
        }
    }

}
