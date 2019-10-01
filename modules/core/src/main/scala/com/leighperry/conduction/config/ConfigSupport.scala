package com.leighperry.conduction.config

import cats.data.{ Kleisli, ValidatedNec }
import cats.effect.Sync
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{ Applicative, Functor, Monad, Show }

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
    new Conversion[Int] {
      override def of(s: String): Either[String, Int] =
        eval(s, _.toInt)
    }

  implicit val conversionLong: Conversion[Long] =
    new Conversion[Long] {
      override def of(s: String): Either[String, Long] =
        eval(s, _.toLong)
    }

  implicit val conversionDouble: Conversion[Double] =
    new Conversion[Double] {
      override def of(s: String): Either[String, Double] =
        eval(s, _.toDouble)
    }

  implicit val conversionBoolean: Conversion[Boolean] =
    new Conversion[Boolean] {
      override def of(s: String): Either[String, Boolean] =
        eval(s, _.toBoolean)
    }

  implicit val conversionString: Conversion[String] =
    new Conversion[String] {
      override def of(s: String): Either[String, String] =
        s.asRight
    }

  implicit val functorConversion: Functor[Conversion] =
    new Functor[Conversion] {
      override def map[A, B](fa: Conversion[A])(f: A => B): Conversion[B] =
        new Conversion[B] {
          override def of(s: String): Either[String, B] =
            fa.of(s).map(f)
        }
    }

  private def eval[A](s: String, f: String => A): Either[String, A] =
    Either
      .catchNonFatal(f(s))
      .leftMap(_ => s)
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

trait Environment[F[_]] {
  def get(key: String): F[Option[String]]
}

object Environment {

  def fromEnvVars[F[_]: Sync]: F[Environment[F]] =
    Sync[F]
      .delay(sys.env)
      .map(m => fromMap(m))

  def fromMap[F[_]: Applicative](map: Map[String, String]): Environment[F] =
    new Environment[F] {
      override def get(key: String): F[Option[String]] =
        map.get(key).pure[F]
    }

  def printer[F[_]: Applicative]: String => F[Unit] =
    (s: String) => println(s"         $s").pure[F]

  def silencer[F[_]: Applicative]: String => F[Unit] =
    (_: String) => ().pure[F]

  def logging[F[_]: Monad](inner: Environment[F], log: String => F[Unit]): Environment[F] =
    new Environment[F] {
      override def get(key: String): F[Option[String]] =
        for {
          value <- inner.get(key)
          _ <- value.fold(log(s"Not configured: $key"))(v => log(s"export $key=$v"))
        } yield value
    }
}

////

trait Configured[F[_], A] {
  self =>

  def value(name: String): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]]

  ////

  def withSuffix(suffix: String): Configured[F, A] =
    new Configured[F, A] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        self.value(name = s"${name}_$suffix")
    }

  /**
   * Support sequential use of `Configured` (monadic-style), which is otherwise applicative.
   * Not called `flatMap` to eschew the use in for-comprehensions.
   */
  def andThen[B](
    f: A => ValidatedNec[ConfiguredError, B]
  )(implicit F: Functor[F]): Configured[F, B] =
    new Configured[F, B] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, B]] =
        self
          .value(name)
          .mapF(_.map(_.andThen(f)))
    }

  def or[B](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, Either[A, B]] =
    new Configured[F, Either[A, B]] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Either[A, B]]] =
        self.value(s"${name}_C1").flatMap {
          _.fold(
            errors1 =>
              cb.value(s"${name}_C2").map {
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
  ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
    F.value(name)

  ////

  implicit def configuredA[F[_]: Monad, A: Conversion]: Configured[F, A] =
    new Configured[F, A] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        Kleisli {
          env =>
            for {
              value <- env.get(name)
              c <- value
                .map(
                  s => {
                    Conversion[A]
                      .of(s)
                      .fold(
                        error => ConfiguredError.InvalidValue(name, error).invalidNec[A],
                        _.validNec[ConfiguredError]
                      )
                  }
                )
                .getOrElse(ConfiguredError.MissingValue(name).invalidNec[A])
                .pure[F]
            } yield c
        }
    }

  implicit def configuredOption[F[_], A](
    implicit F: Functor[F],
    A: Configured[F, A]
  ): Configured[F, Option[A]] =
    new Configured[F, Option[A]] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Option[A]]] =
        Configured[F, A](s"${name}_OPT").map {
          _.fold(
            c =>
              if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec
              else c.invalid,
            a => a.some.valid
          )
        }
    }

  implicit def configuredList[F[_], A](
    implicit F: Monad[F],
    A: Configured[F, A]
  ): Configured[F, List[A]] =
    new Configured[F, List[A]] {
      override def value(
        name: String
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, List[A]]] =
        Configured[F, Int](s"${name}_COUNT").flatMap {
          _.fold(
            c => Kleisli(_ => c.invalid[List[A]].pure[F]),
            n => {
              List
                .tabulate(n)(identity)
                .traverse {
                  i =>
                    Configured[F, A](s"${name}_$i")
                }
                .map {
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

  implicit def applicativeConfigured[F[_]](
    implicit F: Applicative[F]
  ): Applicative[Configured[F, *]] =
    new Applicative[Configured[F, *]] {
      override def pure[A](a: A): Configured[F, A] =
        new Configured[F, A] {
          override def value(
            name: String
          ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
            Kleisli(_ => a.validNec[ConfiguredError].pure[F])
        }

      override def ap[A, B](ff: Configured[F, A => B])(ca: Configured[F, A]): Configured[F, B] =
        new Configured[F, B] {
          override def value(
            name: String
          ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, B]] =
            Kleisli {
              env =>
                (ca.value(name).run(env), ff.value(name).run(env))
                  .tupled
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
