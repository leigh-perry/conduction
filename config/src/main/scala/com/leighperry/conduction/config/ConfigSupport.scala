package com.leighperry.conduction.config

import cats.data.{Kleisli, ValidatedNec}
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Applicative, Functor, Monad, Show}


/**
  * Support for reading detailed, nested configuration from environment variables etc
  */
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
  def fromEnvVars: Environment =
    new Environment {

      import scala.collection.JavaConverters._

      val envvars: Map[String, String] = System.getenv.asScala.toMap

      override def get(key: String): Option[String] =
        envvars.get(key)
    }

  def fromMap(map: Map[String, String]): Environment =
    (key: String) => map.get(key)

  val printer: String => Unit = (s: String) => println(s"         $s")
  val silencer: String => Unit = (_: String) => ()

  def logging(inner: Environment, log: String => Unit = silencer): Environment =
    (key: String) => {
      val value = inner.get(key)
      value.fold(log(s"Not configured: $key"))(v => log(s"export $key=$v"))
      value
    }
}

////

trait Configured[F[_], A] {
  self =>

  def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]]

  def withSuffix(suffix: String): Configured[F, A] =
    new Configured[F, A] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
        Kleisli {
          env =>
            self.value(name = s"${name}_$suffix")
              .run(env)
        }
    }

  def andThen[B](f: A => ValidatedNec[ConfiguredError, B])(implicit F: Functor[F]): Configured[F, B] =
    new Configured[F, B] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, B]] =
        Kleisli {
          env =>
            self.value(name)
              .run(env)
              .map(_.andThen(f))
        }
    }

}

object Configured {
  def apply[F[_], A](implicit F: Configured[F, A]): Configured[F, A] = F

  def apply[F[_], A](name: String)(
    implicit F: Configured[F, A]
  ): Kleisli[F, Environment, ValidatedNec[ConfiguredError, A]] =
    F.value(name)

  implicit def configuredInt[F[_] : Applicative]: Configured[F, Int] =
    new Configured[F, Int] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Int]] =
        Kleisli {
          env =>
            eval[Int](env, name, _.toInt)
              .pure[F]
        }
    }

  implicit def configuredLong[F[_] : Applicative]: Configured[F, Long] =
    new Configured[F, Long] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Long]] =
        Kleisli {
          env =>
            eval[Long](env, name, _.toLong)
              .pure[F]
        }
    }

  implicit def configuredDouble[F[_] : Applicative]: Configured[F, Double] =
    new Configured[F, Double] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Double]] =
        Kleisli {
          env =>
            eval[Double](env, name, _.toDouble)
              .pure[F]
        }
    }

  implicit def configuredString[F[_] : Applicative]: Configured[F, String] =
    new Configured[F, String] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, String]] =
        Kleisli {
          env =>
            eval[String](env, name, identity)
              .pure[F]
        }
    }

  implicit def configuredOption[F[_], A](
    implicit F: Monad[F],
    A: Configured[F, A]
  ): Configured[F, Option[A]] =
    new Configured[F, Option[A]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Option[A]]] =
        Kleisli {
          env =>
            Configured[F, A](s"${name}_OPT")
              .run(env)
              .map {
                _.fold(
                  c => if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec else c.invalid,
                  a => a.some.valid
                )
              }
        }
    }

  implicit def configuredList[F[_], A](
    implicit F: Monad[F],
    A: Configured[F, A]
  ): Configured[F, List[A]] =
    new Configured[F, List[A]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, List[A]]] =
        Kleisli {
          env =>
            Configured[F, Int](s"${name}_COUNT")
              .run(env)
              .flatMap {
                _.fold(
                  c => c.invalid[List[A]].pure[F],
                  n => {
                    List.tabulate(n)(identity)
                      .traverse {
                        i =>
                          Configured[F, A](s"${name}_$i")
                            .run(env)
                      }.map {
                      list: List[ValidatedNec[ConfiguredError, A]] =>
                        list.sequence
                    }
                  }
                )
              }
        }
    }

  implicit def configuredEither[F[_], A, B](
    implicit F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    new Configured[F, Either[A, B]] {
      override def value(name: String): Kleisli[F, Environment, ValidatedNec[ConfiguredError, Either[A, B]]] =
        Kleisli {
          env =>
            Configured[F, A](s"${name}_C1")
              .run(env)
              .flatMap {
                _.fold(
                  errors1 =>
                    Configured[F, B](s"${name}_C2")
                      .run(env)
                      .map {
                        _.fold(
                          errors2 => (errors1 ++ errors2).invalid[Either[A, B]],
                          b => b.asRight[A].valid
                        )
                      },
                  a => a.asLeft[B].validNec[ConfiguredError].pure[F]
                )
              }
        }
    }

  implicit def applicativeConfigured[F[_], AA](implicit F: Monad[F]): Applicative[Configured[F, ?]] =
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

  // TODO test laws

  private def eval[A](env: Environment, name: String, f: String => A): ValidatedNec[ConfiguredError, A] =
    env.get(name)
      .map {
        s =>
          Either.catchNonFatal(f(s))
            .fold(
              _ => ConfiguredError.InvalidValue(name, s).invalidNec[A],
              _.validNec[ConfiguredError]
            )
      }.getOrElse(ConfiguredError.MissingValue(name).invalidNec[A])

}
