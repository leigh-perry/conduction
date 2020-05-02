package com.leighperry.conduction.config

import cats.data.{ Kleisli, ValidatedNec }
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{ Applicative, Functor, Monad }
import com.leighperry.conduction.config.Environment.Key

final case class ConfigValueInfo(name: String, valueType: String)

final case class ConfigDescription(values: List[ConfigValueInfo]) extends AnyVal {
  def prettyPrint(separator: String): String =
    values
      .map(v => s"${v.name}: ${v.valueType}")
      .mkString(separator)
}

object ConfigDescription {
  def apply(values: ConfigValueInfo*): ConfigDescription =
    ConfigDescription(values.toList)
}

////

trait Configured[F[_], A] {
  self =>

  def value(
    name: Environment.Key
  ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]]

  def description(name: Environment.Key): ConfigDescription

  ////

  def withSuffix(suffix: String): Configured[F, A] =
    new Configured[F, A] {
      override def value(
        name: Environment.Key
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        self.value(name = suffixed(name))

      override def description(name: Environment.Key): ConfigDescription =
        self.description(name = suffixed(name))

      private def suffixed(name: Key) =
        s"${name}_$suffix"
    }

  def andThen[B](
    f: A => ValidatedNec[ConfiguredError, B]
  )(
    implicit F: Functor[F],
    B: Configured[F, B]
  ): Configured[F, B] =
    new Configured[F, B] {
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, B]] =
        self
          .value(name)
          .mapF(_.map(_.andThen(f)))

      override def description(name: Key): ConfigDescription =
        ConfigDescription(self.description(name).values ++ B.description(name).values)
    }

  def or[B](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, Either[A, B]] =
    new Configured[F, Either[A, B]] {
      override def value(
        name: Environment.Key
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Either[A, B]]] =
        self.value(leftName(name)).flatMap {
          _.fold(
            errors1 =>
              cb.value(rightName(name)).map {
                _.fold(
                  errors2 => (errors1 ++ errors2).invalid[Either[A, B]],
                  b => b.asRight[A].valid
                )
              },
            a => Kleisli(_ => a.asLeft[B].validNec[ConfiguredError].pure[F])
          )
        }

      override def description(name: Key): ConfigDescription =
        ConfigDescription(self.description(leftName(name)).values ++ cb.description(rightName(name)).values)

      private def leftName(name: Key) =
        s"${name}_C1"

      private def rightName(name: Key) =
        s"${name}_C2"
    }
}

// TODO support sealed traits

object Configured {
  def apply[F[_], A](implicit F: Configured[F, A]): Configured[F, A] =
    F

  def apply[F[_], A](
    name: String
  )(implicit F: Configured[F, A]): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
    F.value(name)

  ////

  implicit def applicativeConfigured[F[_]](
    implicit F: Applicative[F]
  ): Applicative[Configured[F, *]] =
    new Applicative[Configured[F, *]] {
      override def pure[A](a: A): Configured[F, A] =
        new Configured[F, A] {
          override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
            Kleisli(_ => a.validNec[ConfiguredError].pure[F])

          override def description(name: Key): ConfigDescription =
            ConfigDescription()
        }

      override def ap[A, B](ff: Configured[F, A => B])(ca: Configured[F, A]): Configured[F, B] =
        new Configured[F, B] {
          override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, B]] =
            Kleisli {
              env =>
                (ca.value(name).run(env), ff.value(name).run(env))
                  .tupled
                  .map {
                    _.mapN {
                      (a, a2b) => a2b(a)
                    }
                  }
            }

          override def description(name: Key): ConfigDescription =
            ConfigDescription(ff.description(name).values ++ ca.description(name).values)
        }
    }

  implicit def configuredA[F[_]: Monad, A: Conversion]: Configured[F, A] =
    new Configured[F, A] {
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        Kleisli {
          env =>
            for {
              value <- env.get(name)
              c <- value.map {
                s =>
                  Conversion[A]
                    .of(s)
                    .fold(
                      error => ConfiguredError.invalidValue(name, error).invalidNec[A],
                      _.validNec[ConfiguredError]
                    )
              }.getOrElse(ConfiguredError.missingValue(name).invalidNec[A])
                .pure[F]
            } yield c
        }

      override def description(name: Key): ConfigDescription =
        ConfigDescription(ConfigValueInfo(name, Conversion[A].description))
    }

  implicit def configuredOption[F[_], A](
    implicit F: Functor[F],
    A: Configured[F, A]
  ): Configured[F, Option[A]] =
    new Configured[F, Option[A]] {
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Option[A]]] =
        Configured[F, A](optionName(name)).map {
          _.fold(
            c =>
              if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec
              else c.invalid,
            a => a.some.valid
          )
        }

      override def description(name: Key): ConfigDescription =
        A.description(name = optionName(name))

      private def optionName(name: Key) =
        s"${name}_OPT"
    }

  implicit def configuredList[F[_], A](
    implicit F: Monad[F],
    A: Configured[F, A]
  ): Configured[F, List[A]] =
    new Configured[F, List[A]] {
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, List[A]]] =
        Configured[F, Int](countName(name)).flatMap {
          _.fold(
            c => Kleisli(_ => c.invalid[List[A]].pure[F]),
            n =>
              (0 until n)
                .toList
                .traverse(i => Configured[F, A](indexName(name, i.toString)))
                .map {
                  list: List[ValidatedNec[ConfiguredError, A]] => list.sequence
                }
          )
        }

      override def description(name: Key): ConfigDescription =
        ConfigDescription(
          ConfigValueInfo(countName(name), Conversion[Int].description) ::
            A.description(name = indexName(name, "n")).values
        )

      private def countName(name: Key) =
        name + "_COUNT"

      private def indexName(name: Key, i: String) =
        s"${name}_$i"
    }

  implicit def configuredEither[F[_], A, B](
    implicit F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    A or B

  ////

  implicit class SealedTraitOps[F[_], ST, A <: ST](ca: Configured[F, A]) {
    def orAdt[B <: ST](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, ST] =
      new Configured[F, ST] {
        override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, ST]] =
          ca.value(name).flatMap {
            _.fold(
              errors1 =>
                cb.value(name).map {
                  _.fold(
                    errors2 => (errors1 ++ errors2).invalid[ST],
                    b => b.valid
                  )
                },
              a => Kleisli(_ => (a: ST).validNec[ConfiguredError].pure[F])
            )
          }

        override def description(name: Key): ConfigDescription =
          ConfigDescription(ca.description(name).values ++ cb.description(name).values)
      }

    def |[B <: ST](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, ST] =
      orAdt(cb)
  }

}
