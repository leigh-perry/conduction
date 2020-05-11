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
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{ Applicative, Functor, Monad, Monoid }
import com.leighperry.conduction.config.Environment.Key

final case class ConfigValueInfo(name: String, valueType: String)

final case class ConfigDescription(values: List[ConfigValueInfo]) extends AnyVal {
  def sorted: ConfigDescription =
    ConfigDescription(values.sortBy(_.name))

  def prettyPrint(separator: String): String =
    values
      .map(v => s"${v.name}: ${v.valueType}")
      .mkString(separator)
}

object ConfigDescription {
  def apply(values: ConfigValueInfo*): ConfigDescription =
    ConfigDescription(values.toList)

  implicit val instanceMonoid: Monoid[ConfigDescription] =
    new Monoid[ConfigDescription] {
      override def empty: ConfigDescription =
        ConfigDescription(Nil)
      override def combine(x: ConfigDescription, y: ConfigDescription): ConfigDescription =
        ConfigDescription(x.values |+| y.values)
    }
}

////

trait Configured[F[_], A] {
  self =>

  def value(
    name: Environment.Key
  ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]]

  def description(name: Environment.Key): ConfigDescription

  ////

  def withSuffix(suffix: String): Configured[F, A] = {
    def suffixed(name: Key) = s"${name}_$suffix"

    Configured.create(
      name => self.value(name = suffixed(name)),
      name => self.description(name = suffixed(name))
    )
  }

  def andThen[B](
    f: A => ValidatedNec[ConfiguredError, B]
  )(implicit
    F: Functor[F],
    B: Configured[F, B]
  ): Configured[F, B] =
    Configured.create(
      name => self.value(name).mapF(_.map(_.andThen(f))),
      name => self.description(name) |+| B.description(name)
    )

  def or[B](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, Either[A, B]] = {
    def leftName(name: Key) = s"${name}_C1"
    def rightName(name: Key) = s"${name}_C2"

    // TODO applicative product / andThen
    Configured.create(
      name =>
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
        },
      name => self.description(leftName(name)) |+| cb.description(rightName(name))
    )
  }
}

object Configured {
  def apply[F[_], A](implicit F: Configured[F, A]): Configured[F, A] =
    F

  def apply[F[_], A](
    name: String
  )(implicit F: Configured[F, A]): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
    F.value(name)

  ////

  implicit def applicativeConfigured[F[_]](implicit
    F: Applicative[F]
  ): Applicative[Configured[F, *]] =
    new Applicative[Configured[F, *]] {
      override def pure[A](a: A): Configured[F, A] =
        create(
          name => Kleisli(_ => a.validNec[ConfiguredError].pure[F]),
          name => ConfigDescription()
        )

      override def ap[A, B](ff: Configured[F, A => B])(ca: Configured[F, A]): Configured[F, B] =
        create(
          name =>
            Kleisli {
              env =>
                (ca.value(name).run(env), ff.value(name).run(env))
                  .tupled
                  .map {
                    _.mapN {
                      (a, a2b) => a2b(a)
                    }
                  }
            },
          name => ff.description(name) |+| ca.description(name)
        )
    }

  implicit def configuredA[F[_]: Monad, A: Conversion]: Configured[F, A] =
    create(
      name =>
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
        },
      name => ConfigDescription(ConfigValueInfo(name, Conversion[A].description))
    )

  implicit def configuredOption[F[_], A](implicit
    F: Functor[F],
    A: Configured[F, A]
  ): Configured[F, Option[A]] = {
    def optionName(name: Key) = s"${name}_OPT"

    create(
      name =>
        Configured[F, A](optionName(name)).map {
          _.fold(
            c =>
              if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec
              else c.invalid,
            a => a.some.valid
          )
        },
      name => A.description(name = optionName(name))
    )
  }

  implicit def configuredList[F[_], A](implicit
    F: Monad[F],
    A: Configured[F, A]
  ): Configured[F, List[A]] = {
    def countName(name: Key) = name + "_COUNT"
    def indexName(name: Key, i: String) = s"${name}_$i"

    create(
      name =>
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
        },
      name =>
        ConfigDescription(
          ConfigValueInfo(countName(name), Conversion[Int].description) ::
            A.description(name = indexName(name, "n")).values
        )
    )
  }

  implicit def configuredEither[F[_], A, B](implicit
    F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    A or B

  ////

  implicit class SealedTraitOps[F[_]: Monad, ST, A <: ST](ca: Configured[F, A]) {
    def orAdt[B <: ST](cb: Configured[F, B]): Configured[F, ST] =
      create(
        name =>
        // TODO applicative product
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
          },
        name => ca.description(name) |+| cb.description(name)
      )

    def |[B <: ST](cb: Configured[F, B]): Configured[F, ST] =
      orAdt(cb)
  }

  ////

  def create[F[_], A](
    fValue: Key => Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]],
    fDescription: Key => ConfigDescription
  ): Configured[F, A] =
    new Configured[F, A] {
      override def value(name: Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        fValue(name)
      override def description(name: Key): ConfigDescription =
        fDescription(name)
    }

  def const[F[_]: Applicative, A](a: A): Configured[F, A] =
    create(
      _ => Kleisli(_ => a.validNec.pure[F]),
      _ => Monoid[ConfigDescription].empty
    )

  def fail[F[_]: Applicative, A](err: ConfiguredError): Configured[F, A] =
    create(
      _ => Kleisli(_ => err.invalidNec.pure[F]),
      _ => Monoid[ConfigDescription].empty
    )

}
