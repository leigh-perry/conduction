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

trait Configured[F[_], A] {
  self =>

  def value(
    name: Environment.Key
  ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]]

  ////

  def withSuffix(suffix: String): Configured[F, A] =
    new Configured[F, A] {
      override def value(
        name: Environment.Key
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
        name: Environment.Key
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, B]] =
        self
          .value(name)
          .mapF(_.map(_.andThen(f)))
    }

  def or[B](cb: Configured[F, B])(implicit F: Monad[F]): Configured[F, Either[A, B]] =
    new Configured[F, Either[A, B]] {
      override def value(
        name: Environment.Key
      ): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Either[A, B]]] =
        self.value(name + "_C1").flatMap {
          _.fold(
            errors1 =>
              cb.value(name + "_C2").map {
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
    }

  implicit def configuredOption[F[_], A](
    implicit F: Functor[F],
    A: Configured[F, A]
  ): Configured[F, Option[A]] =
    new Configured[F, Option[A]] {
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, Option[A]]] =
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
      override def value(name: Environment.Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, List[A]]] =
        Configured[F, Int](name + "_COUNT").flatMap {
          _.fold(
            c => Kleisli(_ => c.invalid[List[A]].pure[F]),
            n =>
              List
                .tabulate(n)(identity)
                .traverse(i => Configured[F, A](s"${name}_$i"))
                .map {
                  list: List[ValidatedNec[ConfiguredError, A]] => list.sequence
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

}
