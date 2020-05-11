package com.leighperry.conduction.config.shapeless

import cats.{ Functor, Monad }
import cats.data.{ Kleisli, ValidatedNec }
import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.syntax.validated._
import com.leighperry.conduction.config.Configured.create
import com.leighperry.conduction.config.Environment.Key
import com.leighperry.conduction.config.{ ConfigDescription, Configured, ConfiguredError, Conversion, Environment }
import shapeless.labelled.{ field, FieldType }
import shapeless.{ :+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness }

import scala.language.experimental.macros

/**
 * Implicit resolution for cats.effect.IO.
 * You can add others for other IO implementations of Sync, eg Monix Task or ZIO.
 */
object AutoConfigInstancesIO extends AutoConfigInstances[IO]

/** Implicit resolution for any F[_] with Monad */
abstract class AutoConfigInstances[F[_]: Monad] extends ShapelessConfigSupport0[F] {

  // Explicitly re-expose the companion object implicits at higher priority
  implicit def configuredA[F[_]: Monad, A: Conversion]: Configured[F, A] =
    Configured.configuredA

  implicit def configuredOption[F[_], A](implicit F: Functor[F], A: Configured[F, A]): Configured[F, Option[A]] =
    Configured.configuredOption

  implicit def configuredList[F[_], A](implicit F: Monad[F], A: Configured[F, A]): Configured[F, List[A]] =
    Configured.configuredList

  implicit def configuredEither[F[_], A, B](implicit
    F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    Configured.configuredEither

}

/** Product types */
private[shapeless] abstract class ShapelessConfigSupport1[F[_]: Monad] {

  implicit val deriveHNil: Configured[F, HNil] =
    Configured.const(HNil)

  implicit def deriveHCons[K <: Symbol, H, T <: HList](implicit
    key: Witness.Aux[K],
    cfgH: Configured[F, H],
    cfgT: Configured[F, T]
  ): Configured[F, FieldType[K, H] :: T] = {
    val suffix = Content.classToSnakeUpperCase(key.value.name)
    val h = cfgH.withSuffix(suffix)
    val t = cfgT

    Configured
      .create(
        name => {
          val hk: Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, H]] = h.value(name)
          val tk: Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, T]] = t.value(name)

          // can't hk |+| tk directly
          (hk, tk).mapN {
            (vn1: ValidatedNec[ConfiguredError, H], vn2: ValidatedNec[ConfiguredError, T]) =>
              (vn1, vn2).mapN(field[K](_) :: _)
          }
        },
        name => h.description(name) |+| t.description(name)
      )
  }

  implicit def deriveGeneric[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    cfgR: Configured[F, R]
  ): Configured[F, A] =
    cfgR.map(gen.from)

}

/** Coproduct types */
private[shapeless] abstract class ShapelessConfigSupport0[F[_]: Monad] extends ShapelessConfigSupport1[F] {

  // ADTs https://stackoverflow.com/questions/52117213/generic-derivation-for-adts-in-scala-with-a-custom-representation

  trait ReprConfigured[C <: Coproduct] extends Configured[F, C]

  def wrap[A <: Coproduct](cfg: Configured[F, A]): ReprConfigured[A] =
    new ReprConfigured[A] {
      override def value(name: Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, A]] =
        cfg.value(name)
      override def description(name: Key): ConfigDescription =
        cfg.description(name)
    }

  implicit val deriveCNil: ReprConfigured[CNil] =
    wrap(Configured.fail(ConfiguredError.UnrecognisedAdtValue("CNil")))

  implicit def deriveCCons[K <: Symbol, L, R <: Coproduct](implicit
    key: Witness.Aux[K],
    cfgL: Configured[F, L],
    cfgR: ReprConfigured[R]
  ): ReprConfigured[FieldType[K, L] :+: R] =
    wrap {
      val suffix = Content.classToSnakeUpperCase(key.value.name)
      val l: Configured[F, L] = cfgL.withSuffix(suffix)
      //val r: ReprConfigured[R] = wrap(cfgR.withSuffix(suffix))
      val r: ReprConfigured[R] = cfgR

      create(
        name =>
          l.value(name).flatMap {
            _.fold(
              errors1 =>
                r.value(name).map {
                  _.fold(
                    errors2 => (errors1 ++ errors2).invalid[FieldType[K, L] :+: R],
                    b => Inr(b).valid
                  )
                },
              a =>
                Kleisli(
                  (_: Environment[F]) => (Inl(field[K](a)): FieldType[K, L] :+: R).validNec[ConfiguredError].pure[F]
                )
            )
          },
        name => l.description(name) |+| r.description(name)
      )
    }

  //implicit def deriveAdt[A, Repr <: Coproduct](implicit
  //  gen: LabelledGeneric.Aux[A, Repr],
  //  deriveRepr: Lazy[ReprConfigured[Repr]]
  //): Configured[F, A] =
  //  (deriveRepr.value: Configured[F, Repr])
  //    .map(gen.from)

}

////

object Content {
  def classToSnakeUpperCase(name: String): String =
    name
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toUpperCase()
}
