package com.leighperry.conduction.config.shapeless

import cats.Applicative
import cats.data.{ Kleisli, ValidatedNec }
import cats.effect.IO
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.semigroup._
import com.leighperry.conduction.config.{ Configured, ConfiguredError, Environment }
import shapeless.labelled.{ field, FieldType }
import shapeless.{ ::, HList, HNil, LabelledGeneric, Witness }

import scala.language.experimental.macros

/**
 * Implicit resolution for cats.effect.IO.
 * You can add others for other IO implementations of Sync, eg Monix Task or ZIO.
 */
object AutoConfigInstancesIO extends ShapelessConfigSupport[IO]

private[shapeless] abstract class ShapelessConfigSupport[F[_]: Applicative] {

  implicit val deriveHNil: Configured[F, HNil] =
    Configured.const(HNil)

  implicit def deriveHCons[K <: Symbol, H, T <: HList](implicit
    key: Witness.Aux[K],
    cfgH: Configured[F, H],
    cfgT: Configured[F, T]
  ): Configured[F, FieldType[K, H] :: T] =
    Configured
      .create(
        name => {
          val hk: Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, H]] =
            cfgH
              .withSuffix(Content.classToSnakeUpperCase(key.value.name))
              .value(name)

          val tk: Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, T]] = cfgT.value(name)

          // can't hk |+| tk directly
          (hk, tk).mapN {
            (vn1: ValidatedNec[ConfiguredError, H], vn2: ValidatedNec[ConfiguredError, T]) =>
              (vn1, vn2).mapN(field[K](_) :: _)
          }
        },
        name => cfgH.description(name) |+| cfgT.description(name)
      )

  implicit def deriveGeneric[A, R](implicit
    gen: LabelledGeneric.Aux[A, R],
    cfgR: Configured[F, R]
  ): Configured[F, A] =
    cfgR.map(gen.from)

  // TODO ADTs https://stackoverflow.com/questions/52117213/generic-derivation-for-adts-in-scala-with-a-custom-representation
}

object Content {
  def classToSnakeUpperCase(name: String): String =
    name
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toUpperCase()
}
