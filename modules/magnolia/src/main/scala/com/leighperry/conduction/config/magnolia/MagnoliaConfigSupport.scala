package com.leighperry.conduction.config.magnolia

import cats.data.{Kleisli, ValidatedNec}
import cats.effect.IO
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Functor, Monad}
import com.leighperry.conduction.config.Environment.Key
import com.leighperry.conduction.config.{ConfigDescription, ConfigValueInfo, Configured, ConfiguredError, Conversion, Environment}
import magnolia.{CaseClass, Magnolia, Param, SealedTrait}

import scala.language.experimental.macros

/**
 * Implicit resolution for cats.effect.IO.
 * You can add others for other IO implementations of Sync, eg Monix Task or ZIO.
 */
object AutoConfigInstancesIO extends AutoConfigInstances[IO]

/** Implicit resolution for any F[_] with Applicative */
abstract class AutoConfigInstances[F[_]: Applicative] extends MagnoliaConfigSupport[F] {

  // Explicitly re-expose the companion object implicits at higher priority
  implicit def configuredA[F[_]: Monad, A: Conversion]: Configured[F, A] =
    Configured.configuredA

  implicit def configuredOption[F[_], A](implicit F: Functor[F], A: Configured[F, A]): Configured[F, Option[A]] =
    Configured.configuredOption

  implicit def configuredList[F[_], A](implicit F: Monad[F], A: Configured[F, A]): Configured[F, List[A]] =
    Configured.configuredList

  implicit def configuredEither[F[_], A, B](
    implicit F: Monad[F],
    A: Configured[F, A],
    B: Configured[F, B]
  ): Configured[F, Either[A, B]] =
    Configured.configuredEither

}

////

/** Magnolia support for product types */
private[magnolia] abstract class MagnoliaConfigSupport[F[_]: Applicative] {

  type Typeclass[T] = Configured[F, T]

  /**
   * How new [[Configured]]s for case classes should be constructed
   *
   * final case class Endpoint(host: String, port: Int)
   * object Endpoint {
   *   implicit def configuredInstance[F[_]](implicit F: Monad[F]): Configured[F, Endpoint] =
   *     (
   *       Configured[F, String].withSuffix("HOST"),
   *       Configured[F, Int].withSuffix("PORT")
   *     ).mapN(Endpoint.apply)
   * }
   */
  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    caseClass
      .parameters
      .toList
      .traverse {
        p: Param[Typeclass, T] =>
          p.typeclass
            .withSuffix(p.label.toUpperCase)
            .asInstanceOf[Configured[F, Any]] // TODO methodCase to uppercaseSnake
      }
      .map(list => caseClass.rawConstruct(list))

  /** How to choose which subtype of the sealed trait to use for decoding */
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    new Configured[F, T] {
      override def value(name: Key): Kleisli[F, Environment[F], ValidatedNec[ConfiguredError, T]] =
        sys.error("Sum types are not supported")  // TODO support sealed traits

      override def description(name: Key): ConfigDescription =
        sys.error("Sum types are not supported")
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
