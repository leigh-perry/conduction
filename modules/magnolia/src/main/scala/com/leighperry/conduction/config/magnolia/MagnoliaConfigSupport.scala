package com.leighperry.conduction.config.magnolia

import cats.data.NonEmptyList
import cats.effect.IO
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ Functor, Monad }
import com.leighperry.conduction.config.{ Configured, Conversion }
import magnolia.{ CaseClass, Magnolia, Param, SealedTrait, Subtype }

import scala.language.experimental.macros

/**
 * Implicit resolution for cats.effect.IO.
 * You can add others for other IO implementations of Sync, eg Monix Task or ZIO.
 */
object AutoConfigInstancesIO extends AutoConfigInstances[IO]

/** Implicit resolution for any F[_] with Applicative */
abstract class AutoConfigInstances[F[_]: Monad] extends MagnoliaConfigSupport[F] {

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

private[magnolia] abstract class MagnoliaConfigSupport[F[_]: Monad] {

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
            .withSuffix(Content.classToSnakeUpperCase(p.label))
            .asInstanceOf[Configured[F, Any]]
      }
      .map(list => caseClass.rawConstruct(list))

  /** How to choose which subtype of the sealed trait to use for decoding */
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    NonEmptyList
      .fromListUnsafe(sealedTrait.subtypes.toList)
      .map(asTypeclass)
      .reduceLeft(_ | _)

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  private def asTypeclass[T](subtype: Subtype[Typeclass, T]): Typeclass[T] =
    subtype
      .typeclass
      .asInstanceOf[Configured[F, T]]
      .withSuffix(Content.classToSnakeUpperCase(subtype.typeName.short))

  private def parse(value: String): String = {
    println(value)
    value
  }
}

object Content {
  def classToSnakeUpperCase(name: String): String =
    name
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toUpperCase()
}
