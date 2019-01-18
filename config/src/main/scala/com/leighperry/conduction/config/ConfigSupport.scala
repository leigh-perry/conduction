package com.leighperry.conduction.config

import cats.Functor
import cats.data.ValidatedNec
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._


/**
  * Support for reading detailed, nested configuration from environment variables etc
  */
trait ConfiguredError

object ConfiguredError {
  final case class MissingValue(name: String) extends ConfiguredError
  final case class InvalidValue(name: String, value: String) extends ConfiguredError
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

trait Configured[A] {
  def value(env: Environment, name: String): ValidatedNec[ConfiguredError, A]
}

object Configured {
  def apply[A](implicit F: Configured[A]): Configured[A] = F

  implicit val `Configured for Int`: Configured[Int] =
    new Configured[Int] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Int] =
        eval[Int](env, name, _.toInt)
    }

  implicit val `Configured for Long`: Configured[Long] =
    new Configured[Long] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Long] =
        eval[Long](env, name, _.toLong)
    }

  implicit val `Configured for Double`: Configured[Double] =
    new Configured[Double] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Double] =
        eval[Double](env, name, _.toDouble)
    }

  implicit val `Configured for String`: Configured[String] =
    new Configured[String] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, String] =
        eval[String](env, name, identity)
    }

  implicit def `Configured for Option`[A: Configured]: Configured[Option[A]] =
    new Configured[Option[A]] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Option[A]] =
        Configured[A]
          .value(env, s"${name}_OPT")
          .fold(
            c => if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec else c.invalid,
            a => a.some.valid
          )
    }

  implicit def `Configured for List`[A: Configured]: Configured[List[A]] =
    new Configured[List[A]] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, List[A]] =
        Configured[Int]
          .value(env, s"${name}_COUNT")
          .fold(
            c => c.invalid,
            n =>
              List.tabulate(n)(identity)
                .traverse(i => Configured[A].value(env, s"${name}_$i"))
          )
    }

  implicit def `Configured for Either`[A: Configured, B: Configured]: Configured[Either[A, B]] =
    new Configured[Either[A, B]] {
      override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, Either[A, B]] =
        Configured[A]
          .value(env, s"${name}_C1")
          .fold(
            c1 =>
              Configured[B]
                .value(env, s"${name}_C2")
                .fold(
                  c2 => (c1 ++ c2).invalid[Either[A, B]],
                  b => b.asRight[A].valid
                ),
            a => a.asLeft[B].valid
          )
    }

  implicit def `Functor for Configured`: Functor[Configured] =
    new Functor[Configured] {
      override def map[A, B](fa: Configured[A])(f: A => B): Configured[B] =
        new Configured[B] {
          override def value(env: Environment, name: String): ValidatedNec[ConfiguredError, B] =
            fa.value(env, name)
              .map(f)
        }

    }

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

////

final class ConfiguredOps[A](val e: Configured[A]) extends AnyVal {
  def valueSuffixed(env: Environment, name: String, suffix: String): ValidatedNec[ConfiguredError, A] =
    e.value(env, s"${name}_$suffix")
}

trait ToConfiguredOps {
  implicit def `Ops for Configured`[A](e: Configured[A]): ConfiguredOps[A] =
    new ConfiguredOps[A](e)
}

object configuredinstances
  extends ToConfiguredOps

////

object ConfigSupport
  extends ToConfiguredOps {

  def wrapped[A: Configured, W](
    env: Environment,
    name: String,
    suffix: String,
    mapper: A => W
  ): ValidatedNec[ConfiguredError, W] =
    Configured[A].valueSuffixed(env, name, suffix).map(mapper)

}
