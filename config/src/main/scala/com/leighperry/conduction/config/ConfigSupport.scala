package com.leighperry.conduction.config

import cats.data.ValidatedNec
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._


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
  def vars: Environment =
    new Environment {

      import scala.collection.JavaConverters._

      val envvars: Map[String, String] = System.getenv.asScala.toMap

      override def get(key: String): Option[String] =
        envvars.get(key)
    }

  def withMap(map: Map[String, String]): Environment =
    (key: String) => map.get(key)

  def withDebug(inner: Environment, log: String => Unit = println): Environment =
    (key: String) => {
      val value = inner.get(key)
      log(s"> get $key => $value")
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
