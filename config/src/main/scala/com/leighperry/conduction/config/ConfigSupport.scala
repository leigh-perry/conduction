package com.leighperry.conduction.config

import cats.data.{Reader, ValidatedNec}
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{Applicative, Apply, Show}


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

trait Configured[A] {
  def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, A]]
}

object Configured {
  def apply[A](implicit F: Configured[A]): Configured[A] = F

  implicit val `Configured for Int`: Configured[Int] =
    new Configured[Int] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, Int]] =
        Reader {
          case (env, name) =>
            eval[Int](env, name, _.toInt)
        }
    }

  implicit val `Configured for Long`: Configured[Long] =
    new Configured[Long] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, Long]] =
        Reader {
          case (env, name) =>
            eval[Long](env, name, _.toLong)
        }
    }

  implicit val `Configured for Double`: Configured[Double] =
    new Configured[Double] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, Double]] =
        Reader {
          case (env, name) =>
            eval[Double](env, name, _.toDouble)
        }
    }

  implicit val `Configured for String`: Configured[String] =
    new Configured[String] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, String]] =
        Reader {
          case (env, name) =>
            eval[String](env, name, identity)
        }
    }

  implicit def `Configured for Option`[A: Configured]: Configured[Option[A]] =
    new Configured[Option[A]] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, Option[A]]] =
        Reader {
          case (env, name) =>
            Configured[A]
              .value
              .run(env -> s"${name}_OPT")
              .fold(
                c => if (c.forall(_.isInstanceOf[ConfiguredError.MissingValue])) None.validNec else c.invalid,
                a => a.some.valid
              )
        }
    }

  implicit def `Configured for List`[A: Configured]: Configured[List[A]] =
    new Configured[List[A]] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, List[A]]] =
        Reader {
          case (env, name) =>
            Configured[Int]
              .value
              .run(env -> s"${name}_COUNT")
              .fold(
                c => c.invalid,
                n => {
                  // 0..n toList
                  val list: List[ValidatedNec[ConfiguredError, A]] =
                    List.tabulate(n)(identity)
                      .traverse {
                        i =>
                          Configured[A]
                            .value
                            .run(env -> s"${name}_$i")
                      }
                  list.sequence // inline gives compilation error :-(
                }
              )
        }
    }

  implicit def `Configured for Either`[A: Configured, B: Configured]: Configured[Either[A, B]] =
    new Configured[Either[A, B]] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, Either[A, B]]] =
        Reader {
          case (env, name) =>
            Configured[A]
              .value
              .run(env -> s"${name}_C1")
              .fold(
                valid1 =>
                  Configured[B]
                    .value
                    .run(env -> s"${name}_C2")
                    .fold(
                      valid2 => (valid1 ++ valid2).invalid[Either[A, B]],
                      b => b.asRight[A].valid
                    ),
                a => a.asLeft[B].valid
              )
        }
    }

  implicit val `Applicative for Configured`: Applicative[Configured] =
    new Applicative[Configured] {
      override def pure[A](a: A): Configured[A] =
        new Configured[A] {
          override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, A]] =
            Reader(_ => a.validNec)
        }

      override def ap[A, B](ff: Configured[A => B])(fa: Configured[A]): Configured[B] =
        new Configured[B] {
          override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, B]] =
            Reader {
              // TODO run = smell
              envName =>
                Apply[ValidatedNec[ConfiguredError, ?]]
                  .map2[A, A => B, B](
                  fa.value.run(envName),
                  ff.value.run(envName)
                ) {
                  (a: A, a2b: A => B) =>
                    a2b(a)
                }
            }
        }
    }

  //  implicit val `Monad for Configured`: Monad[Configured] =
  //    new Monad[Configured] {
  //      override def pure[A](a: A): Configured[A] =
  //        new Configured[A] {
  //          override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, A]] =
  //            Reader(_ => a.validNec)
  //        }
  //
  //      override def flatMap[A, B](fa: Configured[A])(f: A => Configured[B]): Configured[B] =
  //        new Configured[B] {
  //          override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, B]] =
  //            Reader {
  //              case (env, name) =>
  //                fa.value.run(env -> name)
  //                .map {
  //                  a =>
  //                    f(a).value.run
  //                }
  //
  //
  //            }
  //        }
  //      override def tailRecM[A, B](a: A)(f: A => Configured[Either[A, B]]): Configured[B] =
  //        ???
  //    }

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

final class ConfiguredOps[A](val c: Configured[A]) extends AnyVal {
  def suffixed(suffix: String): Configured[A] =
    new Configured[A] {
      override def value: Reader[(Environment, String), ValidatedNec[ConfiguredError, A]] =
        Reader {
          // TODO combinator for this? not dimap...
          case (env, name) =>
            c.value
              .run(env -> s"${name}_$suffix")
        }
    }
}

trait ToConfiguredOps {
  implicit def `Ops for Configured`[A](e: Configured[A]): ConfiguredOps[A] =
    new ConfiguredOps[A](e)
}

object configuredinstances
  extends ToConfiguredOps
