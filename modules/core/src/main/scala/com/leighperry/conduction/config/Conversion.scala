package com.leighperry.conduction.config

import cats.Functor
import cats.syntax.either._

/**
 * Support for reading detailed, nested configuration from environment variables etc
 * Low level conversion from String
 */
trait Conversion[A] {
  def of(s: String): Either[String, A]
}

object Conversion {
  def apply[A](implicit F: Conversion[A]): Conversion[A] = F

  implicit val conversionInt: Conversion[Int] =
    new Conversion[Int] {
      override def of(s: String): Either[String, Int] =
        eval(s, _.toInt)
    }

  implicit val conversionLong: Conversion[Long] =
    new Conversion[Long] {
      override def of(s: String): Either[String, Long] =
        eval(s, _.toLong)
    }

  implicit val conversionDouble: Conversion[Double] =
    new Conversion[Double] {
      override def of(s: String): Either[String, Double] =
        eval(s, _.toDouble)
    }

  implicit val conversionBoolean: Conversion[Boolean] =
    new Conversion[Boolean] {
      override def of(s: String): Either[String, Boolean] =
        eval(s, _.toBoolean)
    }

  implicit val conversionString: Conversion[String] =
    new Conversion[String] {
      override def of(s: String): Either[String, String] =
        s.asRight
    }

  implicit val functorConversion: Functor[Conversion] =
    new Functor[Conversion] {
      override def map[A, B](fa: Conversion[A])(f: A => B): Conversion[B] =
        new Conversion[B] {
          override def of(s: String): Either[String, B] =
            fa.of(s).map(f)
        }
    }

  private def eval[A](s: String, f: String => A): Either[String, A] =
    Either
      .catchNonFatal(f(s))
      .leftMap(_ => s)
}
