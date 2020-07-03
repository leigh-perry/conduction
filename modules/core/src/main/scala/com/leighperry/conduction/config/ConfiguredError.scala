package com.leighperry.conduction.config

import cats.Show

trait ConfiguredError

object ConfiguredError {
  final case class MissingValue(name: Environment.Key) extends ConfiguredError
  final case class InvalidValue(name: Environment.Key, value: String) extends ConfiguredError
  final case class UnrecognisedAdtValue(name: Environment.Key) extends ConfiguredError

  def missingValue(name: String): ConfiguredError =
    MissingValue(name)

  def invalidValue(name: String, value: String): ConfiguredError =
    InvalidValue(name, value)

  implicit val show: Show[ConfiguredError] =
    Show.show {
      case MissingValue(name) => s"Missing value: $name"
      case InvalidValue(name, value) => s"Invalid value for $name: $value"
    }
}
