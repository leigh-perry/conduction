package com.leighperry.conduction.config.hocon

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
import com.leighperry.conduction.config.Environment
import com.typesafe.config.{
  ConfigException,
  ConfigFactory,
  ConfigParseOptions,
  ConfigResolveOptions
}

object HoconEnvironment {

  def fromHoconFile[F[_]: Sync](configFilepath: String): F[Environment[F]] =
    Sync[F].delay {
      ConfigFactory.load(
        configFilepath,
        ConfigParseOptions.defaults().setAllowMissing(false),
        ConfigResolveOptions.defaults
      )
    }.map {
      ts =>
        new Environment[F] {
          override def get(key: String): F[Option[String]] =
            Sync[F].delay {
              Either.catchOnly[ConfigException.Missing](ts.getString(key)).toOption
            }
        }
    }
}
