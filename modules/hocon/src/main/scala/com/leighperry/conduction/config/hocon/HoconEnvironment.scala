package com.leighperry.conduction.config.hocon

import java.io.{ File, PrintWriter, StringWriter }

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
import com.leighperry.conduction.config.Environment
import com.typesafe.config.{ ConfigException, ConfigFactory }

object HoconEnvironment {

  def fromHoconFile[F[_]: Sync](configFilepath: String): F[Environment[F]] =
    Sync[F].delay {
      ConfigFactory.parseFile(new File(configFilepath))
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
