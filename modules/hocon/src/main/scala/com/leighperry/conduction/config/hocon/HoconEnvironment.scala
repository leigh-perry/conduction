package com.leighperry.conduction.config.hocon

import java.io.File

import cats.effect.Sync
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import com.leighperry.conduction.config.Environment
import com.typesafe.config.{ConfigException, ConfigFactory}

object HoconEnvironment {

  def fromHoconFile[F[_]: Sync](configFilepath: String): F[Environment[F]] =
    Sync[F].delay {
      ConfigFactory.parseFile(new File(configFilepath))
    }.map {
      ts =>
        new Environment[F] {
          override def get(key: Environment.Key): F[Option[String]] =
            Sync[F].delay {
              val keyString = key.intercalate(".")
              Either
                .catchOnly[ConfigException.Missing](ts.getString(keyString)) // throws on missing
                .fold(_ => None, Option(_)) // handle null string returned
            }
        }
    }
}
