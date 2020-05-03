package com.leighperry.conduction.config.testsupport

import cats.effect.IO
import com.leighperry.conduction.config.Environment
import com.leighperry.conduction.config.Environment.{ fromArgs, fromMap, fromPropertiesFile, logging, silencer }
import org.scalacheck.Gen

object EnvGenerators {

  def envIO(params: Map[String, String], log: String => IO[Unit] = silencer[IO]): IO[Environment[IO]] =
    IO(logging[IO](fromMap[IO](params), log))

  def argsIO(params: Map[String, String], log: String => IO[Unit] = silencer[IO]): IO[Environment[IO]] =
    IO(logging[IO](fromArgs[IO](params.map { case (k, v) => s"--$k=$v" }.toList), log))

  def propertyFileIO(propertiesFilename: String, log: String => IO[Unit] = silencer[IO]): IO[Environment[IO]] =
    for {
      fp <- IO(getClass.getClassLoader.getResource(s"$propertiesFilename.properties"))
      ep <- fromPropertiesFile[IO](fp.getFile)
      q <- IO(logging[IO](ep, log))
    } yield q

  // Test all supported environments for each test
  def genEnvIO(
    params: Map[String, String],
    propertiesFilename: String,
    log: String => IO[Unit] = silencer[IO]
  ): Gen[IO[Environment[IO]]] =
    Gen.oneOf(
      Gen.const(envIO(params, log)),
      Gen.const(argsIO(params, log)),
      Gen.const(propertyFileIO(propertiesFilename, log))
    )

}
