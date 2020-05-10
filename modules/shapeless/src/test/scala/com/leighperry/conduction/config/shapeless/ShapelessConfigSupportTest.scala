package com.leighperry.conduction.config.shapeless

import cats.data.{NonEmptyChain, Validated}
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import com.leighperry.conduction.config.Environment.{Key, fromMap, silencer}
import com.leighperry.conduction.config.shapeless.AutoConfigInstancesIO._
import com.leighperry.conduction.config.testsupport.EnvGenerators._
import com.leighperry.conduction.config.testsupport.TestSupport
import com.leighperry.conduction.config.{ConfigDescription, ConfigValueInfo, Configured, ConfiguredError, Environment}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object ShapelessConfigSupportTest extends Properties("Shapeless config support") with TestSupport {

  final case class KV[V](key: String, v: V)

  def genKV[V: Arbitrary] =
    for {
      key <- genSymbol(1, 20)
      v <- genFor[V]
    } yield KV(key, v)

  property("Primitive String values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v))
      Configured[IO, String](k)
        .run(map)
        .map(c => c.shouldBe(v.validNec))
  }

  property("Primitive Int values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v.toString))
      Configured[IO, Int](k)
        .run(map)
        .map(c => c.shouldBe(v.validNec))
  }

  property("Present Option[String] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> v))
      Configured[IO, Option[String]](k)
        .run(map)
        .map(c => c.shouldBe(v.some.validNec))
  }

  property("Present Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> v.toString))
      Configured[IO, Option[Int]](k)
        .run(map)
        .map(c => c.shouldBe(v.some.validNec))
  }

  property("Missing Option[String] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v))
      Configured[IO, Option[String]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Missing Option[Int] values") = forAllIO(genKV[String]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(k -> v.toString))
      Configured[IO, Option[Int]](s"${k}a")
        .run(map)
        .map(c => c.shouldBe(None.validNec))
  }

  property("Misconfigured Option[Int] values") = forAllIO(genKV[Int]) {
    kv =>
      val k = kv.key
      val v = kv.v
      val map = fromMap[IO](Map(s"${k}_OPT" -> s"${v}x"))
      Configured[IO, Option[Int]](k)
        .run(map)
        .map(
          c =>
            c.shouldSatisfy {
              case Validated.Invalid(nec) =>
                nec.shouldBeNec(
                  NonEmptyChain(
                    ConfiguredError.invalidValue(s"${k}_OPT", s"${v}x")
                  )
                )
              case _ => false
            }
        )
  }

  property("Present valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val map = fromMap[IO](Map(k -> v))
    for {
      c <- Configured[IO, Double]("A_DOUBLE").run(map)
    } yield c.shouldBe(1.23.validNec)
  }

  property("Missing valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double]("MISSING")
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.missingValue("MISSING").invalidNec))
  }

  property("Invalid valid Double") = simpleTestIO {
    val k = "A_DOUBLE"
    val v = "1.23xxx"
    val map = fromMap[IO](Map(k -> v))
    Configured[IO, Double](k)
      .run(map)
      .map(c => c.shouldBe(ConfiguredError.invalidValue(k, v).invalidNec))
  }

  ////

  property("Present valid Configured[IO, ShapelessEndpoint]") = forAllIO(
    hgenEnvIO(
      Map(
        "LP1_HOST" -> "lp1-host",
        "LP1_PORT" -> "1"
      ),
      "test01"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, ShapelessEndpoint]("LP1").run(env)
      } yield c.shouldBe(ShapelessEndpoint("lp1-host", 1).validNec)
  }

  property("Present valid Configured[IO, TwoShapelessEndpoints]") = forAllIO(
    hgenEnvIO(
      Map(
        "MULTI_EP1_HOST" -> "multi-ep1-host",
        "MULTI_EP1_PORT" -> "2",
        "MULTI_EP2_HOST" -> "multi-ep2-host",
        "MULTI_EP2_PORT" -> "3"
      ),
      "test02"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, TwoShapelessEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        TwoShapelessEndpoints(ShapelessEndpoint("multi-ep1-host", 2), ShapelessEndpoint("multi-ep2-host", 3)).validNec
      )
  }

  property("Present valid Configured[IO, ThreeShapelessEndpoints]") = forAllIO(
    hgenEnvIO(
      Map(
        "MULTI_EP1_HOST" -> "multi-ep1-host",
        "MULTI_EP1_PORT" -> "2",
        "MULTI_EP2_HOST" -> "multi-ep2-host",
        "MULTI_EP2_PORT" -> "3",
        "MULTI_EP3_HOST" -> "multi-ep3-host",
        "MULTI_EP3_PORT" -> "4"
      ),
      "test03"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, ThreeShapelessEndpoints]("MULTI").run(env)
      } yield c.shouldBe(
        ThreeShapelessEndpoints(
          ShapelessEndpoint("multi-ep1-host", 2),
          ShapelessEndpoint("multi-ep2-host", 3),
          ShapelessEndpoint("multi-ep3-host", 4)
        ).validNec
      )
  }

  def hgenEnvIO(
    params: Map[String, String],
    propertiesFilename: String,
    log: String => IO[Unit] = silencer[IO]
  ): Gen[IO[Environment[IO]]] =
//    Gen.oneOf(
      Gen.const(envIO(params, log))
//      Gen.const(argsIO(params, log)),
//      Gen.const(propertyFileIO(propertiesFilename, log))
//    )

  property("Present valid Configured[IO, Either[ShapelessEndpoint, ShapelessEndpoint]]") = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test04"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[ShapelessEndpoint, ShapelessEndpoint]]("CHOICE").run(env)
      } yield c.shouldBe(ShapelessEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[ShapelessEndpoint, ShapelessEndpoint]] via `or` syntax") = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test05"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, ShapelessEndpoint].or(Configured[IO, ShapelessEndpoint]).value("CHOICE").run(env)
      } yield c.shouldBe(ShapelessEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Missing Configured[IO, Either[ShapelessEndpoint, ShapelessEndpoint]]") = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C1_HOST" -> "choice-c1-host",
        "CHOICE_C1_PORT" -> "5"
      ),
      "test06"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Either[ShapelessEndpoint, ShapelessEndpoint]]("CHOICE").run(env)
      } yield c.shouldBe(ShapelessEndpoint("choice-c1-host", 5).asLeft.valid)
  }

  property("Present valid Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint]], ShapelessEndpoint]") =
    forAllIO(
      hgenEnvIO(
        Map(
          "CHOICE_C1_C1_HOST" -> "choice-c1-c1-host",
          "CHOICE_C1_C1_PORT" -> "7",
          "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
          "CHOICE_C1_C2_PORT" -> "8",
          "CHOICE_C2_HOST" -> "choice2-c2-host",
          "CHOICE_C2_PORT" -> "5"
        ),
        "test07"
      )
    ) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint], ShapelessEndpoint]]("CHOICE").run(env)
        } yield c.shouldBe(ShapelessEndpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
    }

  property(
    "Present valid Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint]], ShapelessEndpoint] left/left via `or` syntax"
  ) = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C1_C1_HOST" -> "choice-c1-c1-host",
        "CHOICE_C1_C1_PORT" -> "7",
        "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
        "CHOICE_C1_C2_PORT" -> "8",
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5"
      ),
      "test08"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint], ShapelessEndpoint]] =
            Configured[IO, ShapelessEndpoint]
              .or[ShapelessEndpoint](Configured[IO, ShapelessEndpoint])
              .or[ShapelessEndpoint](Configured[IO, ShapelessEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(ShapelessEndpoint("choice-c1-c1-host", 7).asLeft.asLeft.validNec)
  }

  property(
    "Present valid Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint]], ShapelessEndpoint] left/right via `or` syntax"
  ) = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C1_C2_HOST" -> "choice-c1-c2-host",
        "CHOICE_C1_C2_PORT" -> "8",
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5",
        "CHOICE_OPT_C2_C1_HOST" -> "choice-opt-c2-c1-host",
        "CHOICE_OPT_C2_C1_PORT" -> "9"
      ),
      "test09"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint], ShapelessEndpoint]] =
            Configured[IO, ShapelessEndpoint].or(Configured[IO, ShapelessEndpoint]).or(Configured[IO, ShapelessEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(ShapelessEndpoint("choice-c1-c2-host", 8).asRight.asLeft.validNec)
  }

  property("Present valid Configured[IO, Either[Either[,]], ShapelessEndpoint] right via `or` syntax") = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_C2_HOST" -> "choice2-c2-host",
        "CHOICE_C2_PORT" -> "5"
      ),
      "test10"
    )
  ) {
    e =>
      for {
        env <- e
        c <- {
          val cfg: Configured[IO, Either[Either[ShapelessEndpoint, ShapelessEndpoint], ShapelessEndpoint]] =
            Configured[IO, ShapelessEndpoint].or(Configured[IO, ShapelessEndpoint]).or(Configured[IO, ShapelessEndpoint])
          cfg
            .value("CHOICE")
            .run(env)
        }
      } yield c.shouldBe(ShapelessEndpoint("choice2-c2-host", 5).asRight.validNec)
  }

  private val separator = "_"

  def keyOf(parts: String*): Key =
    parts.mkString(separator)

  property("Missing Configured[IO, Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]") =
    forAllIO(hgenEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]("CHOICE").run(env)
        } yield c.shouldSatisfy(
          _.fold(
            e =>
              e.shouldBeNec(
                NonEmptyChain(
                  ConfiguredError.missingValue(keyOf("CHOICE", "C1", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C1", "PORT")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C1", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C1", "PORT")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C2", "HOST")),
                  ConfiguredError.missingValue(keyOf("CHOICE", "C2", "C2", "PORT"))
                )
              ),
            _ => false
          )
        )
    }

  property(
    "Present valid Configured[IO, Option[Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]]"
  ) = forAllIO(
    hgenEnvIO(
      Map(
        "CHOICE_OPT_C2_C1_HOST" -> "choice-opt-c2-c1-host",
        "CHOICE_OPT_C2_C1_PORT" -> "9"
      ),
      "test11"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Option[Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]]("CHOICE")
          .run(env)
      } yield c.shouldBe(ShapelessEndpoint("choice-opt-c2-c1-host", 9).asLeft.asRight.some.valid)
  }

  property("Missing Configured[IO, Option[Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]]") =
    forAllIO(hgenEnvIO(Map.empty, "empty")) {
      e =>
        for {
          env <- e
          c <- Configured[IO, Option[Either[ShapelessEndpoint, Either[ShapelessEndpoint, ShapelessEndpoint]]]]("CHOICE")
            .run(env)
        } yield c.shouldBe(None.validNec)
    }

  property("Present valid Configured[IO, List[Int]]") = forAllIO(
    hgenEnvIO(
      Map(
        "INTLIST_COUNT" -> "3",
        "INTLIST_0" -> "1000",
        "INTLIST_1" -> "1001",
        "INTLIST_2" -> "1002"
      ),
      "test12"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(List(1000, 1001, 1002).validNec)
  }

  property("Missing Configured[IO, List[Int]]") = forAllIO(hgenEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[Int]]("INTLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("INTLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[ShapelessEndpoint]]") = forAllIO(
    hgenEnvIO(
      Map(
        "EPLIST_COUNT" -> "2",
        "EPLIST_0_HOST" -> "eplist0-host",
        "EPLIST_0_PORT" -> "2",
        "EPLIST_1_HOST" -> "eplist1-host",
        "EPLIST_1_PORT" -> "3"
      ),
      "test13"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[ShapelessEndpoint]]("EPLIST").run(env)
      } yield c.shouldBe(List(ShapelessEndpoint("eplist0-host", 2), ShapelessEndpoint("eplist1-host", 3)).validNec)
  }

  property("Missing Configured[IO, List[ShapelessEndpoint]]") = forAllIO(hgenEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[ShapelessEndpoint]]("EPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("EPLIST", "COUNT")).invalidNec)
  }

  property("Present valid Configured[IO, List[TwoShapelessEndpoints]]") = forAllIO(
    hgenEnvIO(
      Map(
        "TEPLIST_COUNT" -> "2",
        "TEPLIST_0_EP1_HOST" -> "teplist0-ep1-host",
        "TEPLIST_0_EP1_PORT" -> "7",
        "TEPLIST_0_EP2_HOST" -> "multilist-ep1-host0",
        "TEPLIST_0_EP2_PORT" -> "7",
        "TEPLIST_1_EP1_HOST" -> "teplist1-ep1-host",
        "TEPLIST_1_EP1_PORT" -> "7",
        "TEPLIST_1_EP2_HOST" -> "multilist-ep2-host1",
        "TEPLIST_1_EP2_PORT" -> "7",
        "SOMEINT" -> "567"
      ),
      "test14"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoShapelessEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(
        List(
          TwoShapelessEndpoints(ShapelessEndpoint("teplist0-ep1-host", 7), ShapelessEndpoint("multilist-ep1-host0", 7)),
          TwoShapelessEndpoints(ShapelessEndpoint("teplist1-ep1-host", 7), ShapelessEndpoint("multilist-ep2-host1", 7))
        ).validNec
      )
  }

  property("Missing Configured[IO, List[TwoShapelessEndpoints]]") = forAllIO(hgenEnvIO(Map.empty, "empty")) {
    e =>
      for {
        env <- e
        c <- Configured[IO, List[TwoShapelessEndpoints]]("TEPLIST").run(env)
      } yield c.shouldBe(ConfiguredError.missingValue(keyOf("TEPLIST", "COUNT")).invalidNec)
  }

  property("Configured should handle newtypes") = forAllIO(
    hgenEnvIO(
      Map(
        "SOMEINT" -> "567"
      ),
      "test15"
    )
  ) {
    e =>
      for {
        env <- e
        c <- Configured[IO, Int].map(i => s"int[$i]").value("SOMEINT").run(env)
      } yield c.shouldBe("int[567]".validNec)
  }

//  property("Configured description should handle sealed traits") = simpleTest(
//    Configured[IO, ShapelessSomeAdt]
//      .description("LP1")
//      .shouldBe(
//        ConfigDescription(
//          List(
//            ConfigValueInfo("LP1_SHAPELESS_DUAL_EPS_EP1_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_DUAL_EPS_EP1_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_DUAL_EPS_EP2_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_DUAL_EPS_EP2_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_DUAL_EXTRA", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_SINGLE_EP_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_SINGLE_EP_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_SINGLE_EXTRA", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP1_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP1_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP2_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP2_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP3_HOST", "string"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EPS_EP3_PORT", "integer"),
//            ConfigValueInfo("LP1_SHAPELESS_TRIPLE_EXTRA", "string")
//          )
//        )
//      )
//  )
//
//  property("Configured should handle sealed traits – ShapelessSingle") = forAllIO(
//    hgenEnvIO(
//      Map(
//        "LP1_SHAPELESS_SINGLE_EP_HOST" -> "lp1-host",
//        "LP1_SHAPELESS_SINGLE_EP_PORT" -> "1",
//        "LP1_SHAPELESS_SINGLE_EXTRA" -> "singleextra"
//      ),
//      "test19"
//    )
//  ) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, ShapelessSomeAdt].value("LP1").run(env)
//      } yield c.shouldBe(ShapelessSomeAdt.ShapelessSingle(ShapelessEndpoint("lp1-host", 1), "singleextra").validNec)
//  }
//
//  property("Configured should handle sealed traits – ShapelessDual") = forAllIO(
//    hgenEnvIO(
//      Map(
//        "LP1_SHAPELESS_DUAL_EPS_EP1_HOST" -> "multi-ep1-host",
//        "LP1_SHAPELESS_DUAL_EPS_EP1_PORT" -> "2",
//        "LP1_SHAPELESS_DUAL_EPS_EP2_HOST" -> "multi-ep2-host",
//        "LP1_SHAPELESS_DUAL_EPS_EP2_PORT" -> "3",
//        "LP1_SHAPELESS_DUAL_EXTRA" -> "123"
//      ),
//      "test20"
//    )
//  ) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, ShapelessSomeAdt].value("LP1").run(env)
//      } yield c.shouldBe(
//        ShapelessSomeAdt
//          .ShapelessDual(
//            TwoShapelessEndpoints(ShapelessEndpoint("multi-ep1-host", 2), ShapelessEndpoint("multi-ep2-host", 3)),
//            123
//          )
//          .validNec
//      )
//  }
//
//  property("Configured should handle sealed traits - ShapelessTriple") = forAllIO(
//    hgenEnvIO(
//      Map(
//        "LP1_SHAPELESS_TRIPLE_EPS_EP1_HOST" -> "multi-ep1-host",
//        "LP1_SHAPELESS_TRIPLE_EPS_EP1_PORT" -> "2",
//        "LP1_SHAPELESS_TRIPLE_EPS_EP2_HOST" -> "multi-ep2-host",
//        "LP1_SHAPELESS_TRIPLE_EPS_EP2_PORT" -> "3",
//        "LP1_SHAPELESS_TRIPLE_EPS_EP3_HOST" -> "multi-ep3-host",
//        "LP1_SHAPELESS_TRIPLE_EPS_EP3_PORT" -> "4",
//        "LP1_SHAPELESS_TRIPLE_EXTRA" -> "singleextra"
//      ),
//      "test21"
//    )
//  ) {
//    e =>
//      for {
//        env <- e
//        c <- Configured[IO, ShapelessSomeAdt].value("LP1").run(env)
//      } yield c.shouldBe(
//        ShapelessSomeAdt
//          .ShapelessTriple(
//            ThreeShapelessEndpoints(
//              ShapelessEndpoint("multi-ep1-host", 2),
//              ShapelessEndpoint("multi-ep2-host", 3),
//              ShapelessEndpoint("multi-ep3-host", 4)
//            ),
//            "singleextra"
//          )
//          .validNec
//      )
//  }

  property("classToSnakeUpperCase") = Prop.forAll(
    for {
      w <- Gen.nonEmptyListOf(genSymbol(2, 5).map(s => s"${s.head.toUpper}${s.substring(1).toLowerCase}"))
      c <- Gen.alphaLowerChar
    } yield (
      w.map(_ + c).mkString, // WordxClassx
      w.map(_ + c).map(_.toUpperCase).mkString("_") // WORDX_CLASSX
    )
  ) {
    case (classCase, snakeCaseUpper) =>
      Content.classToSnakeUpperCase(classCase) == snakeCaseUpper
  }

  //

  final case class ShapelessEndpoint(host: String, port: Int)
  final case class TwoShapelessEndpoints(ep1: ShapelessEndpoint, ep2: ShapelessEndpoint)
  final case class ThreeShapelessEndpoints(ep1: ShapelessEndpoint, ep2: ShapelessEndpoint, ep3: ShapelessEndpoint)

  sealed trait ShapelessSomeAdt
  object ShapelessSomeAdt {
    final case class ShapelessSingle(ep: ShapelessEndpoint, extra: String) extends ShapelessSomeAdt
    final case class ShapelessDual(eps: TwoShapelessEndpoints, extra: Int) extends ShapelessSomeAdt
    final case class ShapelessTriple(eps: ThreeShapelessEndpoints, extra: String) extends ShapelessSomeAdt
  }

}
