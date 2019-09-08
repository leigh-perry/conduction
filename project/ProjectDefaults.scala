import Dependencies._
import sbt.Keys._
import sbt.{Resolver, _}

object ProjectDefaults {
  private val scalacOptionsWarnings =
    Set(
      "-Xlint",
      "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
      //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xfuture", // Turn on future language features.
      "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
      "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
      "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
      "-Xlint:option-implicit", // Option.apply used implicit view.
      "-Xlint:package-object-classes", // Class or object defined in package object.
      "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match", // Pattern match may not be typesafe.
      "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification", // Enable partial unification in type constructor inference
      "-Ywarn-dead-code", // Warn when dead code is identified.
      "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen", // Warn when numerics are widened.
      "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals", // Warn if a local definition is unused.
      "-Ywarn-unused:params", // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates", // Warn if a private member is unused.
      "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
    )

  val settings =
    Seq(
      scalaVersion := "2.12.9",

      scalacOptions ++=
        Seq(
          "-deprecation",
          "-encoding", "UTF-8",
          "-unchecked",
          "-Xfuture",
          "-explaintypes", // Explain type errors in more detail.
          "-feature", // Emit warning and location for usages of features that should be imported explicitly.
          "-target:jvm-1.8",
          "-language:_",
          "-Ybackend-parallelism", java.lang.Runtime.getRuntime.availableProcessors.toString,
        ) ++ scalacOptionsWarnings,

      fork in Test := true,
      fork in run := true,

      testFrameworks += new TestFramework("minitest.runner.Framework"),

      javaOptions ++=
        Seq(
          "-Xms1G",
          "-Xmx1G",
          "-XX:MaxMetaspaceSize=512M",
          "-XX:+HeapDumpOnOutOfMemoryError",
          "-XX:HeapDumpPath=./heap-dump.hprof",
          "-XX:-IgnoreUnrecognizedVMOptions",
          "-XX:+UnlockExperimentalVMOptions",
          "-XX:+EnableJVMCI",
          "-XX:+UseJVMCICompiler",
        ),

      // Disable warnings in console
      scalacOptions in(Compile, console) ~= {
        _ filterNot scalacOptionsWarnings.apply
      },
      scalacOptions in(Test, console) ~= {
        _ filterNot scalacOptionsWarnings.apply
      },

      resolvers ++=
        Seq(
          //Resolver.mavenLocal,
          Resolver.typesafeRepo("releases")/*,
          Resolver.sonatypeRepo("public"), "Confluent Maven Repo" at "http://packages.confluent.io/maven/"*/
        ),

      //Stops the auto creation of java / scala-2.11 directories
      unmanagedSourceDirectories in Compile ~= {
        _.filter(_.exists)
      },
      unmanagedSourceDirectories in Test ~= {
        _.filter(_.exists)
      },

      // All subprojects to use
      libraryDependencies ++=
        Seq(
          cats,
          catsEffect
        ),

      addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
    )

}
