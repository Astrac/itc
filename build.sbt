lazy val commonSettings = List(
  organization := "astrac",
  licenses += ("Apache-2.0", url(
    "https://opensource.org/licenses/Apache-2.0",
  )),
  scalaVersion := "3.2.2",
  resolvers ++= Resolver.sonatypeOssRepos("releases") ++ Seq(
    Resolver.bintrayRepo("astrac", "maven"),
  ),
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    // "-language:higherKinds", // Allow higher-kinded types
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Werror", // Fail the compilation if there are any warnings.
    "-Wunused:all",
    "-Ykind-projector",
  ),
)

lazy val catsEffectVersion = "3.4.10"
lazy val catsVersion = "2.9.0"
lazy val drosteVersion = "0.9.0"
lazy val fs2Version = "3.6.1"
lazy val literallyVersion = "1.1.0"
lazy val munitVersion = "0.7.29"
lazy val parboiledVersion = "2.4.1"
lazy val scalacheckVersion = "1.17.0"

lazy val `itc-core` = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/core"))
  .settings(
    name := "itc-core",
    description := "An Interval Tree Clock implementation",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.parboiled" %%% "parboiled" % parboiledVersion,
      "org.typelevel" %%% "literally" % literallyVersion,
      "io.higherkindness" %%% "droste-core" % drosteVersion,
      "org.typelevel" %%% "discipline-munit" % "2.0.0-M3" % Test,
      "org.typelevel" %%% "cats-laws" % catsVersion % Test,
      "org.typelevel" %%% "cats-testkit" % catsVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "0.7.29" % Test,
    ),
  )
  .settings(commonSettings)

lazy val `itc-simulator` = crossProject(JSPlatform, JVMPlatform)
  .in(file("modules/simulator"))
  .settings(commonSettings)
  .settings(
    name := "itc-simulator",
    description := "Simulator for a network using the ITC Fork-Event-Join model",
  )
  .dependsOn(`itc-core` % "compile->compile;test->test")
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0")
        .cross(CrossVersion.for3Use2_13),
    ),
  )

lazy val itc = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/_root"))
  .settings(commonSettings)
  .settings(
    name := "itc",
    description := "Interval Tree Clock implementation and utilities",
  )
  .aggregate(`itc-core`, `itc-simulator`)
