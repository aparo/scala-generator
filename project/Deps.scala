import sbt.{stringToOrganization, ModuleID}

object Deps {
  val ammoniteOps = "com.lihaoyi" %% "ammonite-ops" % "2.4.1"
  val osLib       = "com.lihaoyi" %% "os-lib" % "0.9.0"
  val guava       = "com.google.guava" % "guava" % "23.0"
  val ts          = "org.scalablytyped.converter" %% "ts" % "1.0.0-beta40"
  val scalatest   = "org.scalatest" %% "scalatest" % "3.2.14"
  val scopt       = "com.github.scopt" %% "scopt" % "4.1.0"

  val javaDiff = "io.github.java-diff-utils" % "java-diff-utils" % "4.12"
  val scalaFmt = "org.scalameta" %% "scalafmt-core" % "3.6.1"

  val jsoniterScala      = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.20.0"
  val jsoniterScalaCirce = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe" % "2.20.0"
  // Use the "provided" scope instead when the "compile-internal" scope is not supported
  val jsoniterScalaMacros =
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.20.0" % "compile-internal"

  val zioStreams        = "dev.zio" %% "zio-streams" % Versions.zio
  val collectionCompact = "org.scala-lang.modules" %% "scala-collection-compat" % "2.9.0"
  val zioJson           = "dev.zio" %% "zio-json" % Versions.zioJson
  val zioTest           = "dev.zio" %% "zio-test" % Versions.zio % "test"
  val zioTestSbt        = "dev.zio" %% "zio-test-sbt" % Versions.zio % "test"
}

object Versions {
  val zio     = "2.0.5"
  val zioJson = "0.4.2"

}
