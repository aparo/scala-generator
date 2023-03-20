import scala.sys.process.stringToProcess

lazy val latestTag = "git tag -l --sort=committerdate".!!.linesIterator.toVector.last.drop( /* 'v' */ 1)

// BSP setup to use with bloop
Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / bspEnabled := false
autoStartServer := false
Global / excludeLintKeys += autoStartServer

// bloop hasn't upgraded to scala-xml 2 yet
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always,
  "com.lihaoyi" %% "geny" % VersionScheme.Always,
)

lazy val core = project
  .configure(baseSettings, optimize)
  .settings(
    libraryDependencies ++= Seq(
      Deps.javaDiff,
      Deps.ts,
      // Deps.scalaFmt,
      Deps.fastparse,
      Deps.jsoniterScala,
      Deps.jsoniterScalaMacros,
      Deps.osLib,
      Deps.guava,
      Deps.zioStreams,
      Deps.scalatest % Test,
      Deps.zioJson,
      Deps.zioTest,
      Deps.zioTestSbt,
    ),
  )

lazy val elasticsearch = project
  .configure(baseSettings, optimize)
  .enablePlugins(SbtTwirl)
  .settings(
    libraryDependencies ++= Seq(
      Deps.scopt,
      Deps.jsoniterScalaMacros,
      // Deps.ammoniteOps,
      // Deps.ts,
      Deps.scalatest % Test,
      Deps.zioTest,
      Deps.zioTestSbt,
    ),
  )
  .dependsOn(core)

lazy val `zio-elasticsearch` = project
  .configure(baseSettings, optimize)
  .settings(
    libraryDependencies ++= Seq(
      Deps.zioJson,
      Deps.zioJsonExtra,
      Deps.zioJsonException,
      Deps.jsoniterScala,
      Deps.jsoniterScalaMacros,
      Deps.zioStreams,
      Deps.scalatest % Test,
      Deps.zioTest,
      Deps.zioTestSbt,
    ),
  )
  .dependsOn(core)

lazy val generator = project
  .configure(baseSettings, optimize)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "io.megl.generator.internal",
    buildInfoKeys := Seq[BuildInfoKey](
      "gitSha" -> "git rev-parse -1 HEAD".!!.split("\n").last.trim,
      "version" -> version.value,
    ),
    libraryDependencies ++= Seq(
      Deps.scopt,
      // Deps.ammoniteOps,
      Deps.osLib,
//      Deps.ts,
      Deps.scalatest % Test,
      Deps.zioTest,
      Deps.zioTestSbt,
    ),
    Test / fork := true,
    assembly / test := {},
    assembly / mainClass := Some("io.megl.generator.Main"),
    /* meh meh meh */
    assembly / assemblyMergeStrategy := {
      case foo if foo.contains("io/github/soc/directories/")         => MergeStrategy.first
      case foo if foo.contains("reflect.properties")                 => MergeStrategy.first
      case foo if foo.contains("scala-collection-compat.properties") => MergeStrategy.first
      case foo if foo.endsWith("module-info.class")                  => MergeStrategy.discard
      case foo if foo.contains("org/fusesource")                     => MergeStrategy.first
      case foo if foo.contains("META-INF/native/")                   => MergeStrategy.first
      case foo if foo.contains("scala/annotation")                   => MergeStrategy.last
      case foo if foo.contains("META-INF/sisu/javax.inject.Named")   => MergeStrategy.discard
      case other                                                     => (assembly / assemblyMergeStrategy).value(other)
    },
    Test / testOptions += Tests.Argument("-P4"),
  )
  .dependsOn(core, elasticsearch)

lazy val root = project
  .in(file("."))
  .settings(
    name := "generator-root",
    publish / skip := true,
  )
  .configure(baseSettings)
  .aggregate(generator, core, elasticsearch)

lazy val baseSettings: Project => Project =
  _.settings(
    sonatypeCredentialHost := Sonatype.sonatype01,
    sonatypeProfileName := "io.megl",
    organization := "io.megl.generator",
    licenses += ("Apache 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/aparo/scala-generator")),
    developers := List(
      Developer(
        "aparo",
        "Alberto Paro",
        "alberto.paro@gmail.com",
        url("https://github.com/aparo"),
      ),
    ),
    // scalaVersion := "3.2.2",
    scalaVersion := "2.12.10",
    scalacOptions ~= (_.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-unused:params", "-Xfatal-warnings"))),
    /* disable scaladoc */
    Compile / doc / sources := Nil,
  )

lazy val optimize: Project => Project =
  _.settings(
    scalacOptions ++= {
      if (insideCI.value || !isSnapshot.value)
        Seq(
          "-opt:l:inline",
          "-opt:l:method",
          "-opt:simplify-jumps",
          "-opt:compact-locals",
          "-opt:copy-propagation",
          "-opt:redundant-casts",
          "-opt:box-unbox",
          "-opt:nullness-tracking",
          "-opt-inline-from:org.scalablytyped.converter.internal.**",
          "-opt-warnings",
        )
      else Nil
    },
  )
