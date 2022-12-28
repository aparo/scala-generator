/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch

import zio._

case class DevConfig(devESSourcePath: os.Path, devRestAPIPath: os.Path, devScalaAPIDestPath: os.Path)

object DevConfig {

  def fromEnvironment: Task[DevConfig] =
    for {
      esSource <- zio.System.env("ELASTICSEARCH_PATH")
      restAPIPathSource <- zio.System.env("ELASTICSEARCH_REST_API_PATH")
      scalaAPIDest <- zio.System.env("ELASTICSEARCH_SCALA_API_PATH")
    } yield DevConfig(
      esSource.map(v => os.Path(v)).getOrElse(os.home / "sources" / "elasticsearch" / "elasticsearch"),
      restAPIPathSource.map(v => os.Path(v)).getOrElse(os.pwd / "elasticsearch" / "rest-api-spec"),
      scalaAPIDest.map(v => os.Path(v)).getOrElse(os.home / "projects" / "zio-elasticsearch"),
    )

}

// class DevConfig extends ConfigHelper {
//   def rootConfig: Config = ConfigFactory.load()

//   lazy val devESSourcePath =
//     get[String]("dev.elasticsearch-path") || (File.home / "sources" / "elasticsearch" / "elasticsearch")
//       .toString()
//   lazy val devRestAPIPath      = get[String]("dev.rest-api-path") || "/opt/meglio/tools/generator/rest-api-spec"
//   lazy val devScalaAPIDestPath = get[String]("dev.scala-api-path") || "/opt/meglio/tools/generator/tmp"
// //  lazy val devRestAPIPath      = get[String]("dev.rest-api-path") || (File.home / "Projects" / "code-generator" / "rest-api-spec").toString()
// //  lazy val devScalaAPIDestPath = get[String]("dev.scala-api-path") || (File.home / "Projects" / "code-generator" / "tmp").toString()

// }
