/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch

import zio._

case class DevConfig(
    devESSourcePath:     os.Path,
    devRestAPIPath:      os.Path,
    specAPIPath:         os.Path,
    devScalaAPIDestPath: os.Path,
)

object DevConfig {

  def fromEnvironment: Task[DevConfig] =
    for {
      esSource <- zio.System.env("ELASTICSEARCH_PATH")
      apiSpec <- zio.System.env("ELASTICSEARCH_API_SPEC_PATH")
      restAPIPathSource <- zio.System.env("ELASTICSEARCH_REST_API_PATH")
      scalaAPIDest <- zio.System.env("ELASTICSEARCH_SCALA_API_PATH")
    } yield DevConfig(
      devESSourcePath =
        esSource.map(v => os.Path(v)).getOrElse(os.home / "projects" / "github" / "elasticsearch" / "elasticsearch"),
      devRestAPIPath = restAPIPathSource.map(v => os.Path(v)).getOrElse(os.pwd / "elasticsearch" / "rest-api-spec"),
      specAPIPath =
        apiSpec.map(v => os.Path(v)).getOrElse(os.pwd / os.up / "elasticsearch" / "elasticsearch-specification"),
      devScalaAPIDestPath = scalaAPIDest.map(v => os.Path(v)).getOrElse(os.pwd / "zio-elasticsearch"),
//      scalaAPIDest.map(v => os.Path(v)).getOrElse(os.home / "projects" / "zio-elasticsearch"),
    )

}
