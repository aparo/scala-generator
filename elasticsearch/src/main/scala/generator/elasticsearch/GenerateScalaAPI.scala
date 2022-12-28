/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch

import generator.elasticsearch.api.ElasticSearchScalaCodeGenerator
import zio._

object GenerateScalaAPI extends ZIOAppDefault {
  def run = myAppLogic

  val myAppLogic =
    for {
      config <- DevConfig.fromEnvironment
      generator = new ElasticSearchScalaCodeGenerator(config)
      _ <- ZIO.attempt(generator.run())
    } yield ()

}
