/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch

import generator.elasticsearch.klass.ElasticSearchGenerator
import generator.elasticsearch.klass.mapping.MappingRenderTemplate
import generator.elasticsearch.klass.query.QueryRenderTemplate
import zio._

case class GenerateAPIOption(languages: List[String] = Nil)
object GenerateAPI extends ZIOAppDefault {
  def run = myAppLogic

  val myAppLogic =
    for {
      config <- DevConfig.fromEnvironment
      generator = new ElasticSearchGenerator(
        config,
        pathScope = "mappings",
        renderTemplate = new MappingRenderTemplate(config, "mappings"),
      )
      _ <- ZIO.attempt(generator.generateScala())
    } yield ()

}
