/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

import generator.elasticsearch.DevConfig

trait BaseGeneratorTrait {
  def devConfig: DevConfig

  def classes: List[ClassDef]

  def pathScope: String

  def renderTemplate: BaseRenderTemplate

  def generateScala(): Unit =
    renderTemplate.generateScala(classes)

}
