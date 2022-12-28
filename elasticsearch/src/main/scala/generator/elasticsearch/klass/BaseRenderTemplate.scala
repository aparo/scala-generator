/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

/** Created by alberto on 10/04/2017.
  */
trait BaseRenderTemplate {
  def generateScala(classes: List[ClassDef]): Unit
}
