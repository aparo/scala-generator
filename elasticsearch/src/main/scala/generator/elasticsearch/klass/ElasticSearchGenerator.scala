/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

import generator.elasticsearch.DevConfig

import scala.collection.mutable

class ElasticSearchGenerator(val devConfig: DevConfig, val pathScope: String, val renderTemplate: BaseRenderTemplate)
    extends BaseGeneratorTrait {

  val root    = devConfig.devRestAPIPath
  val destDir = devConfig.devScalaAPIDestPath
  os.makeDir.all(destDir)

  lazy val classes: List[ClassDef] = {
    // loading queries
    val results = new mutable.ListBuffer[ClassDef]
    os.walk(root / pathScope, skip = _.last.endsWith(".json")).foreach { filename =>
      println(s"Loading: $filename")
      results += ClassDef.load(filename)
    }
    if (results.exists(_.name.startsWith("_"))) {
      val common = results.find(_.name == "_common").get
      results -= common
      results.map(q => q.copy(members = q.members ++ common.members)).sortBy(_.name).toList
    } else results.sortBy(_.name).toList

  }

}
