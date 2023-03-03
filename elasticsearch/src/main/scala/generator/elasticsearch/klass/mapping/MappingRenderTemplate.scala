/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass.mapping
import generator.elasticsearch.klass.{BaseRenderTemplate, ClassDef}
import generator.elasticsearch.{APIUtils, DevConfig}

import scala.collection.mutable

class MappingRenderTemplate(val devConfig: DevConfig, val pathScope: String) extends BaseRenderTemplate {
  def generateScala(classes: List[ClassDef]): Unit = {
    val destQueriesDir = devConfig.devScalaAPIDestPath / pathScope
    if (!os.exists(destQueriesDir)) os.makeDir(destQueriesDir)

    val qClasses = classes.map(c => MappingClassDef(c))
    val body     = new mutable.ListBuffer[String]

    var includes = Set(
      "import com.github.plokhotnyuk.jsoniter_scala.core._",
      "import com.github.plokhotnyuk.jsoniter_scala.macros._",
    )

    val queryTemplate = elasticsearch.mapping.txt.Mapping
    qClasses.foreach { obj =>
      val rd = queryTemplate.render(obj).toString()
      println(rd)
      body += rd
      includes ++= obj.scalaExtraImports

    }
    APIUtils.saveFile(destQueriesDir / "mappings.scala", (includes.toList ++ body).mkString("\n"))
  }

}
