/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved
 */

package generator.elasticsearch.klass.query

import generator.elasticsearch.klass.{BaseRenderTemplate, ClassDef}
import generator.elasticsearch.{APIUtils, DevConfig}

import scala.collection.mutable.ArrayBuffer

class QueryRenderTemplate(val devConfig: DevConfig, val pathScope: String) extends BaseRenderTemplate {
  def generateScala(klasses: List[ClassDef]): Unit = {
    val destQueriesDir = devConfig.devScalaAPIDestPath / pathScope
    if (!os.exists(destQueriesDir)) os.makeDir(destQueriesDir)

    val queryTemplate = elasticsearch.query.txt.Query
    val classes       = klasses.map(c => QueryClassDef(c))
    classes
      // .filter(_.name=="bool")
      .foreach { obj =>
        APIUtils.saveFile(destQueriesDir / s"${obj.className.get}.scala", queryTemplate.render(obj).toString())
      //        println(queryTemplate.render(obj))
      }
    // generating queries converter
    scalaLookUp("Queries", "Query", classes)

  }

  private def scalaLookUp(name: String, returnName: String, values: List[QueryClassDef]): Unit = {
    val destSearchDir = devConfig.devScalaAPIDestPath / "search"
    if (!os.exists(destSearchDir)) os.makeDir(destSearchDir)

    val lines = new ArrayBuffer[String]
    lines += s"""/* Copyright 2018-2023 Alberto Paro on Apache 2 License. All Rights Reserved */
                |package zio.elasticsearch.$pathScope
                |
                |import io.circe.Json
                |
                |object $name {
                |  def fromJson(json: Json):$returnName =""".stripMargin
    lines += "     json.as[JsonObject].foreach{"
    lines += "      case (name,jsValue) =>"
    lines += "        name match {"
    lines ++= values
      .sortBy(_.name)
      .map(q => s"""          case "${q.name}" => return ${q.className.get}.fromJson(jsValue)""")

    lines += "        }"
    lines += "     }"
    lines += "  }"

    APIUtils.saveFile(destSearchDir / s"$name.scala", lines.mkString("\n"))
  }
}
