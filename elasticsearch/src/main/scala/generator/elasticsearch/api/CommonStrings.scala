/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

object CommonStrings {
  val defaultExtractor = """\(\s*default:\s*(.*?)\)""".r

  def isMultiple(description: String) = description.contains("comma-separated")

  def extractDefault(mtype: String, description: String): Option[Any] =
    defaultExtractor.findFirstIn(description) match {
      case None => None
      case Some(value) =>
        val result = value.split(":").last.trim.stripSuffix(")")
        if (result.contains(" "))
          return None
        mtype match {
          case "boolean" => Some(result == "true")
          case _         => Some(result)
        }
    }

}
