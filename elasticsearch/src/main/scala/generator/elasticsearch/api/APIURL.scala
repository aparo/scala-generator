/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

final case class Deprecated(version: String, description: String)
object Deprecated {
  implicit val codec: JsonValueCodec[Deprecated] = JsonCodecMaker.make[Deprecated]
}

case class APIPath(
    path:       String,
    methods:    List[String] = Nil,
    parts:      Map[String, PartDescription] = Map.empty[String, PartDescription],
    params:     Map[String, Parameter] = Map.empty[String, Parameter],
    deprecated: Option[Deprecated] = None,
) {
  def getPartDocumentation(name: String): String = {
    parts
      .filter { case (name2, part) =>
        name == name2
      }
      .foreach(p => return p._2.description)
    ""
  }

  def isDeprecated: Boolean = deprecated.isDefined
}

object APIPath {
  implicit val codec: JsonValueCodec[APIPath] = JsonCodecMaker.make[APIPath]
}

case class APIURL(paths: List[APIPath] = Nil) {
  def params: Map[String, Parameter] =
    paths.filterNot(_.isDeprecated).flatMap(_.params.toList).toMap

  def validPaths: List[APIPath] = paths.filterNot(_.isDeprecated)

  def getPartDocumentation(name: String): String =
    paths
      .filterNot(_.isDeprecated)
      .flatMap(_.parts.toList)
      .toMap
      .filter { case (name2, part) =>
        name == name2
      }
      .map(p => p._2.description)
      .headOption
      .getOrElse("")

}

object APIURL {
  implicit val codec: JsonValueCodec[APIURL] = JsonCodecMaker.make[APIURL]
}
