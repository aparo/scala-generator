/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.elasticsearch.Constants
import generator.ts.{CodeData, ParserContext, ScalaClassMember, ScalaObjectType, SimpleType, UndefinedType, UnionType}
import generator.ts.Converters._
case class APIDocumetation(url: String, description: String)
object APIDocumetation {
  implicit val codec: JsonValueCodec[APIDocumetation] = JsonCodecMaker.make[APIDocumetation]
}

case class APIEntry(
    name:           String = "undefined",
    documentation:  APIDocumetation,
    url:            APIURL,
    body:           Option[APIBody] = None,
    result:         Option[APIResult] = None,
    params:         Map[String, Parameter] = Map.empty[String, Parameter],
    nativeAction:   Option[String] = None,
    nativeRequest:  Option[String] = None,
    nativeResponse: Option[String] = None,
    response:       Option[APIResponse] = None,
    stability:      Option[String] = None,
) {
  // from indices.get_field_mapping -> GetFieldMapping
  lazy val className: String = {
    val n = name.split('.').drop(1).flatMap(_.split('_')).map(_.capitalize).mkString
    APIEntry.mappingNames.getOrElse(n, n)
  }

  lazy val scalaRequest:  String = className + "Request"
  lazy val scalaResponse: String = className + "Response"

  def hasBody: Boolean = this.body.isDefined

  def requestPackage: String = s"$basePackage.requests".stripSuffix(".")

  def responsePackage: String = s"$basePackage.responses".stripSuffix(".")

  def basePackage: String = s"${Constants.namespaceName}.${scope.replace("client", "")}".stripSuffix(".")


  lazy val scope: String = {
    val tokens = name.split("\\.")
    var scope  = "client"
    if (tokens.length > 1)
      scope = tokens(tokens.length - 2)
    scope
  }

  lazy val camelName: String = {
    val name = this.name.replace("_", ".").split("\\.").map(p => p.capitalize).mkString("")
    APIEntry.mappingNames.getOrElse(name, name)
  }

}

object APIEntry {
  implicit val codec: JsonValueCodec[APIEntry] =
    JsonCodecMaker.make[APIEntry](CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforceCamelCase))
  implicit val codecMap: JsonValueCodec[Map[String, APIEntry]] =
    JsonCodecMaker.make[Map[String, APIEntry]](CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforceCamelCase))

  val mappingNames = Map(
    "Mget" -> "MultiGet",
    "Mlt" -> "MoreLikeThis",
    "Mpercolate" -> "MultiPercolate",
    "Msearch" -> "MultiSearch",
    "Mtermvectors" -> "MultiTermVectors",
  )

}
