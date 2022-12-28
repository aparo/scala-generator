/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import zio.json.JsoniterScalaCodec._
import zio.json.ast.Json

case class Member(
    name:         String,
    var `type`:   String,
    var required: Boolean = false,
    var multiple: Boolean = false,
    skip_default: Option[Boolean] = Some(false),
    codeName:     Option[String] = None,
    subType:      Option[String] = None,
    description:  String = "",
    options:      Option[List[Json]] = None,
    inField:      Option[Boolean] = None,
    var default:  Option[Json] = None,
) extends ScalaMemberTrait {

  if (name.startsWith("_")) required = false
  if (name == "boost") {
    `type` = "double"
    default = Some(Json.Num(1.0))
  }
}
object Member {
  implicit val codec: JsonValueCodec[Member] = JsonCodecMaker.make[Member]
}
