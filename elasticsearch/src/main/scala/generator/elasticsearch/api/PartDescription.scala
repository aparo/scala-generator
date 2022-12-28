/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class PartDescription(`type`: String, description: String, required: Boolean = false)

object PartDescription {
  implicit val codec: JsonValueCodec[PartDescription] = JsonCodecMaker.make[PartDescription]
}

case class APIResult(scala: String)

object APIResult {
  implicit val codec: JsonValueCodec[APIResult] = JsonCodecMaker.make[APIResult]
}
