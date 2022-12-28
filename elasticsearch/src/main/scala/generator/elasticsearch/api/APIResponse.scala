/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import zio.json.ast.Json
import zio.json.JsoniterScalaCodec._

final case class APIResponse(
    params:  Map[String, Parameter] = Map.empty[String, Parameter],
    samples: List[Json] = Nil,
) {}

object APIResponse {
  implicit val codec: JsonValueCodec[APIResponse] = JsonCodecMaker.make[APIResponse]
}
