/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class APIBody(description: String = "", required: Boolean = false, serialize: String = "")
object APIBody {
  implicit val codec: JsonValueCodec[APIBody] = JsonCodecMaker.make
}
