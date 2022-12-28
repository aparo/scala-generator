/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

final case class ClassDef(
    name:          String,
    className:     Option[String] = None,
    documentation: String = "",
    parent:        Option[String] = None,
    var members:   List[Member],
    fieldMode:     Option[Boolean] = Some(false),
) {

  def reorderMembers(): Unit =
    members = members.filter(_.required) ::: members.filterNot(_.required).filterNot(_.name.startsWith("_")) ::: members
      .filterNot(_.required)
      .filter(_.name.startsWith("_"))

}

object ClassDef {
  implicit val codec: JsonValueCodec[ClassDef] = JsonCodecMaker.make[ClassDef]
  def load(filename: os.Path): ClassDef = {
    val data = readFromStream[ClassDef](filename.getInputStream)
    data.reorderMembers()
    data
  }
}
