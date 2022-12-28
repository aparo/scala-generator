/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass

import generator.elasticsearch.APIUtils
import zio.json.ast.Json

trait BaseMemberTrait {
  def name: String

  def `type`: String

  def required: Boolean

  def multiple: Boolean

  def codeName: Option[String]

  def subType: Option[String]

  def description: String

  def options: Option[List[Json]]

  def inField: Option[Boolean]

  def default: Option[Json]

  def isBaseType: Boolean = APIUtils.isBaseType(`type`)

  def isInField: Boolean = inField.getOrElse(false)

}
