/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.klass.query

import generator.elasticsearch.klass.{BaseClassDefTrait, Member}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait ScalaQueryDefTrait extends BaseClassDefTrait {

  def readJsonVarScala: String =
    if (hasFieldValues) {
      val result = new ListBuffer[String]
      result ++= members.map { member =>
        val res = s"""var ${member.nameAttributeScala}: ${member.typeScala}"""
        member.scalaDefault match {
          case None    => s"$res = " + member.scalaForcedDefault
          case Some(s) => s"$res = $s"
        }
      }
      result += "json.as[JsonObject].fields.foreach {"
      result += " case (jName, jValue) =>"
      result += "   jName match {"

      // we first process the _
      val _Members = members.filterNot(m => m.isInField && m.name != "field")

      def deserialize_JsonScala(members: List[Member]): String =
        members
          .map { m =>
            s""" case "${m.name}" => ${m.nameAttributeScala} = jValue${m.readJsonScala}"""
          }
          .mkString("\n")

      result += deserialize_JsonScala(_Members)

      val inFieldMembers = members.filter(m => m.isInField)

      result += "   case s: String =>\n              field = s"

      result += inFieldMembers
        .map { member =>
          s"""${member.nameAttributeScala} = jValue${member.readJsonScala}"""
        }
        .map(s => "            " + s)
        .mkString("\n")

      // val requiredMembers = members.filter(m => m.required || m.isBaseType ).filterNot(_.default.isDefined).filterNot(_.isInField).filterNot(emitted.contains)

      //            case "boost" => boost = jValue.as[Double]
      //            case "_name" => _name = jValue.asOpt[String]
      //            case s: String =>
      //              field = s
      //              value = jValue
      result += "   }"
      result += "}"
      result.map(s => "        " + s).mkString("\n")
    } else {
      ""
    }
  def readJsonScala: String =
    if (hasFieldValues) {
      members
        .map { member =>
          s"""${member.nameAttributeScala} = ${member.nameAttributeScala}"""
        }
        .map(s => "            " + s)
        .mkString(",\n")
    } else {
      members
        .map { member =>
          s"""${member.nameAttributeScala} = (json \\ "${member.name}")${member.readJsonScala}"""
        }
        .map(s => "            " + s)
        .mkString(",\n")
    }

  // extra case class methods
  def extraCaseClassScala: String =
    name match {
      case "range" =>
        val methods = List(
          "def lt(value: typeName) = this.copy(includeUpper = Some(false), to = Some(jsonConversion))",
          "def lte(value: typeName) = this.copy(includeUpper = Some(true), to = Some(jsonConversion))",
          "def gt(value: typeName) = this.copy(from = Some(jsonConversion), includeLower = Some(false))",
          "def gte(value: typeName) = this.copy(from = Some(jsonConversion), includeLower = Some(true))",
        )
        val result = new ArrayBuffer[String]
        methods.foreach { method =>
          scalaTypeToJson.foreach { case (typeName, jsonConversion) =>
            result += method.replace("typeName", typeName).replace("jsonConversion", jsonConversion)

          }
          result += ""
        }
        result.map(s => "   " + s).mkString("\n")
      case _ => ""
    }

  // extra object methods
  def extraObjectScala: String =
    name match {
      case "term" =>
        val methods = List(s"def apply(field: String, value: typeName) = new ${className.get} (field, jsonConversion)")
        val result  = new ArrayBuffer[String]
        methods.foreach { method =>
          scalaTypeToJson.foreach { case (typeName, jsonConversion) =>
            result += method.replace("typeName", typeName).replace("jsonConversion", jsonConversion)
          }
          result += ""
        }
        result.map(s => "   " + s).mkString("\n")
      case _ => ""
    }

}
