/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import scala.collection.mutable.ListBuffer
import zio.json.JsoniterScalaCodec._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.ts.ParserContext
import zio.json.ast.Json
import generator.ts.Converters._
sealed trait AbstractParameter {
  def `type`: String

  def description: String

  def options: List[String]

  def default: Option[Any]

  def required: Boolean

  def subtype: Option[String]

  val notMultipleEnums = Set("Bytes", "WaitForStatus")

  def isMultiple: Boolean = CommonStrings.isMultiple(this.description)

  def getType: String = `type` match {
    case s: String if multiple => "Seq[String]"
    case "boolean|long"                                   => "Long"
    case "number|string"                                  => "String"
    case "boolean"                                        => "Boolean"
    case "list"                                           => "Seq[String]"
    case "string" | "text" | "enum" | "time" | "duration" => "String"
    case "int"                                            => "Int"
    case "integer"                                        => "Int"
    case "double"                                         => "Double"
    case "date"                                           => "java.time.LocalDate"
    case "long"                                           => "Long"
    case "number" =>
      subtype match {
        case Some(value) =>
          value match {
            case "long"    => "Long"
            case "int"     => "Integer"
            case "integer" => "Integer"
          }
        case None =>
          "Double"
      }

    case "Json.Obj" => "Json.Obj"
    case "Boolean"|"Map[String,Json]"|"Chunk[String]"           => `type`
    case "String|Chunk[String]|None" => "Chunk[String]"
    case s:String if s.startsWith("Map[String")            => `type`
    case ""           => "String"
  }

  def requiredToString: String = `type` match {
    case s: String if multiple => ""
    case "string" | "text" => ""
    case _                 => ".toString"
  }

  lazy val multiple = CommonStrings.isMultiple(description) || `type` == "list" || subtype.getOrElse("") == "list" ||
    default.getOrElse(None).isInstanceOf[Seq[_]]

  def getParameterName(name: String): String = name match {
    case "type"   => "`type`"
    case "wait"   => "`wait`"
    case "Format" => "OutputFormat"
    case value    => value.toCamel
  }

  def getCookedDefault: String = getType match {
    case "Boolean" | "Integer" | "Numeric" | "Double" | "Long" => default.get.toString
    case value                                                 => "\"" + default.get.toString + "\""
  }

  /* Check both None and null */
  def defaultIsValid: Boolean = default match {
    case None       => false
    case Some(null) => false
    case Some(a)    => true
  }

  def isEnumMultiple(name: String): Boolean = name.endsWith("s") && !notMultipleEnums.contains(name)

  def toQueryParam(name: String): String = {
    val code = new ListBuffer[String]
    if (multiple && this.`type` != "enum") {
      if (defaultIsValid) {
        //        code += "Seq[String] = "+default.get
        code += "Seq[String] = Nil"
      } else {
        code += "Seq[String] = Nil"
      }
    } else if (!required && !default.isDefined && options.isEmpty) {
      code += s"Option[${getType}] = None"
    } else if (this.`type` == "list") {
      if (defaultIsValid) {
        //        code += "Seq[String] = "+default.get
        code += "Seq[String] = Nil"
      } else {
        code += "Seq[String] = Nil"
      }
    } else if (this.`type` == "enum") {
      val enumT = getParameterName(name.capitalize)
      if (isEnumMultiple(enumT)) {
        if (defaultIsValid) {
//          val d = default.get match {
//            case s: String             => s"${enum}.$s"
//            case fields: List[_]       => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
//            case fields: ListBuffer[_] => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
//          }
          //          code += s"""Seq[${enum}.${enum}] = Seq(${d})"""
          code += s"""Seq[${enumT}] = Nil"""
        } else {
          code += s"""Seq[${enumT}] = Nil"""
        }
      } else {
        if (defaultIsValid) {
          code += s"""${enumT} = ${enumT}.""" + default.get.toString.replace("\"", "")
        } else {
          code += s"""Option[${enumT}] = None"""
        }
      }
    } else {
      if (defaultIsValid) {
        code += getType + "=" + getCookedDefault
      } else {
        if (required)
          code += s"${getType}"
        else
          code += s"Option[${getType}] = None"
      }
    }
    code.mkString
  }

  def toBodyCode(name: String): String = {
    val code = new ListBuffer[String]
    if (multiple) {
      code += s"""    if(${getParameterName(name)}.nonEmpty){
                 |        queryArgs += ("$name" -> ${getParameterName(name)}.toList.mkString(","))
                 |    }
                 |""".stripMargin('|')
    } else if (this.`type` == "enum") {
      val enumT     = getParameterName(name)
      val enumClass = getParameterName(name.capitalize)
      val enumCap   = name.toCamelUpper
      if (isEnumMultiple(enumCap)) {
        code += s"""    if($enumT.nonEmpty) {"""
        if (defaultIsValid) {
          val d = default.get match {
            case Json.Str(v) => s"${enumT}.${v}"
            case Json.Arr(values) =>
              values.collect { case Json.Str(v) => v }.map(s => s"${enumT}.${s}").toList.mkString(", ")
          }
          code += s"""
                     |        if($enumT.toSet !=  Set(${d.capitalize})){
                     |            queryArgs += ("$name" ->$enumT.mkString(","))
                     |        }
          """.stripMargin

          //          val d = default.get match {
          //            case s: String => s"${enum}.$s"
          //            case fields: List[_] => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
          //            case fields: ListBuffer[_] => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
          //          }
          //          //          code += s"""Seq[${enum}.${enum}] = Seq(${d})"""
          //          code += s"""Seq[${enum}.${enum}] = Nil"""
        } else {
          code += s"""
                     |        queryArgs += ("$name" -> $enumT match {
                     |           case Some(e) => e.mkString(",")
                     |           case e => e.mkString(",")
                     |        })
          """.stripMargin

        }
        code += s"""
                   |    }
          """.stripMargin
      } else if (defaultIsValid) {
        code += s"""    if(${enumT}!=${enumClass}.${default.get})
                   |        queryArgs += ("$name" -> ${enumT}.toString)
                   |""".stripMargin('|')
      } else {
        code += s"""    $enumT.foreach{ v =>
                   |        queryArgs += ("$name" -> v$requiredToString)
                   |    }
                   |""".stripMargin('|')
      }
    } else if (defaultIsValid) {
      code += s"""    if(${getParameterName(name)}!=$getCookedDefault) queryArgs += ("$name" -> ${getParameterName(
          name,
        )}$requiredToString)\n"""
    } else {
      code += s"""    ${getParameterName(name)}.foreach{ v=> queryArgs += ("$name" -> v$requiredToString )}\n"""
    }
    code.mkString
  }

  def getValidEnum(name: String): String = name match {
    case "type" => "`type`"
    case "wait" => "`wait`"
    case value  => value
  }

  def getEnum(name: String): Map[String, String] =
    `type` match {
      case "enum" =>
        val newName = getParameterName(name.capitalize)
        Map(newName -> s""" sealed trait $newName extends EnumEntry
                          | case object $newName extends CirceEnum[$newName] with Enum[$newName] {
                          |    ${options
                           .map(getValidEnum)
                           .map(st => s"  case object $st extends $newName")
                           .mkString("\n")}
                          |
                          |    val values = findValues
                          |  }""".stripMargin('|'))
      case _ => Map.empty[String, String]
    }
}

case class CallParameter(
    name:        String,
    `type`:      String,
    description: String,
    options:     List[String] = Nil,
    default:     Option[Json] = None,
    required:    Boolean = false,
    scope:       String = "query",
    subtype:     Option[String] = None,
) extends AbstractParameter {
  def getCookedDocumentation: String = s" * @param $parameterName $description"

  def getDefParameter = s"var $parameterName : $toQueryParam"

  def getDefParameterNoVar = s"$parameterName: $toQueryParam"

  def getDefault(clsName:String)(implicit parserContext: ParserContext):String={
    this.default match {
      case Some(value) => value.toJson
      case None => parserContext.getDefaultParameter(clsName, this.name, this.`type`)
    }
  }

  def getParameterWithDefault(clsName:String)(implicit parserContext: ParserContext):String = {
  if(required) {
      s"$parameterName: $toQueryParam = ${getDefault(clsName)}"
    } else s"$parameterName: $toQueryParam"
  }

  def parameterName: String = this.getParameterName(this.name)

  def toQueryParam: String = toQueryParam(this.name)

  def toObjectParam: String = toQueryParam(this.name).split("=")(0).trim()

  def toBodyCode: String = toBodyCode(this.name)

}

case class Parameter(
    `type`:      String,
    description: String,
    options:     List[String] = Nil,
    default:     Option[Json] = None,
    required:    Boolean = false,
    subtype:     Option[String] = None,
) extends AbstractParameter {

  def toCallParameter(name: String): CallParameter = new CallParameter(
    name,
    `type` = this.`type`,
    description = this.description,
    options = options,
    default = default,
    required = required,
    subtype = subtype,
  )

}

object Parameter {
  implicit val codec: JsonValueCodec[Parameter] = JsonCodecMaker.make[Parameter]
}
