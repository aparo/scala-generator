/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import java.io.{File, FileWriter}

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.elasticsearch.Constants
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
  private var path:       String              = ""
  var extra:              Map[String, String] = Map.empty[String, String]
  var implicits:          List[String]        = List.empty[String]
  private var parameters: List[CallParameter] = List.empty[CallParameter]

  // from indices.get_field_mapping -> IndicesGetFieldMapping
  val className: String = {
    val n = name.split('.').flatMap(_.split('_')).map(_.capitalize).mkString
    APIEntry.mappingNames.getOrElse(n, n)
  }
  val scalaRequest:  String = className + "Request"
  val scalaResponse: String = className + "Response"

  parse()

  private def parse(): Unit = {
    val extra      = new mutable.HashMap[String, String]
    val implicits  = new ListBuffer[String]
    val parameters = new ListBuffer[CallParameter]
    this.url.params.foreach { p =>
      extra ++= p._2.getEnum(p._1)
    }

    // add required in url paths
    val parts = """\{(.*?)\}""".r

    var path: String = url.validPaths.map(_.path).head
    var requiredPaths = parts.findAllMatchIn(path).map(_.group(1)).toList
    var partRequired  = new mutable.HashMap[String, Boolean]
    requiredPaths.foreach { p =>
      partRequired += p -> true
    }
    url.validPaths.map(_.path).foreach {
      case s: String if s.contains("{") =>
        if (s.length > path.length) {
          requiredPaths = parts.findAllMatchIn(s).map(_.group(1)).toList
          requiredPaths.foreach { p =>
            if (!partRequired.contains(p)) partRequired += p -> false
          }
          path = s
        }
      case value =>
    }
    this.path = path
    val requiredPathsCount = requiredPaths.count(p => partRequired(p))
    val hasBody            = this.body.isDefined
    val bodyType = this.body match {
      case None => "JsonObject"
      case Some(b) =>
        b.serialize match {
          case ""     => "JsonObject"
          case "bulk" => "list"
        }
    }

    this.params.foreach { case (name, param) =>
      parameters += param.toCallParameter(name)
    }

    var bodyDone = false
    if (requiredPaths.nonEmpty) {
      if (requiredPathsCount == 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = this.body.get.required,
          scope = "body",
        )
        bodyDone = true
      }
      parameters ++= requiredPaths.map { p =>
        val partDocumentation = url.getPartDocumentation(p)
        val multiple          = CommonStrings.isMultiple(partDocumentation)
        CallParameter(
          cookPart(p, multiple),
          "string",
          description = partDocumentation,
          required = partRequired(p),
          scope = "uri",
        )
      }.toList

      if (requiredPathsCount > 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = this.body.get.required,
          scope = "body",
        )
        bodyDone = true
      }
    }

    if (!bodyDone && hasBody) {
      parameters += CallParameter(
        "body",
        bodyType,
        "body the body of the call",
        required = this.body.get.required,
        scope = "body",
      )
      bodyDone = true
    }
    parameters ++= this.url.params
      .filterNot { case (name, value) =>
        requiredPaths.contains(name)
      }
      .map { case (name, value) =>
        CallParameter(
          name,
          value.`type`,
          description = value.description,
          options = value.options,
          default = value.default,
          required = false,
          subtype = value.subtype,
        )

      }
      .toList

    result match {
      case Some(value) =>
        implicits += value.scala
      case _ =>
    }
    this.implicits = implicits.toList
    this.extra = extra.toMap
    // cleaning dupped names
    val para2 = new mutable.ListBuffer[CallParameter]
    val mset  = new mutable.HashSet[String]
    parameters.foreach { p =>
      if (!mset.contains(p.parameterName)) {
        para2 += p
        mset += p.parameterName
      }
    }

    this.parameters = para2.toList.filter(_.required) ++ para2.filterNot(_.required).sortBy(_.name.dropWhile(_ == '_'))
  }

  def methods: List[String] = url.paths.flatMap(_.methods)

  def toGoodName(value: String): String = value match {
    case value: String if value.contains("_") =>
      val pos = value.indexOf('_')
      value.take(pos) + toGoodName(value.drop(pos + 1).capitalize)
    case _ => value
  }

  private def cookPart(name: String, multiple: Boolean) = name match {
    case "type"  => if (multiple) "docTypes" else "docType"
    case "index" => if (multiple) "indices" else "index"
    case v       => v
  }

  val hasBody: Boolean = this.body.isDefined

  def clientName: String = if (scope == "client") "this" else "client"

  def getClientZIOAccessorsCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    // generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"def $funcName("
    text += defFunc

    text += this.parameters
      .map { parameter =>
        s"${parameter.getDefParameterNoVar}"

      }
      .mkString(",\n")
    text += "): " + s"ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]"
    text += s" =ZIO.accessM[%%SERVICE%%](_.get.$funcName("

    text += parameters
      .map { p =>
        val pname = p.parameterName
        s"$pname = $pname"
      }
      .mkString(",\n")
    text += "))\n\n"

    text += s"  def $funcName(request:${scalaRequest}): ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]= ZIO.accessM[%%SERVICE%%](_.get.execute(request))\n\n"

    text.mkString
  }

  def getClientCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    // generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"def $funcName("
    text += defFunc

    text += this.parameters
      .map { parameter =>
        s"${parameter.getDefParameterNoVar}"

      }
      .mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += s"val request= ${scalaRequest}("

    text += parameters
      .map { p =>
        val pname = p.parameterName
        s"$pname = $pname"
      }
      .mkString(",\n")
    text += ")\n"

    text += "\n"
    text += s"$funcName(request)\n"
    text += "\n  }\n\n"

    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= $clientName.execute(request)\n\n"

    text.mkString
  }

  def getClientNativeCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    // generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"  def $funcName("
    text += defFunc + "\n"

    val newLineFunc: String = " " * defFunc.length

    text += this.parameters
      .map { parameter =>
        newLineFunc + s"${parameter.getDefParameterNoVar}"

      }
      .toList
      .mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += s"val request= new ${scalaRequest}()\n"

    //    val funcCall = s"    $funcName(new $scalaRequest("
    //    var size = funcCall.length

    text += parameters
      .map { p =>
        val pname = p.parameterName
        if (!p.required) {
          if (p.default.isDefined) {
            s"if(${pname} != ${p.getCookedDefault}})request.${pname}(${pname})"
          } else {
            s"${pname}.foreach{p=> request.${pname}(p) }"
          }
        } else {
          s"request.${pname}(${pname})"
        }
      }
      .mkString("\n")
    text += "\n"

    text += s"$funcName(request)\n"

    text += "\n  }\n\n"

    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= ${clientName}.execute(request)\n\n"

    text.mkString
  }

  def requestFilename: String = requestPackage + "." + nativeRequest.get.split('.').last

  def requestPackage: String = s"${Constants.namespaceName}.requests.${scope.replace("client", "")}".stripSuffix(".")

//  def requestClass: String = {
//    val text = new ListBuffer[String]
//    //generating class documentation
//    text += s"package $requestPackage\n\n"
//
//    text += "import scala.collection.mutable\n"
//    text += "import play.api.libs.json._\n"
//    text += "import play.json.extra._\n"
//    text ++= extra.map(v => s"import elasticsearch.${v._1}\n")
//    text += "import elasticsearch.client.ElasticSearchCall\n\n"
//
//
//    text ++= cookedDocumentation.map(s => "  " + s)
//
//
//      text += s"@JsonFormat\n"
//    text += s"final case class ${nativeRequest.get.split("""\.""").last}(\n"
//
//    val newLineFunc: String = "    "
//
//    text += this.parameters.map {
//      parameter =>
//
//        val key=if (parameter.name != parameter.parameterName) {s"""@key("${parameter.name}") """} else ""
//
//        newLineFunc + s"$key${parameter.getDefParameterNoVar}"
//
//    }.mkString(",\n")
//    text += ") extends ElasticSearchCall {\n"
//
//    //queryArgs
//    text += "\n"
//    text += newLineFunc+s"""val urlPath:String = buildUrl("$path""""
//    text += this.parameters.filter(_.scope=="uri").map {
//      parameter =>
//        ", "+parameter.parameterName
//    }.mkString("")
//    text += ")\n\n"
//
//    //queryArgs
//    text += "\n"
//    text += newLineFunc+"def queryArgs:Map[String,String] = {\n"
//    text += newLineFunc*2+"val result = new mutable.HashMap[String,String]\n"
//    text += newLineFunc*2
//    text += this.parameters.filter(_.scope=="query").map {
//      p =>
//        val pname = p.parameterName
//        if (!p.required) {
//          if (p.default.isDefined) {
//            s"""if(${pname} != ${p.getCookedDefault}) result += ("${p.name}" -> ${pname})"""
//          } else {
//            s"""${pname}.foreach{ p => result += ("${p.name}" -> p) }"""
//          }
//        } else {
//          s"""result += ("${p.name}" -> ${pname})"""
//        }
//
//    }.mkString(s"\n${newLineFunc*2}")
//    text += "\n"+newLineFunc+"}\n"
//    text += "\n"
//
//
//
//    text += "}\n"
//
//    text.mkString
//  }

  def responseFilename: String = s"${responsePackage}.${scalaResponse}"
  def responsePackage:  String = s"${Constants.namespaceName}.responses.${scope.replace("client", "")}".stripSuffix(".")

  def responseClass: String = {
    val text = new ListBuffer[String]
    // generating class documentation
    text += s"package $responsePackage\n\n"
    text += s"import scala.collection.mutable\n"
    text += s"import io.circe.derivation.annotations._\n"
    text ++= extra.map(v => s"import ${Constants.namespaceName}.${v._1}\n")

    text ++= cookedDocumentation.map(s => "  " + s)

    text += s"@JsonCodec\n"
    text += s"final case class $scalaResponse(){\n"
    text += "}\n"

    text.mkString
  }

  def getClientCalls_old: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    // generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"  def $funcName("
    text += defFunc + "\n"

    val newLineFunc: String = " " * defFunc.length

    text += this.parameters
      .map { parameter =>
        newLineFunc + s"${parameter.getDefParameterNoVar}"

      }
      .toList
      .mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += "    // Custom Code On\n"
    text += "    // Custom Code Off\n"

    val funcCall = s"    $funcName(new $scalaRequest("
    var size     = funcCall.length
    text += funcCall + parameters
      .map { p =>
        val pname = p.parameterName
        size = size + pname.length * 2 + 1
        if (size > 80) {
          size = pname.length * 2 + 1 + funcCall.length
          "\n" + " " * funcCall.length + s"${pname}=${pname}"
        } else s"${pname}=${pname}"
      }
      .mkString(", ")
    text += s"))\n"

    //    if (hasBody)
    //      if (result.isDefined) {
    //        text += s"""    val response=${clientName}.doCall("${methods.head}", urlPath, body, queryArgs=queryArgs.toMap)\n"""
    //      } else {
    //        text += s"""    ${clientName}.doCall("${methods.head}", urlPath, body, queryArgs=queryArgs.toMap)"""
    //      }
    //    else
    //    if (result.isDefined) {
    //      text += s"""    val response=${clientName}.doCall("${methods.head}", urlPath, queryArgs=queryArgs.toMap)\n"""
    //    } else {
    //      text += s"""    ${clientName}.doCall("${methods.head}", urlPath, queryArgs=queryArgs.toMap)"""
    //    }
    //
    //    result match {
    //      case Some(value) =>
    //        val implicitsName = value.scala(0).toLower + value.scala.substring(1) + "Fmt"
    //        text += s"""    ${implicitsName}.reads(response).get"""
    //      case _ =>
    //    }

    text += "\n  }\n\n"

    text += s"  def $funcName(request:$scalaRequest):$getRequestZioReturn= ${clientName}.doCall(request).map(fromJsonOrError[$getRequestReturn])\n\n"

    text.mkString
  }

  def getRequestZioReturn: String = s"ZioResponse[${getRequestReturn}]"

  def getRequestReturn: String =
    if (nativeResponse.isDefined)
      nativeResponse.get.split("""\.""").last
    else
      result match {
        case Some(value) => value.scala
        case _           => scalaResponse
      }

  lazy val cookedDocumentation: List[String] = {
    val doc = new ListBuffer[String]
    doc += "/*\n"
    doc += " * " + this.documentation.description + "\n"
    doc += " * For more info refers to " + this.documentation.url + "\n"
    doc += " * \n"
    doc += this.parameters.map(_.getCookedDocumentation).mkString("\n")
    doc += "\n"
    doc += " */\n"
    doc.toList
  }

  def cookdoc(lines: List[String]): String = {
    val text = new ListBuffer[String]
    text += "  /*\n"
    lines.foreach(l => text += "   * " + l.stripMargin(' '))
    text += "   * */"

    text.toList.mkString + "\n"
  }

  lazy val scope: String = {
    val tokens = name.split("\\.")
    var scope  = "client"
    if (tokens.length > 1)
      scope = tokens(tokens.length - 2)
    scope
  }

  def generateRequest: List[String] = {
    val doc = new ListBuffer[String]
    // generating class documentation

    doc ++= cookedDocumentation
    //    doc += "\n"
    val classDef =
      doc += "@JsonCodec\n"
    doc += s"final case class ${scalaRequest}(\n"

    val newLineFunc: String = " " * classDef.length

    doc += this.parameters
      .map { parameter =>
        val key = if (parameter.name != parameter.parameterName) {
          s"""@JsonKey("${parameter.name}") """
        } else ""
        newLineFunc + s"$key${parameter.getDefParameterNoVar}"
      }
      .toList
      .mkString(",\n") + ") extends ActionRequest {\n"
    doc += s"""  def method:String="${methods.head}"\n\n"""
    doc ++= getDefUrlPath
    doc += "\n"
    doc ++= getDefQueryArgs
    doc += "\n"
    if (!hasBody) {
      doc += """  def body:Json=JsNull"""
      doc += "\n"
      doc += "\n"
    }
    doc += "  // Custom Code On\n"
    doc += "  // Custom Code Off\n"
    doc += "\n"

    doc += "}\n"
    doc += "\n"
//    doc += "\n"
//    doc += s"object ${scalaRequest} {\n"
//    doc += "\n"
//    doc += "  def apply(" + this.parameters.filter(p => p.required || (!p.required && p.scope == "uri")).
//      map(p => s"${p.parameterName}:${p.toObjectParam}").toList.mkString(", ") + s"):$scalaRequest = {\n"
//    doc += s"      new ${scalaRequest}(" + this.parameters.filter(p => p.required || (!p.required && p.scope == "uri")).
//      map(p => s"${p.parameterName}=${p.parameterName}").toList.mkString(", ") + s")\n"
//    doc += "    }\n"
//    doc += "  // Custom Code On\n"
//    doc += "  // Custom Code Off\n"
//    doc += "}\n"
//    doc += "\n"

    doc.toList
  }

  def getDefQueryArgs: List[String] = {
    val parameters = this.parameters.filterNot(_.required).filterNot(_.scope == "uri")
    if (parameters.nonEmpty) {
      val text = new ListBuffer[String]
      text += "  def queryArgs:Map[String, String] = {\n"
      text += "    //managing parameters\n"
      text += "    val queryArgs = new mutable.HashMap[String, String]()\n"
      parameters.foreach { parameter =>
        val code = parameter.toBodyCode
        if (!code.isEmpty)
          text += code
      }
      text += "    // Custom Code On\n"
      text += "    // Custom Code Off\n"

      text += "    queryArgs.toMap\n"
      text += "  }\n"
      text.toList
    } else {
      List("def queryArgs: Map[String, String] = Map.empty[String, String]\n")
    }

  }

  def getDefUrlPath: List[String] = {
    val text = new ListBuffer[String]
    if (this.path.contains("{")) {
      text += s"""  def urlPath:String = this.makeUrl("""
      text += path
        .split("/")
        .map { p =>
          val cleanValue = p.stripPrefix("{").stripSuffix("}")
          if (p.startsWith("{"))
            toGoodName(cookPart(cleanValue, CommonStrings.isMultiple(url.getPartDocumentation(cleanValue))))
          else "\"" + cookPart(p, false) + "\"" // is not a variable it cannot be multiple
        }
        .toList
        .tail
        .mkString(", ")
      text += """)"""
    } else {
      text += s"""  def urlPath = "${this.path}" """
    }
    text += "\n"
    text.toList
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
