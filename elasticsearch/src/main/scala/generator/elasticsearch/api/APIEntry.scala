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
import generator.ts.{ParserContext, ScalaClassMember}
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
  private var implements: List[String] = List.empty[String]

  // from indices.get_field_mapping -> IndicesGetFieldMapping
//  val className: String = {
//    val n = name.split('.').flatMap(_.split('_')).map(_.capitalize).mkString
//    APIEntry.mappingNames.getOrElse(n, n)
//  }
  // from indices.get_field_mapping -> GetFieldMapping
  val className: String = {
    val n = name.split('.').drop(1).flatMap(_.split('_')).map(_.capitalize).mkString
    APIEntry.mappingNames.getOrElse(n, n)
  }

  val scalaRequest:  String = className + "Request"
  val scalaResponse: String = className + "Response"

  def toCallParameter(p: ScalaClassMember): CallParameter = CallParameter(
    name=p.name,
    `type`=p.typ.map{
      typ =>
      var result=typ.toScalaType
        if(result.startsWith("Option["))result=result.substring("Option[".length, result.length-1)
      result
    }.getOrElse("Json"),
    description = p.comments.rawCs.map(fixCommentTyped).filter(_.nonEmpty).mkString("", "", "\n"),
    required = !p.isOptional

  )

  def fixCommentTyped(str:String):String={
    var result=str
    if(result.startsWith("/**")) result=result.substring(3).trim
    result=result.dropWhile(_.isSpaceChar)
    if(result.startsWith("* ")) result=result.substring(2)
    result = result.dropWhile(_.isSpaceChar)
    if(result.contains("*/")) result=result.replace("*/", "").trim
    if(result.startsWith("@")) result="\n"+result

    result
  }

  def parse(implicit parserContext: ParserContext): APIEntry = {
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
      case None => "Json.Obj"
      case Some(b) =>
        b.serialize match {
          case ""     => "Json.Obj"
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

    // we add other request parameters taken from typed parsed data
    // retrieve typed data
    parserContext.getNamespaceClass(s"${scope}.requests.${scalaRequest}").foreach{
      rqTyped =>
        implements = if (rqTyped.implements.isEmpty) Nil else {
          rqTyped.implements.map(_.toScalaType)
        }
        // called all derived typed parameters
        val recursiveFields = rqTyped.implements.flatMap(impl => parserContext.getRecursiveMembers(impl))
        para2 ++= recursiveFields.map(p => toCallParameter(p))
    }


    this.parameters = para2.toList.filter(_.required) ++ para2.filterNot(_.required).sortBy(_.name.dropWhile(_ == '_'))
    this
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

  def hasBody: Boolean = this.body.isDefined

  def clientName: String = if (scope == "client") "this" else "client"

  def getClientZIOAccessorsCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    // generating class documentation
    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)

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
    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)

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
    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)

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

  def responseFilename: String = s"${responsePackage}.${scalaResponse}"
  def responsePackage:  String = s"${Constants.namespaceName}.${scope.replace("client", "")}.responses".stripSuffix(".")

  def responseClass(implicit parserContext: ParserContext): String = {
    val text = new ListBuffer[String]
    // generating class documentation
    text += s"package $responsePackage\n\n"
    text += s"import scala.collection.mutable\n"
    text += s"import zio.json._\n"
    text += s"import zio.json.ast._\n"
//    text += s"import com.github.plokhotnyuk.jsoniter_scala.core._\n"
//    text += s"import com.github.plokhotnyuk.jsoniter_scala.macros._\n"
    text ++= extra.map(v => s"import ${Constants.namespaceName}.${v._1}\n")

    var implements: List[String] = List.empty[String]


    val para2=new ListBuffer[(String, CallParameter)]()

    // we add other request parameters taken from typed parsed data
    // retrieve typed data
    parserContext.getNamespaceClass(s"${scope}.responses.${scalaResponse}").foreach {
      rqTyped =>
        implements = if (rqTyped.implements.isEmpty) Nil else {
          rqTyped.implements.map(_.toScalaType)
        }
        // called all derived typed parameters

        val recursiveFields = new ListBuffer[(String, ScalaClassMember)]()
         rqTyped.members.find(_.name=="body").foreach{
           c =>
             recursiveFields ++=c.members.map(v => rqTyped.name -> v )
             c.typ match {
               case Some(value) =>
                 recursiveFields ++=parserContext.getRecursiveMembersWithParentName(value)
               case None =>
             }
         }
        para2 ++= recursiveFields.map(p => p._1 -> toCallParameter(p._2))
    }


    val parameters: List[(String, CallParameter)] = para2.toList.filter(_._2.required) ++ para2.filterNot(_._2.required).sortBy(_._2.name.dropWhile(_ == '_'))
    text ++= cookedDocumentation(true, parameters.map(_._2)).map(s => "  " + s)

    text += s"final case class ${scalaResponse}(\n"

    val newLineFunc: String = "  "


    text += parameters
      .map {
        case (className, parameter) =>
        val key = ""
        newLineFunc + s"$key${parameter.getParameterWithDefault(className)}"
      }
      .toList
      .mkString(",\n")
    if(implements.nonEmpty){
      implements.headOption.foreach{
        cls =>
          text+=s") extends $cls "
      }
      val remains=implements.tail
      if(remains.nonEmpty){
        text+=s"${implements.mkString("with ", "with ", "")}"
      }
      text+="{\n"

    } else {
      text+=s") {\n"
    }
    text += "}\n"

    text += s"object $scalaResponse{\n"
//    text += s"implicit val jsonCodec: JsonValueCodec[$scalaResponse] = JsonCodecMaker.make[$scalaResponse](CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforceCamelCase))"
    text += s"   implicit val jsonCodec: JsonCodec[$scalaResponse] = DeriveJsonCodec.gen[$scalaResponse]"
    text += "}\n"

    text.mkString
  }

  def getRequestZioReturn: String = s"ZIO[Any, FrameworkException, ${getRequestReturn}]"

  def getRequestReturn: String =
    if (nativeResponse.isDefined)
      nativeResponse.get.split("""\.""").last
    else
      result match {
        case Some(value) => value.scala
        case _           => scalaResponse
      }

  def cookedDocumentation(withParameters:Boolean, parameters: List[CallParameter]): List[String] = {
    val doc = new ListBuffer[String]
    doc += "/*\n"
    doc += " * " + this.documentation.description + "\n"
    doc += " * For more info refers to " + this.documentation.url + "\n"
    if(withParameters && parameters.nonEmpty){
      doc += " * \n"
      doc += parameters.map(_.getCookedDocumentation).mkString("", "\n", "\n")
    }
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

    doc ++= cookedDocumentation(true, this.parameters)
    val classDef =
      doc += "\n"
    doc += s"final case class ${scalaRequest}(\n"

    val newLineFunc: String = " " * classDef.length


    doc += this.parameters
      .map { parameter =>
        val key = ""
        newLineFunc + s"$key${parameter.getDefParameterNoVar}"
      }
      .toList
      .mkString(",\n") + s") extends ActionRequest ${implements.mkString("with ", "with ", "")}{\n"
    doc += s"""  def method:String="${methods.head}"\n\n"""
    doc ++= getDefUrlPath
    doc += "\n"
    doc ++= getDefQueryArgs
    doc += "\n"
    if (!hasBody) {
      doc += """  def body:Json=Json.Null"""
      doc += "\n"
      doc += "\n"
    }
    doc += "  // Custom Code On\n"
    doc += "  // Custom Code Off\n"
    doc += "\n"

    doc += "}\n"
    doc += "\n"
//    doc += s"object ${scalaRequest} {\n"
//    doc += s"implicit val jsonCodec: JsonValueCodec[$scalaRequest] = JsonCodecMaker.make[$scalaRequest](CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforceCamelCase))"
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
