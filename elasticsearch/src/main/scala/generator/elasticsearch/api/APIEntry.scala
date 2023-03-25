/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.elasticsearch.Constants
import generator.ts.{CodeData, ParserContext, ScalaClassMember, ScalaObjectType, SimpleType, UndefinedType, UnionType}
import generator.ts.Converters._
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
  private var path:        String              = ""
  var extra:               Map[String, String] = Map.empty[String, String]
  var implicits:           List[String]        = List.empty[String]
  private var parameters:  List[CallParameter] = List.empty[CallParameter]
  private var implements:  List[String]        = List.empty[String]
  private var requestBody: String              = "Json"
  var extraClassCodes:     List[CodeData]      = List.empty[CodeData]
  import ApiEntryTyped._

  // from indices.get_field_mapping -> IndicesGetFieldMapping
//  val className: String = {
//    val n = name.split('.').flatMap(_.split('_')).map(_.capitalize).mkString
//    APIEntry.mappingNames.getOrElse(n, n)
//  }
  // from indices.get_field_mapping -> GetFieldMapping
  lazy val className: String = {
    val n = name.split('.').drop(1).flatMap(_.split('_')).map(_.capitalize).mkString
    APIEntry.mappingNames.getOrElse(n, n)
  }

  lazy val scalaRequest:  String = className + "Request"
  lazy val scalaResponse: String = className + "Response"

//  def parse(implicit parserContext: ParserContext): APIEntry = {
//    val extra      = new mutable.HashMap[String, String]
//    val implicits  = new ListBuffer[String]
//    val parameters = new ListBuffer[CallParameter]
//    this.url.params.foreach { p =>
//      extra ++= p._2.getEnum(p._1)
//    }
//
//    // add required in url paths
//    val parts = """\{(.*?)\}""".r
//
//    var path: String = url.validPaths.map(_.path).head
//    var requiredPaths = parts.findAllMatchIn(path).map(_.group(1)).toList
//    var partRequired  = new mutable.HashMap[String, Boolean]
//    requiredPaths.foreach { p =>
//      partRequired += p -> true
//    }
//    url.validPaths.map(_.path).foreach {
//      case s: String if s.contains("{") =>
//        if (s.length > path.length) {
//          requiredPaths = parts.findAllMatchIn(s).map(_.group(1)).toList
//          requiredPaths.foreach { p =>
//            if (!partRequired.contains(p)) partRequired += p -> false
//          }
//          path = s
//        }
//      case value =>
//    }
//    this.path = path
//    val requiredPathsCount = requiredPaths.count(p => partRequired(p))
//    val hasBody            = this.body.isDefined
//    val bodyType = this.body match {
//      case None => "Json"
//      case Some(b) =>
//        b.serialize match {
//          case ""     => "Json"
//          case "bulk" => "list"
//        }
//    }
//
//    this.params.foreach { case (name, param) =>
//      parameters += param.toCallParameter(name)
//    }
//
//    var bodyDone = false
//    if (requiredPaths.nonEmpty) {
//      if (requiredPathsCount == 0 && hasBody) {
//        parameters += CallParameter(
//          "body",
//          bodyType,
//          "body the body of the call",
//          required = this.body.get.required,
//          scope = "body",
//        )
//        bodyDone = true
//      }
//      parameters ++= requiredPaths.map { p =>
//        val partDocumentation = url.getPartDocumentation(p)
//        val multiple          = CommonStrings.isMultiple(partDocumentation)
//        CallParameter(
//          cookPart(p, multiple),
//          "string",
//          description = partDocumentation,
//          required = partRequired(p),
//          scope = "uri",
//        )
//      }
//
//      if (requiredPathsCount > 0 && hasBody) {
//        parameters += CallParameter(
//          "body",
//          bodyType,
//          "body the body of the call",
//          required = this.body.get.required,
//          scope = "body",
//        )
//        bodyDone = true
//      }
//    }
//
//    if (!bodyDone && hasBody) {
//      parameters += CallParameter(
//        "body",
//        bodyType,
//        "body the body of the call",
//        required = this.body.get.required,
//        scope = "body",
//      )
//      bodyDone = true
//    }
//    parameters ++= this.url.params
//      .filterNot { case (name, _) =>
//        requiredPaths.contains(name)
//      }
//      .map { case (name, value) =>
//        CallParameter(
//          name,
//          value.`type`,
//          description = value.description,
//          options = value.options,
//          default = value.default,
//          required = false,
//          subtype = value.subtype,
//        )
//
//      }
//      .toList
//
//    result match {
//      case Some(value) =>
//        implicits += value.scala
//      case _ =>
//    }
//    this.implicits = implicits.toList
//    this.extra = extra.toMap
//    // cleaning dupped names
//    val para2 = new mutable.ListBuffer[CallParameter]
//    val mset  = new mutable.HashSet[String]
//    parameters.foreach { p =>
//      if (!mset.contains(p.parameterName)) {
//        para2 += p
//        mset += p.parameterName
//      }
//    }
//
//    // we add other request parameters taken from typed parsed data
//    // retrieve typed data
//    parserContext.getNamespaceClass(s"${scope}.requests.${scalaRequest}").foreach{
//      rqTyped =>
//        rqTyped.members.foreach{
//          case sc if sc.name=="path_parts" =>
//            sc.typ.getOrElse(UndefinedType) match {
//              case UndefinedType => ()
//              case UnionType(_, _) => ()
//              case st:SimpleType =>
//                if(!para2.exists(_.name==st.name))
//                  para2 += CallParameter(name=st.name, `type`=st.toScalaType, description = "", required = st.isRequired, scope="uri")
//              case ScalaObjectType(_, _, _, members, _, _) =>
//                members.foreach{
//                  st1 =>
//                    st1.members.foreach{
//                      st =>
//                        if (!para2.exists(_.name == st.name))
//                          para2 += toCallParameter(st).copy(scope="uri")
//                    }
//                }
//            }
//          case sc if sc.name=="query_parameters" =>
//            sc.typ.getOrElse(UndefinedType) match {
//              case UndefinedType => ()
//              case UnionType(_, _) => ()
//              case st: SimpleType =>
//                if (!para2.exists(_.name == st.name))
//                  para2 += CallParameter(name = st.name, `type` = st.toScalaType, description = "", required = st.isRequired, scope = "query")
//              case ScalaObjectType(_, _, _, members, _, _) =>
//                members.foreach {
//                  st1 =>
//                    st1.members.foreach {
//                      st =>
//                        if (!para2.exists(_.name == st.name))
//                          para2 += toCallParameter(st).copy(scope = "query")
//                    }
//                }
//            }
//          case sc if sc.name=="body" =>
//            parserContext.getTypedBody(scalaRequest) match {
//              case Some((cls,desc)) =>
//                // we update body
//              this.requestBody=cls
//                para2.find(_.name == "body") match {
//                  case Some(value) => para2 -= value
//                    para2 += value.copy(`type`=cls, description = desc)
//                  case None =>
//                    para2 += CallParameter(
//                      "body",
//                      cls,
//                      desc,
//                      required = this.body.get.required,
//                      scope = "body",
//                    )
//                }
//
//              case None =>
//                // we create a request body
//              val rqBody=scalaRequest+"Body"
//              sc.typ.foreach{
//                scalatype =>
//                  scalatype match {
//                    case st:ScalaObjectType =>
//                      val cd = st.copy(name = rqBody, namespace = this.requestPackage.replace("zio.elasticsearch.", "")).toCode
//                      if (cd != CodeData.empty) {
//                        extraClassCodes ::= cd.copy(imports = List(s"import $basePackage._")::: cd.imports)
//                        this.requestBody=rqBody
//                        para2.find(_.name == "body") match {
//                          case Some(value) => para2 -= value
//                            para2 += value.copy(`type` = rqBody)
//                          case None =>
//                            para2 += CallParameter(
//                              "body",
//                              rqBody,
//                              s"a $rqBody",
//                              required = this.body.get.required,
//                              scope = "body",
//                            )
//                        }
//
//                      }
//
//                  }
//
//              }
//            }
//        }
//        implements = if (rqTyped.implements.isEmpty) Nil else {
//          rqTyped.implements.map(_.toScalaType)
//        }
//        // called all derived typed parameters
//        val recursiveFields = rqTyped.implements.flatMap(impl => parserContext.getRecursiveMembers(impl))
//        para2 ++= recursiveFields.map(p => toCallParameter(p))
//    }
//
//
//    this.parameters = para2.toList.filter(_.required) ++ para2.filterNot(_.required).sortBy(_.name.dropWhile(_ == '_'))
//    this
//  }

//  def methods: List[String] = url.paths.flatMap(_.methods)
//
//
//  def clientName: String = if (scope == "client") "this" else "client"

  def hasBody: Boolean = this.body.isDefined

//  def getClientZIOAccessorsCalls(implicit parserContext: ParserContext): String = {
//    val text     = new ListBuffer[String]
//    val funcName = toGoodName(name.split("\\.").last)
//    // generating class documentation
//    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)
//
//    val defFunc = s"def $funcName("
//    text += defFunc
//
//    text += this.parameters
//      .map { parameter =>
//        s"${parameter.getDefParameterNoVar}"
//
//      }
//      .mkString(",\n")
//    text += "): " + s"ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]"
//    text += s" =ZIO.accessM[%%SERVICE%%](_.get.$funcName("
//
//    text += parameters
//      .map { p =>
//        val pname = p.parameterName
//        s"$pname = $pname"
//      }
//      .mkString(",\n")
//    text += "))\n\n"
//
//    text += s"  def $funcName(request:${scalaRequest}): ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]= ZIO.accessM[%%SERVICE%%](_.get.execute(request))\n\n"
//
//    text.mkString
//  }
//
//  def getClientCalls(implicit parserContext: ParserContext): String = {
//    val text     = new ListBuffer[String]
//    val funcName = toGoodName(name.split("\\.").last)
//    // generating class documentation
//    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)
//
//    val defFunc = s"def $funcName("
//    text += defFunc
//
//    text += this.parameters
//      .map { parameter =>
//        s"${parameter.getDefParameterNoVar}"
//
//      }
//      .mkString(",\n")
//    text += "): " + getRequestZioReturn
//    text += " ={\n"
//
//    text += s"val request= ${scalaRequest}("
//
//    text += parameters
//      .map { p =>
//        val pname = p.parameterName
//        s"$pname = $pname"
//      }
//      .mkString(",\n")
//    text += ")\n"
//
//    text += "\n"
//    text += s"$funcName(request)\n"
//    text += "\n  }\n\n"
//
//    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= $clientName.execute(request)\n\n"
//
//    text.mkString
//  }
//
//  def getClientNativeCalls(implicit parserContext: ParserContext): String = {
//    val text     = new ListBuffer[String]
//    val funcName = toGoodName(name.split("\\.").last)
//    // generating class documentation
//    text ++= cookedDocumentation(true, this.parameters).map(s => "  " + s)
//
//    val defFunc = s"  def $funcName("
//    text += defFunc + "\n"
//
//    val newLineFunc: String = " " * defFunc.length
//
//    text += this.parameters
//      .map { parameter =>
//        newLineFunc + s"${parameter.getDefParameterNoVar}"
//
//      }
//      .toList
//      .mkString(",\n")
//    text += "): " + getRequestZioReturn
//    text += " ={\n"
//
//    text += s"val request= new ${scalaRequest}()\n"
//
//    //    val funcCall = s"    $funcName(new $scalaRequest("
//    //    var size = funcCall.length
//
//    text += parameters
//      .map { p =>
//        val pname = p.parameterName
//        if (!p.required) {
//          if (p.default.isDefined) {
//            s"if(${pname} != ${p.getCookedDefault}})request.${pname}(${pname})"
//          } else {
//            s"${pname}.foreach{p=> request.${pname}(p) }"
//          }
//        } else {
//          s"request.${pname}(${pname})"
//        }
//      }
//      .mkString("\n")
//    text += "\n"
//
//    text += s"$funcName(request)\n"
//
//    text += "\n  }\n\n"
//
//    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= ${clientName}.execute(request)\n\n"
//
//    text.mkString
//  }

  def requestPackage: String = s"$basePackage.requests".stripSuffix(".")

  def responsePackage: String = s"$basePackage.responses".stripSuffix(".")

  def basePackage: String = s"${Constants.namespaceName}.${scope.replace("client", "")}".stripSuffix(".")

//  def getRequestZioReturn: String = s"ZIO[Any, FrameworkException, ${getRequestReturn}]"
//
//  def getRequestReturn: String =
//    if (nativeResponse.isDefined)
//      nativeResponse.get.split("""\.""").last
//    else
//      result match {
//        case Some(value) => value.scala
//        case _           => scalaResponse
//      }
//
//  def cookedDocumentation(withParameters:Boolean, parameters: List[CallParameter])(implicit parserContext: ParserContext): List[String] = {
//    val doc = new ListBuffer[String]
//    doc += "/*\n"
//    doc += " * " + this.documentation.description + "\n"
//    doc += " * For more info refers to " + this.documentation.url + "\n"
//    if(withParameters && parameters.nonEmpty){
//      doc += " * \n"
//      doc += parameters.map(_.getCookedDocumentation).mkString("", "\n", "\n")
//    }
//    doc += " */\n"
//    doc.toList
//  }

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
//  def generateRequest(implicit parserContext: ParserContext): CodeData = {
//    val doc = new ListBuffer[String]
//    val imports = new ListBuffer[String]
//        if (this.scope != "client")
//          imports += s"import ${Constants.namespaceName}.requests.ActionRequest"
//        this.extra.foreach { case (k, v) =>
//          imports += s"import ${Constants.namespaceName}.$k"
//        }
//
//    // generating class documentation
//    doc ++= cookedDocumentation(true, this.parameters)
//    val classDef =
//      doc += "\n"
//    doc += s"final case class ${scalaRequest}(\n"
//
//    val newLineFunc: String = " " * classDef.length
//
//    doc += this.parameters
//      .map { parameter =>
//        val key = ""
//        newLineFunc + s"$key${parameter.getDefParameterNoVar}"
//      }
//      .mkString(",\n") + s") extends ActionRequest[$requestBody] "
//    if(implements.nonEmpty) {
//      doc += implements.mkString("with ", "with ", "")
//      implements.foreach{
//        impl =>
//          parserContext.getImportForClassName(impl) match {
//            case Some(value) => imports += value
//            case None => println(s"Namespace for $impl not found")
//          }
//      }
//    }
//    doc += "{\n"
//    doc += s"""  def method:String="${methods.head}"\n\n"""
//    doc ++= getDefUrlPath
//    doc += "\n"
//    val qargs=getDefQueryArgs
//    doc ++= qargs._1
//    imports ++= qargs._2
//    doc += "\n"
//    if (!hasBody) {
//      doc += """  def body:Json=Json.Null"""
//      doc += "\n"
//      doc += "\n"
//    }
//    doc += "  // Custom Code On\n"
//    doc += "  // Custom Code Off\n"
//    doc += "\n"
//
//    doc += "}\n"
//    doc += "\n"
//    val filename=s"$requestPackage/$scalaRequest.scala".substring(Constants.namespaceName.length+1)
//    CodeData(code=doc.toList, imports = imports.toList, filename = filename, isPackage = false)
//  }

//  def getDefQueryArgs(implicit parserContext: ParserContext): (List[String],List[String]) = {
//    val imports=List("import scala.collection.mutable")
//    val parameters = this.parameters.filterNot(_.required).filterNot(_.scope == "uri")
//    if (parameters.nonEmpty) {
//      val text = new ListBuffer[String]
//      text += "  def queryArgs:Map[String, String] = {\n"
//      text += "    //managing parameters\n"
//      text += "    val queryArgs = new mutable.HashMap[String, String]()\n"
//      parameters.foreach { parameter =>
//        val code = parameter.toBodyCode
//        if (!code.isEmpty)
//          text += code
//      }
//      text += "    // Custom Code On\n"
//      text += "    // Custom Code Off\n"
//
//      text += "    queryArgs.toMap\n"
//      text += "  }\n"
//      (text.toList,imports)
//    } else {
//      (List("def queryArgs: Map[String, String] = Map.empty[String, String]\n"), Nil)
//    }
//
//  }
//
//  def getDefUrlPath: List[String] = {
//    val text = new ListBuffer[String]
//    if (this.path.contains("{")) {
//      text += s"""  def urlPath:String = this.makeUrl("""
//      text += path
//        .split("/")
//        .map { p =>
//          val cleanValue = p.stripPrefix("{").stripSuffix("}")
//          if (p.startsWith("{"))
//            toGoodName(cookPart(cleanValue, CommonStrings.isMultiple(url.getPartDocumentation(cleanValue))))
//          else "\"" + cookPart(p, false) + "\"" // is not a variable it cannot be multiple
//        }
//        .toList
//        .tail
//        .mkString(", ")
//      text += """)"""
//    } else {
//      text += s"""  def urlPath = "${this.path}" """
//    }
//    text += "\n"
//    text.toList
//  }

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
