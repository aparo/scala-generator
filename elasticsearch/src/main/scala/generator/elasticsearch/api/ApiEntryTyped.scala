package generator.elasticsearch.api

import generator.ts.{CodeData, ParserContext}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import generator.elasticsearch.Constants
import generator.ts.{CodeData, ParserContext, ScalaClassMember, ScalaObjectType, SimpleType, UndefinedType, UnionType}
import generator.ts.Converters._

import scala.util.Try

case class ApiEntryTyped(
    name:                 String = "undefined",
    documentation:        APIDocumetation,
    url:                  APIURL,
    className:            String,
    scalaRequest:         String,
    scalaResponse:        String,
    scope:                String,
    basePackage:          String,
    body:                 Option[APIBody] = None,
    result:               Option[APIResult] = None,
    params:               Map[String, Parameter] = Map.empty[String, Parameter],
    nativeAction:         Option[String] = None,
    nativeRequest:        Option[String] = None,
    nativeResponse:       Option[String] = None,
    response:             Option[APIResponse] = None,
    stability:            Option[String] = None,
    path:                 String = "",
    extra:                Map[String, String] = Map.empty[String, String],
    implicits:            List[String] = List.empty[String],
    parameters:           List[CallParameter] = List.empty[CallParameter],
    implements:           List[String] = List.empty[String],
    requestBody:          String = "Json",
    hasBody:              Boolean = false,
    extraClassCodes:      List[CodeData] = List.empty[CodeData],
)(implicit parserContext: ParserContext) {
  import ApiEntryTyped._

  def methods: List[String] = url.paths.flatMap(_.methods)

  def requestPackage: String = name //s"$basePackage.requests".stripSuffix(".")

  def responsePackage: String = name //s"$basePackage.responses".stripSuffix(".")

  def clientName: String = if (scope == "client") "this" else "client"

  def generateRequest: CodeData = {
    val doc     = new ListBuffer[String]
    val imports = new ListBuffer[String]
    if (this.scope != "client")
      imports += s"import ${Constants.namespaceName}.requests.ActionRequest"
    this.extra.foreach { case (k, v) =>
      imports += s"import ${Constants.namespaceName}.$k"
    }
    imports += s"import ${Constants.namespaceName}.common._"

    // generating class documentation
    doc ++= cookedDocumentation(true, this.parameters)
    val classDef =
      doc += "\n"
    doc += s"final case class ${scalaRequest}(\n"

    val newLineFunc: String = " " * classDef.length

    doc += this.parameters
      .map { parameter =>
        newLineFunc + s"${parameter.getDefParameterNoVar}"
      }
      .mkString(",\n") + s") extends ActionRequest[$requestBody] "
    if (implements.nonEmpty) {
      doc += implements.mkString("with ", "with ", "")
      implements.foreach { impl =>
        parserContext.getImportForClassName(impl) match {
          case Some(value) => imports += value
          case None        => println(s"Namespace for $impl not found")
        }
      }
    }
    doc += "{\n"
    doc += s"""  def method:String="${methods.head}"\n\n"""
    doc ++= getDefUrlPath
    doc += "\n"
    val qargs = getDefQueryArgs
    doc ++= qargs._1
    imports ++= qargs._2
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
    val filename = s"$requestPackage/$scalaRequest.scala" //.substring(Constants.namespaceName.length + 1)
    CodeData(code = doc.toList, imports = imports.toList, filename = filename, isPackage = false)
  }

  def generateResponse: CodeData = {
    val imports = new ListBuffer[String]

    val text = new ListBuffer[String]
    // generating class documentation
    imports += s"import $basePackage._"
    imports += s"import zio.json._"
    imports += s"import zio.json.ast._"
    imports += s"import ${Constants.namespaceName}.common._"

    //    text += s"import com.github.plokhotnyuk.jsoniter_scala.core._\n"
    //    text += s"import com.github.plokhotnyuk.jsoniter_scala.macros._\n"
    imports ++= extra.map(v => s"import ${Constants.namespaceName}.${v._1}")

    var implements: List[String] = List.empty[String]

    val para2 = new ListBuffer[(String, CallParameter)]

    // we add other request parameters taken from typed parsed data
    // retrieve typed data
    parserContext.getNamespaceResponseClass(name).foreach { rqTyped =>
      implements =
        if (rqTyped.implements.isEmpty) Nil
        else {
          rqTyped.implements.map(_.toScalaType)
        }
      // called all derived typed parameters

      val recursiveFields = new ListBuffer[(String, ScalaClassMember)]
      rqTyped.members.find(_.name == "body").foreach { c =>
        //             recursiveFields ++=c.members.map(v => rqTyped.name -> v ).filter(v => v._1!=v._2.name)
        c.typ match {
          case Some(value) =>
            recursiveFields ++= parserContext.getRecursiveMembersWithParentName(value)
          case None =>
        }
      }
      para2 ++= recursiveFields.map(p =>
        p._1 -> toCallParameter(
          if (parserContext.MAP_VARIABLES.contains(p._1)) p._2.copy(name = parserContext.MAP_VARIABLES(p._1)) else p._2,
        ),
      )
    }

    val parameters: List[(String, CallParameter)] =
      para2.toList.filter(_._2.required) ++ para2.filterNot(_._2.required).sortBy(_._2.name.dropWhile(_ == '_'))
    var filename  = s"$responsePackage/$scalaResponse.scala"//.substring(Constants.namespaceName.length + 1)
    var isPackage = false
    if (parameters.length == 1 && parameters.head._1 == "Map") {
      // case simple type
      val code = parameters.head._2.toQueryParam
      text += s"type ${scalaResponse}=$code\n"
      // this must go in a package
      filename = s"$responsePackage/package.scala"//.substring(Constants.namespaceName.length + 1)
      isPackage = true
    } else {
      // case object that need to manage the data
      text ++= cookedDocumentation(true, parameters.map(_._2)).map(s => "  " + s)

      text += s"final case class ${scalaResponse}(\n"

      val newLineFunc: String = "  "

      text += parameters
        .map { case (className, parameter) =>
          val key = ""
          val value=Try(s"$key${parameter.getParameterWithDefault(className)}").toOption.getOrElse(s"$key${parameter.getParameterNoDefault(className)}")
          newLineFunc + value
        }
        .toList
        .mkString(",\n")
      if (implements.nonEmpty) {
        implements.headOption.foreach { cls =>
          text += s") extends $cls "
        }
        val remains = implements.tail
        if (remains.nonEmpty) {
          text += s"${implements.mkString("with ", "with ", "")}"
        }
        text += "{\n"

      } else {
        text += s") {\n"
      }
      text += "}\n"

      text += s"object $scalaResponse{\n"
      text += s"   implicit val jsonCodec: JsonCodec[$scalaResponse] = DeriveJsonCodec.gen[$scalaResponse]"
      text += "}\n"
    }

    CodeData(code = text.toList, imports = imports.toList, filename = filename, isPackage = isPackage)
  }

  def getDefQueryArgs: (List[String], List[String]) = {
    val imports    = List("import scala.collection.mutable")
    val parameters = this.parameters.filterNot(_.required).filterNot(_.scope == "uri")
    if (parameters.nonEmpty) {
      val text = new ListBuffer[String]
      text += "  def queryArgs:Map[String, String] = {\n"
      text += "    //managing parameters\n"
      text += "    val queryArgs = new mutable.HashMap[String, String]()\n"
      parameters.foreach { parameter =>
        val code = parameter.toBodyCode
        if (code.nonEmpty)
          text += code
      }
      text += "    // Custom Code On\n"
      text += "    // Custom Code Off\n"

      text += "    queryArgs.toMap\n"
      text += "  }\n"
      (text.toList, imports)
    } else {
      (List("def queryArgs: Map[String, String] = Map.empty[String, String]\n"), Nil)
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

    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= $clientName.execute[$requestBody,$scalaResponse](request)\n\n"

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

  def getRequestZioReturn: String = s"ZIO[Any, FrameworkException, ${getRequestReturn}]"

  def getRequestReturn: String =
    if (nativeResponse.isDefined)
      nativeResponse.get.split("""\.""").last
    else
      result match {
        case Some(value) => value.scala
        case _           => scalaResponse
      }

  def cookedDocumentation(withParameters: Boolean, parameters: List[CallParameter]): List[String] = {
    val doc = new ListBuffer[String]
    doc += "/*\n"
    doc += " * " + this.documentation.description + "\n"
    doc += " * For more info refers to " + this.documentation.url + "\n"
    if (withParameters && parameters.nonEmpty) {
      doc += " * \n"
      doc += parameters.map(_.getCookedDocumentation).mkString("", "\n", "\n")
    }
    doc += " */\n"
    doc.toList
  }

}

object ApiEntryTyped {
  def parse(apiEntry: APIEntry)(implicit parserContext: ParserContext): ApiEntryTyped = {
    val extra       = new mutable.HashMap[String, String]
    val implicits   = new ListBuffer[String]
    val parameters  = new ListBuffer[CallParameter]
    var requestBody = "Json"
    val className: String = apiEntry.className

    val scalaRequest:    String         = apiEntry.scalaRequest
    val scalaResponse:   String         = apiEntry.scalaResponse
    var extraClassCodes: List[CodeData] = List.empty[CodeData]
    val result = apiEntry.result
    val scope  = apiEntry.scope
    val basePackage: String       = s"${Constants.namespaceName}.${scope.replace("client", "")}".stripSuffix(".")
    var implements:  List[String] = List.empty[String]
    var hasBody=false

    // fix names


    apiEntry.url.params.foreach { p =>
      extra ++= p._2.getEnum(p._1)
    }

    // add required in url paths
    val parts = """\{(.*?)\}""".r

    var path: String = apiEntry.url.validPaths.map(_.path).head
    var requiredPaths = parts.findAllMatchIn(path).map(_.group(1)).toList
    val partRequired  = new mutable.HashMap[String, Boolean]
    requiredPaths.foreach { p =>
      partRequired += p -> true
    }
    apiEntry.url.validPaths.map(_.path).foreach {
      case s: String if s.contains("{") =>
        if (s.length > path.length) {
          requiredPaths = parts.findAllMatchIn(s).map(_.group(1)).toList
          requiredPaths.foreach { p =>
            if (!partRequired.contains(p)) partRequired += p -> false
          }
          path = s
        }
      case _ =>
    }
    val requiredPathsCount = requiredPaths.count(p => partRequired(p))
    val bodyType = apiEntry.body match {
      case None => "Json"
      case Some(b) =>
        hasBody=true
        b.serialize match {
          case ""     => "Json"
          case "bulk" => "list"
        }
    }

    apiEntry.params.foreach { case (name, param) =>
      parameters += param.toCallParameter(name)
    }

    var bodyDone = false
    if (requiredPaths.nonEmpty) {
      if (requiredPathsCount == 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = apiEntry.body.get.required,
          scope = "body",
        )
        bodyDone = true
      }
      parameters ++= requiredPaths.map { p =>
        val partDocumentation = apiEntry.url.getPartDocumentation(p)
        val multiple          = CommonStrings.isMultiple(partDocumentation)
        CallParameter(
          cookPart(p, multiple),
          "string",
          description = partDocumentation,
          required = partRequired(p),
          scope = "uri",
        )
      }

      if (requiredPathsCount > 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = apiEntry.body.get.required,
          scope = "body",
        )
        bodyDone = true
        hasBody=true
      }
    }

    if (!bodyDone && hasBody) {
      parameters += CallParameter(
        "body",
        bodyType,
        "body the body of the call",
        required = apiEntry.body.get.required,
        scope = "body",
      )
      bodyDone = true
    }
    parameters ++= apiEntry.url.params
      .filterNot { case (name, _) =>
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
//    this.extra = extra.toMap
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
    parserContext.getNamespaceRequestClass(apiEntry.name).foreach { rqTyped =>
      rqTyped.members.foreach {
        case sc if sc.name == "path_parts" =>
          sc.typ.getOrElse(UndefinedType) match {
            case UndefinedType => ()
            case UnionType(_, _) => ()
            case st: SimpleType =>
              if (!para2.exists(_.name == st.name))
                para2 += CallParameter(
                  name = st.name,
                  `type` = st.toScalaType,
                  description = "",
                  required = st.isRequired,
                  scope = "uri",
                )
            case ScalaObjectType(_, _, _, members, _, _) =>
              members.foreach { st1 =>
                st1.members.foreach { st =>
                  if (!para2.exists(_.name == st.name))
                    para2 += toCallParameter(st).copy(scope = "uri")
                }
              }
          }
        case sc if sc.name == "query_parameters" =>
          sc.typ.getOrElse(UndefinedType) match {
            case UndefinedType => ()
            case UnionType(_, _) => ()
            case st: SimpleType =>
              if (!para2.exists(_.name == st.name))
                para2 += CallParameter(
                  name = st.name,
                  `type` = st.toScalaType,
                  description = "",
                  required = st.isRequired,
                  scope = "query",
                )
            case ScalaObjectType(_, _, _, members, _, _) =>
              members.foreach { st1 =>
                st1.members.foreach { st =>
                  if (!para2.exists(_.name == st.name))
                    para2 += toCallParameter(st).copy(scope = "query")
                }
              }
          }
        case sc if sc.name == "body" =>
          parserContext.getTypedBody(scalaRequest) match {
            case Some(tuple) =>
              val cls = tuple._1
              val desc = tuple._2
              // we update body
              requestBody = cls
              hasBody = true
              para2.find(_.name == "body") match {
                case Some(value) =>
                  para2 -= value
                  para2 += value.copy(`type` = cls, description = desc)
                case None =>
                  para2 += CallParameter(
                    "body",
                    cls,
                    desc,
                    required = apiEntry.body.get.required,
                    scope = "body",
                  )
              }

            case None =>
              // we create a request body
              val rqBody = scalaRequest + "Body"
              sc.typ.foreach {
                case st: ScalaObjectType =>
                  val cd = st
                    .copy(name = rqBody, namespace = apiEntry.requestPackage.replace("zio.elasticsearch.", ""))
                    .toCode
                  if (cd != CodeData.empty) {
                    extraClassCodes ::= cd.copy(imports = List(s"import $basePackage._") ::: cd.imports)
                    requestBody = rqBody
                    para2.find(_.name == "body") match {
                      case Some(value) =>
                        para2 -= value
                        para2 += value.copy(`type` = rqBody)
                      case None =>
                        para2 += CallParameter(
                          "body",
                          rqBody,
                          s"a $rqBody",
                          required = apiEntry.body.get.required,
                          scope = "body",
                        )
                    }

                  }
                case st: SimpleType =>
                  requestBody = rqBody
                  para2.find(_.name == "body") match {
                    case Some(value) =>
                      para2 -= value
                      para2 += value.copy(`type` = st.name)
                    case None =>
                      para2 += CallParameter(
                        "body",
                        st.name,
                        s"a $rqBody",
                        required = apiEntry.body.get.required,
                        scope = "body",
                      )
                  }
              }
          }
      }
      implements =
        if (rqTyped.implements.isEmpty) Nil
        else {
          rqTyped.implements.map(_.toScalaType)
        }
      // called all derived typed parameters
      val recursiveFields = rqTyped.implements.flatMap(impl => parserContext.getRecursiveMembers(impl))
    recursiveFields.foreach{p =>
      if (!para2.exists(_.name == p.name))
        para2 += toCallParameter(p)

    }
    }

    ApiEntryTyped(
      name = apiEntry.name,
      documentation = apiEntry.documentation,
      url = apiEntry.url,
      className = className,
      scalaRequest = scalaRequest,
      scalaResponse = scalaResponse,
      scope = scope,
      basePackage = basePackage,
      body = apiEntry.body,
      result = result,
      params = apiEntry.params,
      nativeAction = apiEntry.nativeAction,
      nativeRequest = apiEntry.nativeRequest,
      nativeResponse = apiEntry.nativeResponse,
      response = apiEntry.response,
      stability = apiEntry.stability,
      path = path,
      extra = extra.toMap,
      implicits = implicits.toList,
      parameters = para2.toList.filter(_.required) ++ para2.filterNot(_.required).sortBy(_.name.dropWhile(_ == '_')),
      implements = implements,
      requestBody = requestBody,
      hasBody = hasBody,
      extraClassCodes = extraClassCodes.toList,
    )
  }

  def toCallParameter(p: ScalaClassMember): CallParameter = CallParameter(
    name = p.name,
    `type` = p.typ
      .map { typ =>
        var result = typ.toScalaType
        if (result.startsWith("Option[")) result = result.substring("Option[".length, result.length - 1)
        result
      }
      .getOrElse("Json"),
    description = p.comments.rawCs.map(fixCommentTyped).filter(_.nonEmpty).mkString("", "", "\n"),
    required = !p.isOptional,
  )

  def cookPart(name: String, multiple: Boolean) = name match {
    case "type"  => if (multiple) "docTypes" else "docType"
    case "index" => if (multiple) "indices" else "index"
    case v       => v
  }

  def fixCommentTyped(str: String): String = {
    var result = str
    if (result.startsWith("/**")) result = result.substring(3).trim
    result = result.dropWhile(_.isSpaceChar)
    if (result.startsWith("* ")) result = result.substring(2)
    result = result.dropWhile(_.isSpaceChar)
    if (result.contains("*/")) result = result.replace("*/", "").trim
    if (result.startsWith("@")) result = "\n" + result

    result
  }

  def toGoodName(value: String): String = value match {
    case value: String if value.contains("_") =>
      val pos = value.indexOf('_')
      value.take(pos) + toGoodName(value.drop(pos + 1).capitalize)
    case _ => value
  }

}
