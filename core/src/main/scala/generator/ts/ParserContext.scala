package generator.ts

import org.scalablytyped.converter.internal.ts._
import org.scalablytyped.converter.internal._
import org.scalablytyped.converter.internal.ts.TsExportee.Names
import org.scalablytyped.converter.internal.ts.TsExportee.Star
import org.scalablytyped.converter.internal.ts.TsExportee.Tree
import scala.collection.mutable
class ParserContext() {
  import Converters._



  val scalaClasses = new mutable.HashMap[String, ScalaClass]
  val MAP_CLASSES: Map[String, String] = Map("string" -> "String", "Dictionary" -> "Map")

  def parse(filename: InFile, namespace: String, tsFile: TsParsedFile): Unit = {
    tsFile.members.foreach { entity =>
      parse(namespace, entity)(filename)
    }
    ()
  }

  def parse(namespace: String, entity: TsContainerOrDecl)(implicit inFile: InFile): Unit =
    entity match {
      case t: TsParsedFile => parse(inFile, namespace, t)
      case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) =>
      case TsDeclModule(comments, declared, name, members, codePath, jsLocation)    =>
      case TsAugmentedModule(comments, name, members, codePath, jsLocation)         =>
      case TsGlobal(comments, declared, members, codePath)                          =>
      case t: TsDeclClass =>
        val c = parse(namespace, t)
        scalaClasses += c.id -> c
      case t: TsDeclInterface =>
        val c = parse(namespace, t)
        scalaClasses += c.id -> c
      case t: TsDeclEnum => parse(t)
      case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) =>
      case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath)      =>
      case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath)            =>
      case t: TsImport =>
        val i = parse(t)
        println(i)
      case TsExport(comments, typeOnly, tpe, exported) =>
        exported match {
          case Names(idents, fromOpt) =>
          case Star(as, from)         =>
          case Tree(decl)             => parse(namespace, decl)
        }
      case TsExportAsNamespace(ident) =>
    }

  def parse(namespace: String, ts: TsDeclInterface)(implicit inFile: InFile): ScalaClass = {
    val allMembers = ts.members.toList.map(parse)
    var name       = ts.name.value
    if (name == "Request") name = inFile.path.last.replace(".ts", "")
    if (name == "Response") name = inFile.path.last.replace(".ts", "")
    val realNamespace = fixNamespace(namespace)

    ScalaClass(
      namespace = realNamespace,
      name = name,
      typeParams = ts.tparams.toList.map(parse),
      comments = ts.comments,
      isAbstract = true,
      implements = ts.inheritance.toList.map(parseTsTypeRef),
      members = allMembers.collect { case t: ScalaClassMember => t },
      codePath = ts.codePath,
    )
  }

  def fixNamespace(namespace:String):String={
    var realNamespace = namespace
    if (realNamespace.startsWith("_global")) {
      realNamespace = realNamespace.replace("_global", "common")
    } else if (realNamespace.startsWith("_types")) {
      realNamespace = realNamespace.replace("_types", "common")
    } else {
      var tokens = realNamespace.split('.')
      List("_types", "common").foreach {
        sep =>
          if (tokens.contains(sep)) tokens = tokens.takeWhile(_ != sep)
      }
      realNamespace = tokens.mkString(".")
    }
    if (realNamespace.isEmpty) realNamespace = "global"

//    if(realNamespace.startsWith("common.")){
//      val tokens = realNamespace.split('.')
//      if(tokens.length>1){
//        val newTokens=List("global")++tokens.tail
//        realNamespace = newTokens.mkString(".")
//      }
//    }

    realNamespace
  }
  def parse(namespace: String, ts: TsDeclClass)(implicit inFile: InFile): ScalaClass = {
    // comments,
    // declared,
    // isAbstract,
    // name,
    // tparams,
    // parent,
    // implements,
    // members,
    // jsLocation,
    // codePath,

    val allMembers = ts.members.toList.map(parse)
    var name       = ts.name.value
    name match {
      case "Request" =>
        name = inFile.path.last.replace(".ts", "")
      case "Response" =>
        name = inFile.path.last.replace(".ts", "")
      case _ =>
    }
    val realNamespace=fixNamespace(namespace)

    ScalaClass(
      namespace = realNamespace,
      name = name,
      typeParams = ts.tparams.toList.map(parse),
      comments = ts.comments,
      isAbstract = ts.isAbstract,
      parent = ts.parent.map(parseTsTypeRef),
      implements = ts.implements.toList.map(parseTsTypeRef),
      members = allMembers.collect { case t: ScalaClassMember => t },
      jsLocation = Some(ts.jsLocation),
      codePath = ts.codePath,
    )
  }

  def parse(member: TsMember): ScalaCode =
    member match {
      case TsMemberFunction(comments, level, name, methodType, signature, isStatic, isReadOnly) => ???
      case TsMemberCall(comments, level, signature)                                             => ???
      case TsMemberIndex(comments, isReadOnly, level, indexing, valueType)                      =>
//        val optType=valueType.map(parse)
        ???
      case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
        val optType = tpe.map(parse)
        ScalaClassMember(
          name = name.value,
          comments = comments,
          level = level,
          isStatic = isStatic,
          typ = optType,
        )
      case TsMemberCtor(comments, level, signature)                                      => ???
      case TsMemberTypeMapped(comments, level, readonly, key, from, as, optionalize, to) => ???
    }

  def parse(ts: TsDeclEnum): ScalaCode =
    ScalaEnum(
      name = ts.name.value,
      comments = ts.comments,
      declared = ts.declared,
      isConst = ts.isConst,
      isValue = ts.isValue,
      exportedFrom = ts.exportedFrom.map(parseTsTypeRef),
      values = ts.members.toList.map(m => ScalaEnumMember(name = m.name.value, expr = m.expr, comments = m.comments)),
      jsLocation = ts.jsLocation,
      codePath = ts.codePath,
    )

  def parse(tpe: TsType): ScalaType =
    tpe match {
      case TsTypeObject(comments, members) =>
        ScalaObjectType(
          namespace = "",
          name = "",
          comments = comments,
          members = members.toList.map(parse),
        )
      case TsTypeKeyOf(key)                         => ???
      case TsTypeRepeated(underlying)               => ???
      case TsTypeExtends(tpe, ext)                  => ???
      case TsTypeAsserts(ident, isOpt)              => ???
      case TsTypeIntersect(types)                   => ???
      case TsTypeFunction(signature)                => ???
      case TsTypeTuple(elems)                       =>
        TupleType(elems.toList.map(parse))
      case TsTypeConditional(pred, ifTrue, ifFalse) => ???
      case t: TsTypeRef => parseTsTypeRef(t)
      case TsTypeQuery(expr) => ???
      case TsTypeUnion(types) =>
        var res = types.toList.map(parse)
        var refreshRequired=false
        if(res.contains(UndefinedType)) {

          res=res.filterNot(_ == UndefinedType)//.map(_.setRequired(true).setNullable(true))
          refreshRequired=true
        }
        val toScala=res.map(_.toScalaType).sortBy(_.length)
        if(toScala.contains(s"Chunk[${toScala.head}]")){
          res=res.filter(_.toScalaType.startsWith("Chunk["))
        }
//        if(toScala.contains("String") && toScala.contains("Boolean"))

        if(refreshRequired) res=res.map(_.setRequired(false).setNullable(true))
        if (res.length == 1) {
          res.head
        } else UnionType(res)

      case TsTypeInfer(tparam)                      => ???
      case TsTypeIs(ident, tpe)                     => ???
      case TsTypeConstructor(isAbstract, signature) => ???
      case TsTypeThis()                             => ???
      case TsTypeLiteral(literal)                   => LiteralType(literal.asString)
      case TsTypeLookup(from, key)                  => ???
    }

  def parse(tpe: TsTypeParam): String = tpe.name.value // TODO add type bounds

  def parse(item:TsTupleElement):ScalaType=SimpleType(MAP_CLASSES.getOrElse(item.tpe.asString, item.tpe.asString))

  def parseTsTypeRef(typeRef: TsTypeRef): ScalaType = {
    val res = typeRef.name.parts.map(p => MAP_CLASSES.getOrElse(p.value, p.value)).mkString(".")
    if (res == "undefined" || res=="null") {
      UndefinedType
    } else
      SimpleType(res, typeRef.tparams.toList.map(parse))
  }

  def parse(ts: TsImport): ScalaImport = {

    val imports: List[String] = ts.imported.toList
      .flatMap(_ match {
        case TsImported.Ident(ident) => List(ident.value)
        case TsImported.Destructured(idents) =>
          idents.toList.map { case (ident, _) =>
            ident.value
          }
        case TsImported.Star(asOpt) => asOpt.map(_.value).toList
      })

    val fromImports = ts.from match {
      case TsImportee.From(from)     => from.value
      case TsImportee.Local(qident)  => qident.parts.map(_.value).mkString(".")
      case TsImportee.Required(from) => from.value
    }

    ScalaImport(fromImports, imports)
  }

  def getClass(name: String): Option[ScalaClass] =
    scalaClasses.values.find(_.name == name)

  def getNamespaceClass(name: String): Option[ScalaClass] =
    scalaClasses.get(name)

  def getNamespaceResponseClass(name: String):Option[ScalaClass] = scalaClasses.find{
    case (nameCls, _) => nameCls.startsWith(name) && nameCls.endsWith("Response")
  }.map(_._2)


  def getNamespaceRequestClass(name: String): Option[ScalaClass] = scalaClasses.find {
    case (nameCls, _) => nameCls.startsWith(name) && nameCls.endsWith("Request")
  }.map(_._2)
  def getImportForClassName(name: String): Option[String] =
    name match {
      case "RequestBase" => Some(s"import zio.elasticsearch.common.$name")
      case _ =>
        val withCommon = s"common.$name"
        if (scalaClasses.contains(withCommon)) {
          val ns = scalaClasses(withCommon).namespace
          Some(s"import $ns.$name")
        } else None
    }

  def getRecursiveMembers(typ: ScalaType): List[ScalaClassMember] =
    getClass(typ.toScalaType) match {
      case Some(value) => value.members ++ value.implements.flatMap(getRecursiveMembers)
      case None        => Nil
    }

  def getRecursiveMembersWithParentName(typ: ScalaType): List[(String, ScalaClassMember)] =
    (typ match {
      case UndefinedType   => List.empty[(String, ScalaClassMember)]
      case UnionType(_, _) => List.empty[(String, ScalaClassMember)]
      case t: SimpleType =>
        if (typ.toScalaType != t.name)
          List(
            typ.toScalaType -> ScalaClassMember(
              name = t.name,
              level = TsProtectionLevel.Default,
              typ = Some(t),
              comments = Comments(None),
              isStatic = false,
            ),
          )
        else List.empty[(String, ScalaClassMember)]
      case t: ScalaObjectType =>
        t.members.flatMap(_.members).filter(p => typ.toScalaType != p.name).map(v => typ.toScalaType -> v)
    }) ++ (getClass(typ.toScalaType) match {
      case Some(value) =>
        value.members.map(v => typ.toScalaType -> v) ++ value.implements.flatMap(getRecursiveMembersWithParentName)
      case None => Nil
    })

  lazy val MAP_DEFAULTS = Map(
    ("", "acknowledged") -> "true",
  )

  def getDefaultParameter(clsName: String, name: String, typ: String): String =
    if (MAP_DEFAULTS.contains(clsName -> name)) {
      MAP_DEFAULTS(clsName -> name)
    } else {
      typ match {
        case "Boolean" => "true"
        case s: String if s.startsWith("Chunk[") => "Chunk.empty" + s.substring("Chunk".length)
        case s: String if s.startsWith("Map[")   => "Map.empty" + s.substring("Map".length)
        case s: String if s.startsWith("List[")  => "Nil"
      }
    }

  lazy val MAP_VARIABLES = Map(
    "Map[String,Pipeline]" -> "pipelines",
  )

  def getVariableName(name: String): String = {
    val result=MAP_VARIABLES.getOrElse(name, name)
    result.toCamel
  }


  lazy val REQUEST_TYPED_BODY = Map(
    "PutPipelineRequest" -> ("Pipeline" -> "a Pipeline"),
  )

  def getTypedBody(scalaRequest: String): Option[(String, String)] =
    REQUEST_TYPED_BODY.get(scalaRequest)
}
