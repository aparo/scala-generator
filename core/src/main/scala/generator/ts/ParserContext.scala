package generator.ts

import org.scalablytyped.converter.internal.ts._
import org.scalablytyped.converter.internal._
import org.scalablytyped.converter.internal.ts.TsExportee.Names
import org.scalablytyped.converter.internal.ts.TsExportee.Star
import org.scalablytyped.converter.internal.ts.TsExportee.Tree
import scala.collection.mutable
class ParserContext() {
  val scalaClasses = new mutable.HashMap[String, ScalaClass]()
  val MAP_CLASSES: Map[String, String] = Map("string" -> "String", "Dictionary" -> "Map")

  def parse(filename:InFile, namespace:String, tsFile: TsParsedFile): Unit = {
    tsFile.members.foreach { entity =>
      parse(namespace, entity)(filename)
    }
    ()
  }

  def parse(namespace:String, entity: TsContainerOrDecl)(implicit inFile: InFile): Unit =
    entity match {
      case t: TsParsedFile => parse(inFile, namespace, t)
      case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) =>
      case TsDeclModule(comments, declared, name, members, codePath, jsLocation) =>
      case TsAugmentedModule(comments, name, members, codePath, jsLocation) =>
      case TsGlobal(comments, declared, members, codePath) =>
      case t: TsDeclClass =>
        val c = parse(namespace, t)
        scalaClasses += s"$namespace.${c.name}" -> c
      case t:TsDeclInterface =>
        val c = parse(namespace, t)
        scalaClasses += s"$namespace.${c.name}" -> c
      case t: TsDeclEnum => parse(t)
      case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) =>
      case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath) =>
      case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath) =>
      case t: TsImport =>
        val i = parse(t)
        println(i)
      case TsExport(comments, typeOnly, tpe, exported) =>
        exported match {
          case Names(idents, fromOpt) =>
          case Star(as, from) =>
          case Tree(decl) => parse(namespace, decl)
        }
      case TsExportAsNamespace(ident) =>
    }

  def parse(namespace: String, ts: TsDeclInterface)(implicit inFile: InFile): ScalaClass = {
    val allMembers = ts.members.toList.map(parse)
    var name = ts.name.value

    ScalaClass(
      namespace = namespace,
      name = name,
      typeParams = ts.tparams.toList.map(parse),
      comments = ts.comments,
      isAbstract = true,
      implements = ts.inheritance.toList.map(parseTsTypeRef),
      members = allMembers.collect { case t: ScalaClassMember => t },
      codePath = ts.codePath
    )
  }

  def parse(namespace:String, ts: TsDeclClass)(implicit inFile: InFile): ScalaClass = {
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
    var name=ts.name.value
    if(name=="Request") name=inFile.path.last.replace(".ts", "")
    if(name=="Response") name=inFile.path.last.replace(".ts", "")

    ScalaClass(
      namespace=namespace,
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
      case TsMemberCall(comments, level, signature) => ???
      case TsMemberIndex(comments, isReadOnly, level, indexing, valueType) =>
//        val optType=valueType.map(parse)
???
      case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
        val optType=tpe.map(parse)
        ScalaClassMember(
          name = name.value,
          comments = comments,
          level = level,
          isStatic = isStatic,
          typ = optType
        )
      case TsMemberCtor(comments, level, signature) => ???
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
      members = ts.members.toList.map(m => ScalaEnumMember(name = m.name.value, expr = m.expr, comments = m.comments)),
      jsLocation = ts.jsLocation,
      codePath = ts.codePath,
    )

  def parse(tpe: TsType): ScalaType =
    tpe match {
      case TsTypeObject(comments, members) => ScalaObjectType(
        namespace = "", name = "",
        comments=comments,
        members = members.toList.map(parse)
      )
      case TsTypeKeyOf(key) => ???
      case TsTypeRepeated(underlying) => ???
      case TsTypeExtends(tpe, ext) => ???
      case TsTypeAsserts(ident, isOpt) => ???
      case TsTypeIntersect(types) => ???
      case TsTypeFunction(signature) => ???
      case TsTypeTuple(elems) => ???
      case TsTypeConditional(pred, ifTrue, ifFalse) => ???
      case t: TsTypeRef => parseTsTypeRef(t)
      case TsTypeQuery(expr) => ???
      case TsTypeUnion(types) =>
        val res = types.toList.map(parse)
        if (types.length == 2) {
          if(res.contains(UndefinedType)) {
            res.filterNot(_ == UndefinedType).map(_.setRequired(true).setNullable(true)).head
          } else UnionType(res)
        } else UnionType(res)

      case TsTypeInfer(tparam) => ???
      case TsTypeIs(ident, tpe) => ???
      case TsTypeConstructor(isAbstract, signature) => ???
      case TsTypeThis() => ???
      case TsTypeLiteral(literal) => ???
      case TsTypeLookup(from, key) => ???
    }

  def parse(tpe: TsTypeParam): String = tpe.name.value // TODO add type bounds

  def parseTsTypeRef(typeRef: TsTypeRef): ScalaType = {
    val res = typeRef.name.parts.map(p => MAP_CLASSES.getOrElse(p.value, p.value)).mkString(".")
    if(res=="undefined"){
      UndefinedType
    } else
      SimpleType(res, typeRef.tparams.toList.map(parse))
  }

  def parse(ts: TsImport): ScalaImport = {

    val imports: List[String] = ts.imported.toList
      .flatMap(_ match {
        case TsImported.Ident(ident) => List(ident.value)
        case TsImported.Destructured(idents) =>
          idents.toList.map {
            case (ident, _) => ident.value
          }
        case TsImported.Star(asOpt) => asOpt.map(_.value).toList
      })

    val fromImports = ts.from match {
      case TsImportee.From(from) => from.value
      case TsImportee.Local(qident) => qident.parts.map(_.value).mkString(".")
      case TsImportee.Required(from) => from.value
    }

    ScalaImport(fromImports, imports)
  }

  def getClass(name:String):Option[ScalaClass]={
    scalaClasses.values.find(_.name == name)
  }
}
