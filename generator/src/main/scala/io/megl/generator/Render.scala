// package io.megl.generator

// import org.scalablytyped.converter.internal.ts._
// import org.scalablytyped.converter.internal._
// import org.scalablytyped.converter.internal.ts.TsExportee.Names
// import org.scalablytyped.converter.internal.ts.TsExportee.Star
// import org.scalablytyped.converter.internal.ts.TsExportee.Tree
// import scala.collection.mutable
// object Render {
//   val MAP_CLASSES: Map[String, String] = Map("string" -> "String", "Dictionary" -> "Map")
//   class ParserContext(){
//     val scalaClass=new mutable.ListBuffer[ScalaClass]()
//   }

//   sealed trait ScalaCode
//   case class ScalaImport(from:               String, items: List[String]) extends ScalaCode
//   case class ScalaClassConstructor(comments: Comments) extends ScalaCode
// //   case class ScalaTypeAlias(comments: Comments, name:String) extends ScalaCode
//   case class ScalaClassMember(
//       name:       String,
//       level:      TsProtectionLevel,
//       typ:        Option[String] = None,
//       comments:   Comments,
//       isStatic:   Boolean = false,
//       isReadOnly: Boolean = false,
//   ) extends ScalaCode
//   case class ScalaClassMethod(name: String, level: TsProtectionLevel, comments: Comments) extends ScalaCode
//   case class ScalaClass(
//       name:       String,
//       typeParams: List[String] = Nil,
//       comments:   Comments,
//       isAbstract: Boolean = false,
//       parent:     Option[String] = None,
//       implements: List[String] = Nil,
//       members:    List[ScalaClassMember] = Nil,
//       methods:    List[ScalaClassMethod] = Nil,
//       jsLocation: JsLocation,
//       codePath:   CodePath,
//   ) extends ScalaCode
//   case class ScalaEnumMember(name: String, expr: Option[TsExpr] = None, comments: Comments) extends ScalaCode
//   case class ScalaEnum(
//       name:         String,
//       comments:     Comments,
//       declared:     Boolean = false,
//       isConst:      Boolean = false,
//       isValue:      Boolean = false,
//       exportedFrom: Option[String] = None,
//       members:      List[ScalaEnumMember] = Nil,
//       jsLocation:   JsLocation,
//       codePath:     CodePath,
//   ) extends ScalaCode

//   def parse(tsFile: TsParsedFile): Unit = {
//     tsFile.members.foreach { entity =>
//       parse(entity)
//     }
//     ()
//   }

//   def parse(entity: TsContainerOrDecl): Unit =
//     entity match {
//       case t: TsParsedFile => parse(t)
//       case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) =>
//       case TsDeclModule(comments, declared, name, members, codePath, jsLocation)    =>
//       case TsAugmentedModule(comments, name, members, codePath, jsLocation)         =>
//       case TsGlobal(comments, declared, members, codePath)                          =>
//       case t: TsDeclClass =>
//         val c = parse(t)
//         println(c)
//       case TsDeclInterface(comments, declared, name, tparams, inheritance, members, codePath) =>
//       case t: TsDeclEnum => parse(t)
//       case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) =>
//       case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath)      =>
//       case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath)            =>
//       case t: TsImport =>
//         val i = parse(t)
//         println(i)
//       case TsExport(comments, typeOnly, tpe, exported) =>
//         exported match {
//           case Names(idents, fromOpt) =>
//           case Star(as, from)         =>
//           case Tree(decl)             => parse(decl)
//         }
//       case TsExportAsNamespace(ident) =>
//     }

//   def parse(ts: TsDeclClass): ScalaClass = {
//     // comments,
//     // declared,
//     // isAbstract,
//     // name,
//     // tparams,
//     // parent,
//     // implements,
//     // members,
//     // jsLocation,
//     // codePath,
//     val allMembers = ts.members.toList.map(parse)

//     ScalaClass(
//       name       = ts.name.value,
//       typeParams = ts.tparams.toList.map(parse),
//       comments   = ts.comments,
//       isAbstract = ts.isAbstract,
//       parent     = ts.parent.map(parse),
//       implements = ts.implements.toList.map(parse),
//       members    = allMembers.collect { case t: ScalaClassMember => t },
//       jsLocation = ts.jsLocation,
//       codePath   = ts.codePath,
//     )
//   }

//   def parse(member: TsMember): ScalaCode =
//     member match {
//       case TsMemberFunction(comments, level, name, methodType, signature, isStatic, isReadOnly) => ???
//       case TsMemberCall(comments, level, signature)                                             => ???
//       case TsMemberIndex(comments, isReadOnly, level, indexing, valueType)                      => ???
//       case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
//         ScalaClassMember(
//           name       = name.value,
//           comments   = comments,
//           level      = level,
//           isStatic   = isStatic,
//           isReadOnly = isReadOnly,
//           typ        = tpe.map(parse),
//         )
//       case TsMemberCtor(comments, level, signature)                                      => ???
//       case TsMemberTypeMapped(comments, level, readonly, key, from, as, optionalize, to) => ???
//     }

//   def parse(ts: TsDeclEnum): ScalaCode =
//     ScalaEnum(
//       name         = ts.name.value,
//       comments     = ts.comments,
//       declared     = ts.declared,
//       isConst      = ts.isConst,
//       isValue      = ts.isValue,
//       exportedFrom = ts.exportedFrom.map(parse),
//       members      = ts.members.toList.map(m => ScalaEnumMember(name = m.name.value, expr = m.expr, comments = m.comments)),
//       jsLocation   = ts.jsLocation,
//       codePath     = ts.codePath,
//     )

//   def parse(tpe: TsType): String =
//     tpe match {
//       case TsTypeObject(comments, members)          => ???
//       case TsTypeKeyOf(key)                         => ???
//       case TsTypeRepeated(underlying)               => ???
//       case TsTypeExtends(tpe, ext)                  => ???
//       case TsTypeAsserts(ident, isOpt)              => ???
//       case TsTypeIntersect(types)                   => ???
//       case TsTypeFunction(signature)                => ???
//       case TsTypeTuple(elems)                       => ???
//       case TsTypeConditional(pred, ifTrue, ifFalse) => ???
//       case t: TsTypeRef => parse(t)
//       case TsTypeQuery(expr) => ???
//       case TsTypeUnion(types) =>
//         val res = types.toList.map(parse).mkString("|")
//         if (types.length == 2) {
//           if (res.endsWith("|undefined")) {
//             "Option[" + res.replace("|undefined", "") + "]"
//           } else res
//         } else res

//       case TsTypeInfer(tparam)                      => ???
//       case TsTypeIs(ident, tpe)                     => ???
//       case TsTypeConstructor(isAbstract, signature) => ???
//       case TsTypeThis()                             => ???
//       case TsTypeLiteral(literal)                   => ???
//       case TsTypeLookup(from, key)                  => ???
//     }

//   def parse(tpe: TsTypeParam): String = tpe.name.value // TODO add type bounds

//   def parse(typeRef: TsTypeRef): String = {
//     val res = typeRef.name.parts.map(p => MAP_CLASSES.getOrElse(p.value, p.value)).mkString(".")

//     if (typeRef.tparams.nonEmpty) {
//       res + "[" + typeRef.tparams.map(parse).mkString(", ") + "]"
//     } else res
//   }

//   def parse(ts: TsImport): ScalaImport = {

//     val imports: List[String] = ts.imported.toList
//       .flatMap(_ match {
//         case TsImported.Ident(ident) => List(ident.value)
//         case TsImported.Destructured(idents) =>
//           idents.toList.map {
//             case (ident, _) => ident.value
//           }
//         case TsImported.Star(asOpt) => asOpt.map(_.value).toList
//       })

//     val fromImports = ts.from match {
//       case TsImportee.From(from)     => from.value
//       case TsImportee.Local(qident)  => qident.parts.map(_.value).mkString(".")
//       case TsImportee.Required(from) => from.value
//     }

//     ScalaImport(fromImports, imports)
//   }
// }
