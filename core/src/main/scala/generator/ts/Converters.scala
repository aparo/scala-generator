package generator.ts

import com.google.common.base.CaseFormat

import scala.collection.mutable.ListBuffer

case class CodeData(code:List[String]=Nil, imports:List[String]=Nil, filename:Option[String]=None)
object CodeData {
  lazy val empty=CodeData(Nil, Nil)
}
object Converters {
  lazy val SKIP_GENERATION = Set(("ingest", "ProcessorContainer"))
  lazy val ABSTRACT_CLASSES = Set(("ingest", "ProcessorBase"))
  lazy val SEALED_CLASSES = Set(("ingest", "ProcessorBase"))
  lazy val REMAP_CLASSES = Map(("ingest", "ProcessorBase") -> "Processor",
    ("ingest", "ProcessorContainer") -> "Processor")
  lazy val DISCRIMINATOR_END = List("Processor")
  lazy val TYPE_MAPPING = Map("boolean" -> "Boolean",
    "Array" -> "Chunk",
    "Field" -> "String",
    "string" -> "String",
    "boolean" -> "Boolean",
    "integer" -> "Int",
    "double" -> "Double",
    "Id" -> "String",
    "Name" -> "String",
    "UserDefinedValue" -> "Json",
    "ProcessorContainer" -> "Processor"
  )

  implicit class ClassConverter(scalaClass: ScalaClass) {
    def getParent(implicit parserContext: ParserContext):Option[ScalaClass]={
      scalaClass.parent.flatMap(p => parserContext.getClass(p.toScalaType))
    }

    def isParentSealed(implicit parserContext: ParserContext):Boolean=getParent.exists(_.isSealed)

    def getDiscriminator:String={
      val name=scalaClass.name.fixName
      DISCRIMINATOR_END.find(p => name.endsWith(p)) match {
        case Some(value) => name.dropRight(value.length).toSnake
        case None => name.toSnake
      }
    }
    def isSealed:Boolean={
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      SEALED_CLASSES.contains(tupleNs)
    }
    def toCode(implicit parserContext: ParserContext): CodeData = {
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      if (!SKIP_GENERATION.contains(tupleNs)) {
        val code = new ListBuffer[String]
        val imports = new ListBuffer[String]
        imports += "import zio.json._"
        val name = REMAP_CLASSES.getOrElse(tupleNs, scalaClass.name)
        var filename=s"${scalaClass.namespace}/$name.scala"
        var addJsonEncoder=false
        if (scalaClass.isAbstract || ABSTRACT_CLASSES.contains(tupleNs)) {
          val sealedV = if (isSealed) "sealed " else ""
          code += s"${sealedV}trait $name {"
          scalaClass.members.foreach {
            member =>
              val cd = member.toDef
              code ++= cd.code
              imports ++= cd.imports
          }
          code += s"}"
          addJsonEncoder=true
        } else {
          if(isParentSealed){
            val hint=getDiscriminator
            code += s"""@jsonHint("$hint")"""
          }
          code += s"final case class $name ("
          val members=new ListBuffer[String]
          scalaClass.members.foreach {
            member =>
              val cd = member.toParam
              imports ++= cd.imports
              members++=cd.code
          }
          scalaClass.parent.foreach{
            parent =>
              parserContext.getClass(parent.toScalaType) match {
                case Some(cls) =>
                  if (cls.isSealed) {
                    val pname = REMAP_CLASSES.getOrElse((cls.namespace,cls.name), cls.name)
                    filename=s"${cls.namespace}/$pname.scala"
                  }
                  cls.members.foreach {
                    member =>
                      val cd = member.toParam
                      imports ++= cd.imports
                      members ++= cd.code
                  }
                case None =>
                  println(s"Missing class $parent")
              }
          }
          code +=members.mkString(", ")
          code += s")"
          addJsonEncoder=true
        }
        if(addJsonEncoder){
          code ++=List("",
            s"object $name {",
            s"  implicit val jsonCodec:JsonCodec[$name]= DeriveJsonCodec.gen[$name]",
            "}"
          )
        }

        CodeData(code = code.toList, imports = imports.toList, filename = Some(filename))
      } else CodeData.empty
    }
  }

  implicit class MethodConverter(member: ScalaClassMember) {
    def getType:String={
      member.typ.map(_.toScalaType).getOrElse("Json")
    }
    def toDef(implicit parserContext: ParserContext): CodeData = {
      CodeData(code=List(s"  def ${member.name.fixName.toCamel}: ${getType}"))
    }

    def toParam(implicit parserContext: ParserContext): CodeData = {
      val str=member.name.fixName
      val name = str.toCamel
      val defaultValue=if(member.isOptional) " = None" else ""
      if(str==name){
        CodeData(code=List(s"""$name: $getType$defaultValue"""))
      } else
      CodeData(code=List(s"""@jsonField("$str") $name: $getType$defaultValue"""))
    }
  }

  implicit class StringUtils(str:String) {
    def toCamel:String= if (str.contains("_")) {
      CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, str)
    } else str

    def toCamelUpper: String =
      CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, str)

    def toSnake: String =
      CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, str)

    def fixName: String = {
      val cleanValue=if (str.endsWith("?")) {
        str.dropRight(1)
      } else str
      if(cleanValue=="if"){"`if`"}else cleanValue
    }

    def typeToScala:String={
      TYPE_MAPPING.getOrElse(str, str)
    }

  }
}