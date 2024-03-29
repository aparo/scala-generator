package generator.ts

import com.google.common.base.CaseFormat

import scala.collection.mutable.ListBuffer

case class CodeData(code: List[String] = Nil, imports: List[String] = Nil, filename: String, isPackage: Boolean = false)
object CodeData {
  lazy val empty = CodeData(Nil, Nil, "package.scala")
}
object Converters {
  lazy val SKIP_GENERATION  = Set(("ingest", "ProcessorContainer"))
  lazy val ABSTRACT_CLASSES = Set(("ingest", "ProcessorBase"))
  lazy val SEALED_CLASSES   = Set(("ingest", "ProcessorBase"))
  lazy val REMAP_CLASSES =
    Map(("ingest", "ProcessorBase") -> "Processor", ("ingest", "ProcessorContainer") -> "Processor")
  lazy val DISCRIMINATOR_END = List("Processor")
  lazy val TYPE_MAPPING = Map(
    "boolean" -> "Boolean",
    "Array" -> "Chunk",
    "Field" -> "String",
    "string" -> "String",
    "long" -> "Long",
    "float" -> "Float",
    "Fields" -> "Chunk[String]",
    "boolean" -> "Boolean",
    "integer" -> "Int",
    "double" -> "Double",
    "Id" -> "String",
    "DateTime" -> "LocalDateTime",
    "Name" -> "String",
    "UserDefinedValue" -> "Json",
    "ProcessorContainer" -> "Processor",
    "IndexName" -> "String",
    "Host" -> "String",
    "Id" -> "String",
    "Ids" -> "Chunk[String]",
    "IndexAlias" -> "String",
    "IndexName" -> "String",
    "IndexPattern" -> "String",
    "IndexPatterns" -> "Chunk[String]",
    "VersionString" -> "String",
    "VersionNumber" -> "Int",
    "EpochTime[UnitMillis]" -> "Long",
    "SequenceNumber" -> "Int",
    "DurationValue[UnitMillis]" -> "Long",
    "NodeId" -> "String",
    "TaskId" -> "String",
    "Ip" -> "String",
    "Duration" -> "String",
    "ByteSize" -> "String",
    "Bytes" -> "String",
    "Str()" -> "String",
    "String|null|None" -> "Option[String]",
    "Boolean|String|None" -> "Option[Json]",
    "Double|null|None" -> "Option[Double]",
    "Double|null" -> "Option[Double]",
    "Int|null" -> "Option[Int]",
    "String|null|None" -> "Option[String]",
    "String|null" -> "Option[String]",
    "Long|null|None" -> "Option[Long]",
    "Long|null" -> "Option[Long]",
    "DateMath|null|None" -> "Option[DateMath]",
    "DateMath|null" -> "Option[DateMath]",
    "Expand_wildcards" -> "ExpandWildcards",
    "Chunk[Double]|null|None" -> "Chunk[Double]",
    "Indices" -> "Chunk[String]",


    "Default_operator" -> "DefaultOperator",
    "Uuid" -> "String",

    // with doubt
    "Stringified[Int]" -> "Int"
  )

  implicit class ClassConverter(scalaClass: ScalaClass) {
    def getParent(implicit parserContext: ParserContext): Option[ScalaClass] =
      scalaClass.parent.flatMap(p => parserContext.getClass(p.toScalaType))

    def isParentSealed(implicit parserContext: ParserContext): Boolean = getParent.exists(_.isSealed)

    def getDiscriminator: String = {
      val name = scalaClass.name.fixName
      DISCRIMINATOR_END.find(p => name.endsWith(p)) match {
        case Some(value) => name.dropRight(value.length).toSnake
        case None        => name.toSnake
      }
    }
    def isSealed: Boolean = {
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      SEALED_CLASSES.contains(tupleNs)
    }
    def toCode(implicit parserContext: ParserContext): CodeData = {
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      if (!SKIP_GENERATION.contains(tupleNs)) {
        val code    = new ListBuffer[String]
        val imports = new ListBuffer[String]
        imports += "import zio.json._"
        val name           = REMAP_CLASSES.getOrElse(tupleNs, scalaClass.name)
        var filename       = s"${scalaClass.namespace}/$name.scala"
        var addJsonEncoder = false
        if (scalaClass.isTrait || ABSTRACT_CLASSES.contains(tupleNs)) {
          val sealedV = if (isSealed) "sealed " else ""
          code += s"${sealedV}trait $name {\n"
          scalaClass.members.foreach { member =>
            val cd = member.toDef
            code ++= cd.code
            code += "\n"
            imports ++= cd.imports
          }
          code += s"}\n"
          if(isSealed)
            addJsonEncoder = true
        } else {
          if (isParentSealed) {
            val hint = getDiscriminator
            code += s"""@jsonHint("$hint")"""
          }
          code += s"final case class $name (\n  "
          val members = new ListBuffer[String]
          scalaClass.members.foreach { member =>
            val cd = member.toParam
            imports ++= cd.imports
            members ++= cd.code
          }
          scalaClass.parent.foreach { parent =>
            parserContext.getClass(parent.toScalaType) match {
              case Some(cls) =>
                if (cls.isSealed) {
                  val pname = REMAP_CLASSES.getOrElse((cls.namespace, cls.name), cls.name)
                  filename = s"${cls.namespace}/$pname.scala"
                }
                cls.members.foreach { member =>
                  val cd = member.toParam
                  imports ++= cd.imports
                  members ++= cd.code
                }
              case None =>
                println(s"Missing class $parent")
            }
          }
          code += members.mkString(",\n  ")
          code += s")\n"
          addJsonEncoder = true
        }
        if (addJsonEncoder) {
          code ++= List(
            "\n",
            s"object $name {\n",
            s"  implicit val jsonCodec:JsonCodec[$name]= DeriveJsonCodec.gen[$name]\n",
            "}\n",
          )
        }

        CodeData(code = code.toList, imports = imports.toList, filename = filename)
      } else CodeData.empty
    }
  }

  implicit class ClassObjectConverter(scalaClass: ScalaObjectType) {

    def getDiscriminator: String = {
      val name = scalaClass.name.fixName
      DISCRIMINATOR_END.find(p => name.endsWith(p)) match {
        case Some(value) => name.dropRight(value.length).toSnake
        case None        => name.toSnake
      }
    }

    def isSealed: Boolean = {
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      SEALED_CLASSES.contains(tupleNs)
    }

    def toCode(implicit parserContext: ParserContext): CodeData = {
      val tupleNs = (scalaClass.namespace, scalaClass.name)
      if (!SKIP_GENERATION.contains(tupleNs)) {
        val code    = new ListBuffer[String]
        val imports = new ListBuffer[String]
        imports += "import zio.json._"
        val name     = REMAP_CLASSES.getOrElse(tupleNs, scalaClass.name)
        var filename = s"${scalaClass.namespace}/$name.scala"
        code += s"\nfinal case class $name (\n"
        val members = new ListBuffer[String]
        scalaClass.members.foreach {
          case m: ScalaClassMember =>
            val cd = m.toParam
            imports ++= cd.imports
            members ++= cd.code
          case _ =>
        }
        code += members.mkString(",\n  ")
        code += s"\n)\n"
        code ++= List(
          "\n",
          s"object $name {\n",
          s"  implicit val jsonCodec:JsonCodec[$name]= DeriveJsonCodec.gen[$name]\n",
          "}\n",
        )

        CodeData(code = code.toList, imports = imports.toList, filename = filename)
      } else CodeData.empty
    }
  }

  implicit class MethodConverter(member: ScalaClassMember) {
    def getType: String =
      member.typ.map(_.toScalaType).getOrElse("Json").typeToScala
    def toDef(implicit parserContext: ParserContext): CodeData =
      CodeData.empty.copy(code = List(s"  def ${member.name.fixName.toCamel}: ${getType}"))

    def toParam(implicit parserContext: ParserContext): CodeData = {
      val str          = member.name.fixName
      val name         = fixReserved(str.toCamel)
      val typ=getType
      val defaultValue = if (member.isOptional) {
        if(typ=="String") "" else " = None"
      } else if(typ.startsWith("Option[")) " = None"
      else ""
      if (str == name) {
        CodeData.empty.copy(code = List(s"""$name: $typ$defaultValue"""))
      } else
        CodeData.empty.copy(code = List(s"""@jsonField("$str") $name: $typ$defaultValue"""))
    }
  }

  implicit class StringUtils(string: String) {
    def toCamel: String = {
      var str = string
      if (str.startsWith("_")) str = str.substring(1)
      if (str.contains("_")) {
        CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, str)
      } else str
    }

    def toCamelUpper: String = {
      var str = string
      if (str.startsWith("_")) str = str.substring(1)
      CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, str)
    }

    def toSnake: String =
      CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, string)

    def fixName: String = {
      val cleanValue = if (string.endsWith("?")) {
        string.dropRight(1)
      } else string
      if (cleanValue == "if") { "`if`" }
      else if (cleanValue == "override") { "`override`" }
      else if (cleanValue == "match") { "`match`" }
      else if (cleanValue == "index-graveyard") { "indexGraveyard" }
      else if (cleanValue == "x-pack") { "xpack" }
      else cleanValue
    }

    def typeToScala: String =
      TYPE_MAPPING.getOrElse(string, string)

  }

  lazy val RESERVED=Set("type")

  def fixReserved(str: String):String={
    if(str.contains(".")) s"`$str`"
    else if(RESERVED.contains(str)) s"`$str`" else str
  }
}
