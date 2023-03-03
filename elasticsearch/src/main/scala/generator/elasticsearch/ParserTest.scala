package generator.elasticsearch

import com.google.common.base.CaseFormat
import fastparse.Parsed
import os._
import tsparse.Parser

import scala.collection.mutable.ListBuffer

object ParserTest extends  App {
  lazy val TYPE_MAPPING=Map("boolean"-> "Boolean",
    "Field"-> "String",
    "string" -> "String",
    "integer" -> "Int",
    "double" -> "Double",
    "Id" -> "String",
    "Name" -> "String",
  "UserDefinedValue"-> "Json",
    "ProcessorContainer" -> "Processor"
  )


  def cookType(typ:String)=TYPE_MAPPING.getOrElse(typ, typ)
  def cookName(name:String)=if(name.contains("_")) {
    val newName=CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, name)
    s"""@jsonField("$name") $newName"""
  } else name

  val dictionaryTs="""\s*([\w_\?]+)\s*:\s*Dictionary\<([\w_]+)\s*,\s*([\w_]+)\>\s*""".r
  val fieldTs="""\s*([\w_\?]+)\s*:\s*([\w_\[\]]+)\s*""".r
  val classTs="""\s*export class ([\w_\?]+)\s*\{\s*""".r
  val classExtendsTs="""\s*export class ([\w_\?]+) extends (.*)\s*\{\s*""".r

  final case class TsMember(name:String, typ:String, required:Boolean=false)
  final case class TsClass(name:String, parents:List[String]=Nil, members:List[TsMember]=Nil)

  def extractClasses(filename:String):List[TsClass]={
    val content= os.read(Path(filename))
    val classes = new ListBuffer[TsClass]
    var members = new ListBuffer[TsMember]
    var currentClass: Option[TsClass] = None

    content.split('\n').foreach {
      case classTs(name) =>
        currentClass.map {
          c =>
            classes += c.copy(members = members.toList)
        }
        members = new ListBuffer[TsMember]
        currentClass = Some(TsClass(name))
      case dictionaryTs(name, type1, type2) =>
        members += TsMember(cookName(name),
          s"Map[${cookType(type1)}, ${cookType(type1)}] = Map.empty[${cookType(type1)}, ${cookType(type2)}]"
        )
      case classExtendsTs(name, parents) =>
        currentClass.map {
          c =>
            classes += c.copy(members = members.toList)
        }
        members = new ListBuffer[TsMember]
        currentClass = Some(TsClass(name, parents = parents.split(' ').filter(_.nonEmpty).toList, members = Nil)
        )
      case dictionaryTs(name, type1, type2) =>
        members += TsMember(cookName(name),
          s"Map[${cookType(type1)}, ${cookType(type1)}] = Map.empty[${cookType(type1)}, ${cookType(type2)}]"
        )
      case fieldTs(name, type1) =>
        if (name.endsWith("?")) {
          if (type1.endsWith("[]"))
            members += TsMember(cookName(name.dropRight(1)),
              s"List[${cookType(type1.dropRight(2))}] = Nil"
            )
          else
            members += TsMember(cookName(name.dropRight(1)),
              s"Option[${cookType(type1)}] = None"
            )
        } else {
          if (type1.endsWith("[]"))
            members += TsMember(cookName(name),
              s"List[${cookType(type1.dropRight(2))}]"
            )
          else
            members += TsMember(cookName(name),
              s"${cookType(type1)}", true
            )
        }
      case s => s
    }
    currentClass.map {
      c =>
        classes += c.copy(members = members.toList)
    }
    classes.toList
  }

  val classes= extractClasses("C:\\Projects\\it-gov\\elasticsearch-specification\\specification\\ingest\\_types\\Processors.ts")
classes.foreach(println)
}
