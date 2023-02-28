/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

//
//import java.io.File
//import javax.inject.{Inject, Singleton}
//
//import akka.actor.ActorSystem
//import morpheus.base.module.ApplicationManager
//import morpheus.base.{NoArgs, NoArgsCommand}
//import com.google.common.base.CaseFormat
//import elasticsearch.utils.JsonUtils
//import io.circe._
//
//import scala.collection.mutable.ListBuffer
//import scala.io.Source
//
//case class Member(var name: String, var `type`: String,
//                  var multiple: Boolean = false, var required: Boolean = true,
//                  var codeName: String = "", var default: Option[Json] = None, var description: String = "") extends JsonUtils {
//  //  def toJson:Json={
//  //     joClean(Json.obj("name"->name, "type"-> `type`, "multiple"->multiple, "required" -> required,
//  //      "codeName"->codeName, "default" -> default, "description" -> description
//  //    ))
//  //  }
//}
//
//object Member {
//  def create(oname: String, `type`: String): Member = {
//    val name = oname match {
//      case "queryName" => "_name"
//      case "name" => "field"
//      case default => default
//    }
//
//    val (typ, mul, req) = convertType(`type`)
//    val realName = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, name)
//
//    new Member(realName, typ, mul, req,
//      codeName = name, description = getDescription(realName))
//
//  }
//
//  def cookType(name: String, currType: String): String = name match {
//    case "minimum_should_match" => "int"
//    case default => currType
//  }
//
//  def getDefault(name: String): Option[Json] = name match {
//    case "boost" => Some(JsNumber(1.0))
//    case default => None
//  }
//
//  def getDescription(name: String): String = name match {
//    case "_name" => "the name for the query"
//    case "boost" => "the boost value for the current query"
//    case "field" => "the field to be used"
//    case "fields" => "the fields to be used"
//    case "value" => "the value to be used"
//    case "values" => "a list of values to be used"
//    case "minimum_should_match" => "mininum number of subqueries to match"
//    case "disable_coord" => "disable the coord for the current query"
//    case "adjust_pure_negative" => "adjust score for pure negative values"
//    case "script" => "the script to be executed"
//    case "lang" => "the scripting language"
//    case "params" => "additional parameters to be passed to a script"
//    case _ => ""
//  }
//
//  def convertType(`type`: String): (String, Boolean, Boolean) = {
//    var multiple = false
//    var required = true
//    var typ = `type`
//    if (typ.startsWith("*")) {
//      required = false
//      typ = typ.substring(1)
//    }
//    if (typ.startsWith("[]")) {
//      multiple = true
//      typ = typ.substring(2)
//    }
//
//    typ match {
//      case "float32" => ("double", multiple, required)
//      case "float64" => ("double", multiple, required)
//      case "int64" => ("long", multiple, required)
//      case "int" => ("integer", multiple, required)
//      case "bool" => ("bool", multiple, required)
//      case "interface{}" => ("jvalue", multiple, required)
//      case "map[string]interface{}" => ("jobject", multiple, required)
//      case s: String => (s, multiple, required)
//
//    }
//  }
//}
//
//case class Record(var name: String, className: String, parent: Option[String] = None, members: List[Member] = Nil,
//                  documentation: String = "") extends JsonUtils {
//  def dump(): Unit = {
//    println(s"class $className exdents $parent  // $name $documentation")
//    members.foreach {
//      member =>
//        println(s"  - ${member.name} ${member.`type`} req:${member.required} mul:${member.multiple}")
//    }
//
//  }
//
//  //  def toJson:Json={
//  //    Json.obj("name"->name, "className" -> className, "parent" -> parent, "documentation" -> documentation,
//  //    "members" -> members.map(_.toJson))
//  //  }
//  //
//  def save(destDir: File): Unit = {
//    implicit val memberFmt = Json.format[Member]
//    implicit val recordFmt = Json.format[Record]
//
//    import generator.elasticsearch.utilities.FileUtils.printToFile
//    printToFile(new File(destDir, s"$name.json")) {
//      f => f.write(Json.prettyPrint(Json.toJson(this)))
//    }
//  }
//}
//
//@Singleton
//class ParseGoAPI @Inject()(val system: ActorSystem, val moduleManager: ApplicationManager) extends NoArgsCommand {
//
//  val name = "ParseGoAPI"
//  val description = "Parse Go API"
//
//  val destinationPath = "/opt/scala/elasticsearch/rest-api-spec"
//
//  override def run(options: NoArgs): Unit = {
//    val files = new File("/opt/scala/goapi").listFiles()
//      .filterNot(_.getName.toLowerCase.endsWith("_test.go"))
//      .filter(_.getName.toLowerCase.endsWith(".go"))
//
//    //processing search queries
//    List(
//      ("search_queries", "queries"),
//      ("search_filters", "filter"), ("search_aggs", "aggregations")).foreach {
//      case (starts, dir) =>
//        val queryFiles = files.filter(_.getName.toLowerCase.startsWith(starts))
//        //      .filter(_.getName=="search_queries_terms.go")
//        queryFiles.foreach {
//          file => dump(file, dir)
//        }
//    }
//
//  }
//
//  val nameExtractorReg = """\s+source\["(.*?)"\]\s+=.*""".r
//
//  def extractName(lines: List[String]): Option[String] = {
//    lines.foreach {
//      case nameExtractorReg(name) => return Some(name)
//      case _ =>
//    }
//    None
//  }
//
//  def dump(sourceFile: File, namespace: String) {
//    val lines = Source.fromFile(sourceFile, "utf8").getLines().toList
//    var inStruct = false
//    val typeExt = """type\s+(\w+)\s+struct\s+\{\s*""".r
//
//    var record: Option[Record] = None
//    var recordName = ""
//    var parentName = ""
//    var documentation = ""
//    var members = new ListBuffer[Member]
//    def emit(): Unit = {
//      record = Some(Record(extractName(lines).getOrElse(CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, recordName)), recordName,
//        Some(parentName), members = members.toList, documentation = documentation))
//    }
//
//    lines.foreach {
//      case s: String if s.contains("reference/current") =>
//        documentation = "http://www.elastic.com/elasticsearch/guide/reference/master" + s.substring(s.indexOf("reference/current") + "reference/current".length)
//      case typeExt(name) =>
//        inStruct = true
//        recordName = name
//      case "}" if inStruct =>
//        inStruct = false
//        emit()
//      case s: String if inStruct =>
//        if (!s.trim.contains(" ")) {
//          // is parent
//          parentName = s.trim
//        } else {
//          val tokens = s.trim.split(" ").filter(_.length > 0)
//          if (tokens.length == 2) {
//            members += Member.create(tokens(0), tokens(1))
//          }
//        }
//      case s: String =>
//
//    }
//
//    if (record.isDefined) {
//      var data = lines.mkString("\n")
//      record.get.members.foreach {
//        m =>
//          if (data.contains(s"${m.name} != nil")) {
//            m.required = false
//          }
//          //          println(s"""${m.name} != "" """)
//          if (data.contains( s"""${m.name} != "" """)) {
//            m.required = false
//          }
//      }
//      record.map(_.dump)
//      val destDir = new File(destinationPath, namespace)
//      if (!destDir.exists()) destDir.mkdirs()
//      record.map(_.save(destDir))
//    }
//  }
//}
//
