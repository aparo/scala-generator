/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

///* Copyright 2017 - Wimobilize Pte Ltd.  All Rights Reserved. */
//package morpheus.dev.webservices.orm
//
//import com.google.common.base.CaseFormat
//import elasticsearch.mappings._
//
//trait CSUtils {
//  def csCase(value: String) = CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, value)
//}
//
//case class CSClassField(name: String, fieldType: String, multiple: Boolean, nested: Boolean = false, parentName: String) extends CSUtils {
//  def csName = {
//    var newName = csCase(name)
//    if (newName == parentName)
//      newName += "Value"
//    newName
//  }
//}
//
//class ClassObject(val module: String, val mapping: RootObjectMapping) extends CSUtils {
//
//  var nestedObj: List[Mapping] = Nil
//  val className = csCase(cookName(mapping.field))
//
//  var csExtraUsings: Set[String] = Set()
//
//  private def cookName(name: String): String = {
//    if (name.startsWith(module + "_"))
//      return name.substring(module.length + 1)
//    name
//  }
//
//  //  def elasticsearchTypeName=meta.typeName
//  //  def csNamespace="TNP."+meta.namespaceName.split("\.").toList.drop(1).map(_.capitalize).mkString(".")
//  //  def className=meta.className
//  def elasticsearchTypeName = mapping.name
//  def csNamespace = "TNP." + csCase(module)
//
//  val fields: List[CSClassField] = {
//    mapping.properties.map {
//      field =>
//        val (fieldType, multiple, nested) = field match {
//          case o: ObjectMapping =>
//            nestedObj = nestedObj ::: o :: Nil
//            (o.field.capitalize, o.multiple, true)
//          case o: DocumentMapping =>
//            nestedObj = nestedObj ::: o :: Nil
//            (o.field.capitalize, false, true)
//          case o: NestedMapping =>
//            nestedObj = nestedObj ::: o :: Nil
//            (o.field.capitalize, o.multiple, true)
//          case o: StringMapping => ("string", o.multiple, false)
//          case o: NumberMapping => (o.numberType.toString match {
//            case "integer" => "int"
//            case d         => d
//          }, o.multiple, false)
//          case o: BooleanMapping => ("bool", o.multiple, false)
//          case o: DateMapping    => ("DateTime", o.multiple, false)
//          case o: BinaryMapping  => ("Binary", false, false)
//          case o: GeoPointMapping =>
//            csExtraUsings += "TNP.Utils"
//            ("GeoPoint", o.multiple, false)
//          case o: IpMapping => ("Ip", o.multiple, false)
//          case _            => ("string", false, false) //TODO manage other mappings
//
//        }
//        new CSClassField(field.field, fieldType, multiple, nested, parentName = className)
//    }.toList
//  }
//}
