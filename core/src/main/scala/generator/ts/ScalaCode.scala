package generator.ts

import org.scalablytyped.converter.internal.ts.TsProtectionLevel
import org.scalablytyped.converter.internal.ts._
import org.scalablytyped.converter.internal._

import scala.collection.mutable.ListBuffer

sealed trait ScalaCode {
  self =>
  def members:    List[ScalaClassMember]
}

case class ScalaImport(from: String, items: List[String]) extends ScalaCode {
  override def members: List[ScalaClassMember] = Nil
}

case class ScalaClassConstructor(comments: Comments) extends ScalaCode{
  override def members: List[ScalaClassMember] = Nil
}


sealed trait ScalaType {
  def isRequired: Boolean
  def isNullable: Boolean
  def setRequired(required: Boolean = false): ScalaType

  def setNullable(nullable: Boolean = false): ScalaType

  def toScalaType: String
}

case object UndefinedType extends ScalaType {


  override def toScalaType: String = "None"

  override def isRequired: Boolean = false

  override def isNullable: Boolean = true

  override def setRequired(required: Boolean): ScalaType = UndefinedType

  override def setNullable(nullable: Boolean): ScalaType = UndefinedType
}

case class UnionType(types: List[ScalaType], isNullable: Boolean = false) extends ScalaType {
  override def isRequired: Boolean = types.exists(_.isRequired)

  override def setRequired(required: Boolean): ScalaType = this

  override def setNullable(nullable: Boolean): ScalaType = this.copy(isNullable = nullable)

  override def toScalaType: String = {
    val union = types.map(_.toScalaType).mkString("|")
    if (isNullable) {
      s"Option[$union]"
    } else union
  }

}

case class SimpleType(
    name:       String,
    tparams:    List[ScalaType] = Nil,
    isRequired: Boolean = false,
    isNullable: Boolean = false,
) extends ScalaType {
  override def setRequired(required: Boolean): ScalaType = copy(isRequired = required)

  override def setNullable(nullable: Boolean): ScalaType = copy(isNullable = nullable)

  override def toScalaType: String = {
    val n = Converters.TYPE_MAPPING.getOrElse(name, name)
    val value =
      if (tparams.isEmpty)
        n
      else {
        s"$n[${tparams.map(_.toScalaType).mkString(",")}]"
      }
    if (isNullable) {
      s"Option[$value]"
    } else value
  }
}

case class ScalaObjectType(
    namespace:  String,
    name:       String,
    comments:   Comments,
    members:    List[ScalaCode] = Nil,
    isRequired: Boolean = true,
    isNullable: Boolean = true,
) extends ScalaType {
  override def setRequired(required: Boolean): ScalaType = copy(isRequired = required)

  override def setNullable(nullable: Boolean): ScalaType = copy(isNullable = nullable)

  override def toScalaType: String = "Klass"


}

//   case class ScalaTypeAlias(comments: Comments, name:String) extends ScalaCode
case class ScalaClassMember(
    name:     String,
    level:    TsProtectionLevel,
    typ:      Option[ScalaType] = None,
    comments: Comments,
    isStatic: Boolean = false,
//                             isReadOnly: Boolean = false,
//                             isRequired: Boolean = false,
) extends ScalaCode {
  def isOptional: Boolean = typ.exists(_.isRequired) || name.endsWith("?")

  override def members: List[ScalaClassMember] = List(this)
}

case class ScalaClassMethod(name: String, level: TsProtectionLevel, comments: Comments) extends ScalaCode{
  override def members: List[ScalaClassMember] = Nil
}


case class ScalaClass(
    namespace:  String,
    name:       String,
    typeParams: List[String] = Nil,
    comments:   Comments,
    isAbstract: Boolean = false,
    parent:     Option[ScalaType] = None,
    implements: List[ScalaType] = Nil,
    members:    List[ScalaClassMember] = Nil,
    methods:    List[ScalaClassMethod] = Nil,
    jsLocation: Option[JsLocation] = None,
    codePath:   CodePath,
) extends ScalaCode {
}
case class ScalaEnumMember(name: String, expr: Option[TsExpr] = None, comments: Comments) extends ScalaCode{
  override def members: List[ScalaClassMember] = Nil
}


case class ScalaEnum(
    name:         String,
    comments:     Comments,
    declared:     Boolean = false,
    isConst:      Boolean = false,
    isValue:      Boolean = false,
    exportedFrom: Option[ScalaType] = None,
    values:      List[ScalaEnumMember] = Nil,
    jsLocation:   JsLocation,
    codePath:     CodePath,
) extends ScalaCode{
  override def members: List[ScalaClassMember] = Nil
}

