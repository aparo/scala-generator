package parsers.painless

object Painless{
  sealed trait Node extends Any {
    def apply(i: Int): Node = this.asInstanceOf[Arr].value(i)

  }
  sealed trait Operator extends Node

  case object OperatorAnd extends Operator
  case object OperatorOr extends Operator
  case object OperatorEqual extends Operator
  case object OperatorNotEqual extends Operator

  case class Expression(left:Node, operator: Operator, right:Node) extends Node

  case class Str(value: String) extends AnyVal with Node

  sealed trait AccessType extends Node
  case class AccessTypeSimple(value: String*) extends AccessType
  case class AccessTypeWithMethod(value: AccessType, method:Node) extends AccessType
  case class Arr(value: Node*) extends AnyVal with Node

  case class Num(value: Double) extends AnyVal with Node

  case object False extends Node{
    def value = false
  }
  case object True extends Node{
    def value = true
  }
  case object Null extends Node{
    def value = null
  }
}



object PainlessParser {
  import fastparse._, NoWhitespace._
  def stringChars(c: Char) = c != '\'' && c != '\\'

  def space[_: P] = P(CharsWhileIn(" \r\n", 0))

  def digits[_: P] = P(CharsWhileIn("0-9"))

  def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

  def fractional[_: P] = P("." ~ digits)

  def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => Painless.Num(x.toDouble)
  )

  def `null`[_: P] = P("null").map(_ => Painless.Null)
  def and[_: P] = P("&&").map(_ => Painless.OperatorAnd)
  def or[_: P] = P("||").map(_ => Painless.OperatorOr)
  def equal[_: P] = P("==").map(_ => Painless.OperatorEqual)
  def notEqual[_: P] = P("!=").map(_ => Painless.OperatorNotEqual)

  def `false`[_: P] = P("false").map(_ => Painless.False)

  def `true`[_: P] = P("true").map(_ => Painless.True)
  def hexDigit[_: P] = P(CharIn("0-9a-fA-F"))

  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  def escape[_: P] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P] = P(CharsWhile(stringChars))


  def string[_: P] =
    P(space ~ "'" ~/ (strChars | escape).rep.! ~ "'").map(Painless.Str)

  def methodChars[_: P] = P(CharIn("a-zA-Z_?").rep(1, max=1) ~ CharsWhileIn("0-9a-zA-Z_?"))
  def pointer[_: P] =
    P(methodChars.!)

  def accessTypeSimple[_: P] =
    P(pointer.rep(min=1, sep = "."./)~ !"(").map(Painless.AccessTypeSimple(_: _*))

  def accessTypeMethod[_: P] =
    P(accessTypeSimple ~/ "("~/ space ~ painlessExpr ~/ space ~ ")").map(v => Painless.AccessTypeWithMethod(v._1, v._2))

  def array[_: P] =
    P("[" ~/ space ~ painlessExpr.rep(sep = ","./) ~ space ~ "]").map(Painless.Arr(_: _*))

  def operator[_: P] =
    P(space ~ (and|or|equal|notEqual) ~ space)

  def accessType[_: P] =
    P((accessTypeMethod|accessTypeSimple))

  def expression[_: P] = P(accessType ~/ operator ~/ painlessExpr).map(v => Painless.Expression(v._1, v._2, v._3))
//  def expression[_: P] = P((accessTypeMethod|accessTypeSimple) ~/ operator ~/ painlessExpr).map(v => Painless.Expression(v._1, v._2, v._3))

  def painlessExpr[_: P]: P[Painless.Node] = P(
    space ~ (  string | `true` | `false` | `null` | number | array | expression | accessType ) ~ space
  )
}
