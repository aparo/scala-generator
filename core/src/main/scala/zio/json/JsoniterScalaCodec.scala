package zio.json

import com.github.plokhotnyuk.jsoniter_scala.core._
import zio.Chunk
import zio.json.ast._

import java.nio.charset.StandardCharsets
import java.util
import scala.collection.immutable.VectorBuilder
import scala.jdk.CollectionConverters._
object JsoniterScalaCodec {

  implicit val jsonC3c: JsonValueCodec[Json] = jsonCodec()

  def jsonCodec(
      maxDepth:     Int = 128,
      initialSize:  Int = 8,
      doSerialize:  Json => Boolean = _ => true,
      numberParser: JsonReader => Json = defaultNumberParser,
  ): JsonValueCodec[Json] =
    new JsoniterScalaCodec(maxDepth, initialSize, doSerialize, numberParser)

  lazy val defaultNumberParser: JsonReader => Json = in => {
    in.setMark()
    var digits = 0
    var b      = in.nextByte()
    if (b == '-') b = in.nextByte()
    try
      while (b >= '0' && b <= '9') {
        b = in.nextByte()
        digits += 1
      }
    catch {
      case _: JsonReaderException => // ignore the end of input error for now
    }
    in.rollbackToMark()
    if ((b | 0x20) != 'e' && b != '.') {
      if (digits < 19) {
        if (digits < 10) Json.Num(in.readInt())
        else Json.Num(in.readLong())
      } else {
        val x = in.readBigInt(null)
        if (x.bitLength < 64) Json.Num(x.longValue)
        else new Json.Num(new java.math.BigDecimal(x.bigInteger))
      }
    } else new Json.Num(in.readBigDecimal(null).bigDecimal)
  }

  def asciiStringToJString[A](buf: Array[Byte], len: Int): Json = Json.Str(new String(buf, len))

//  def stringValue(c: HCursor): String = c.value match {
//    case s: JString => s.value
//    case _ => null
//  }
//
//  def bigIntValue(c: HCursor): BigInt = c.value match {
//    case n: JNumber => n.value match {
//      case jl: JsonLong => BigInt(jl.value)
//      case jbd: JsonBigDecimal =>
//        val bd = jbd.value
//        if (bd.scale == 0) new BigInt(bd.unscaledValue)
//        else null
//      case _ => null
//    }
//    case _ => null
//  }
//
//  def jsonValue(x: BigInt): Json = new JNumber(new JsonBigDecimal(new java.math.BigDecimal(x.bigInteger)))
}

final class JsoniterScalaCodec(
    maxDepth:     Int,
    initialSize:  Int,
    doSerialize:  Json => Boolean,
    numberParser: JsonReader => Json,
) extends JsonValueCodec[Json] {
  private[this] val trueValue        = Json.Bool(true)
  private[this] val falseValue       = Json.Bool(false)
  private[this] val emptyArrayValue  = Json.Arr()
  private[this] val emptyObjectValue = Json.Obj()

  override val nullValue: Json = Json.Null

  override def decodeValue(in: JsonReader, default: Json): Json = decode(in, maxDepth)

  override def encodeValue(x: Json, out: JsonWriter): Unit = encode(x, out, maxDepth)

  private[this] def decode(in: JsonReader, depth: Int): Json = {
    val b = in.nextToken()
    if (b == '"') {
      in.rollbackToken()
      new Json.Str(in.readString(null))
    } else if (b == 'f' || b == 't') {
      in.rollbackToken()
      if (in.readBoolean()) trueValue
      else falseValue
    } else if (b >= '0' && b <= '9' || b == '-') {
      in.rollbackToken()
      numberParser(in)
    } else if (b == '[') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken(']')) emptyArrayValue
      else {
        in.rollbackToken()
        val x = new VectorBuilder[Json]
        while ({
          x += decode(in, depthM1)
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken(']')) new Json.Arr(Chunk.fromIterable(x.result()))
        else in.arrayEndOrCommaError()
      }
    } else if (b == '{') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken('}')) emptyObjectValue
      else {
        in.rollbackToken()
        val x = new util.LinkedHashMap[String, Json](initialSize)
        while ({
          x.put(in.readKeyAsString(), decode(in, depthM1))
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken('}')) new Json.Obj(Chunk.fromIterator(x.asScala.iterator))
        else in.objectEndOrCommaError()
      }
    } else in.readNullOrError(nullValue, "expected JSON value")
  }

  private[this] def encode(x: Json, out: JsonWriter, depth: Int): Unit = x match {
    case s: Json.Str =>
      val str = s.value
      if (str.length == 1) out.writeVal(str.charAt(0))
      else out.writeVal(s.value)
    case b: Json.Bool => out.writeVal(b.value)
    case n: Json.Num  => encodeJsonNumber(n, out)
    case a: Json.Arr =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeArrayStart()
      a.elements.foreach(v => encode(v, out, depthM1))
      out.writeArrayEnd()
    case o: Json.Obj =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeObjectStart()
      val it = o.fields.iterator
      while (it.hasNext) {
        val (k, v) = it.next()
        if (doSerialize(v)) {
          out.writeKey(k)
          encode(v, out, depthM1)
        }
      }
      out.writeObjectEnd()
    case _ => out.writeNull()
  }

  private[this] def encodeJsonNumber(x: Json.Num, out: JsonWriter): Unit = x match {
//    case l: JsonLong => out.writeVal(l.value)
//    case f: JsonFloat => out.writeVal(f.value)
//    case d: JsonDouble => out.writeVal(d.value)
//    case bd: JsonBigDecimal => out.writeVal(bd.value)
    case _ => out.writeRawVal(x.toString.getBytes(StandardCharsets.UTF_8))
  }
}
