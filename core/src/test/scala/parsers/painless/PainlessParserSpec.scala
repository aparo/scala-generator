package parsers.painless

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import parsers.painless.PainlessParser.painlessExpr


class PainlessParserSpec extends AnyWordSpec with Matchers {
  "PainlessParser" should {
    "parse 'test'" in {
      val painlessStr = """'test'"""
      val parsedTree    = Painless.Str("test")
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse 1234" in {
      val painlessStr = """1234"""
      val parsedTree = Painless.Num(1234)
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse true" in {
      val painlessStr = """true"""
      val parsedTree = Painless.True
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse false" in {
      val painlessStr = """false"""
      val parsedTree = Painless.False
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse null" in {
      val painlessStr = """null"""
      val parsedTree = Painless.Null
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse ['test','test2','test3']" in {
      val painlessStr = """['test','test2','test3']"""
      val parsedTree = Painless.Arr(Painless.Str("test"), Painless.Str("test2"), Painless.Str("test3"))
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse ctx?.available_kex.contains('diffie-hellman-group-exchange-sha1')" in {
      val painlessStr = """ctx?.available_kex.contains('diffie-hellman-group-exchange-sha1')"""
      val parsedTree = Painless.AccessTypeSimple("ctx?","available_kex")
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }
    "parse ctx?.available_kex!=null" in {
      val painlessStr = """ctx?.available_kex!=null"""
      val parsedTree = Painless.Expression(Painless.AccessTypeSimple("ctx?", "available_kex"), Painless.OperatorNotEqual, Painless.Null)
      val Parsed.Success(value, _) =
        parse(painlessStr, painlessExpr(_))
      value shouldBe parsedTree
    }


  }
}
