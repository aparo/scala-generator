// /*
//  * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
//  */

// package generator.elasticsearch

// object ExtractQueries /*extends App*/ {
//   import generator.FileUtils._

//   val devConfig = new DevConfig()
//   val outputPath = File(
//     s"/Users/alberto/Projects/CodeGenerator/tmp/${Constants.destPackage}-orm/jvm/src/test/resources/${Constants.destPackage}/queries"
//   )
//   val path        = devConfig.devESSourcePath / "core/src/test/java/org/elasticsearch/index/query"
//   val fragment    = """public void testFromJson()"""
//   val fragmentEnd = """}";"""

//   if (!outputPath.exists) outputPath.createDirectories()

//   val snakeCaseTransformation: String => String = _.replaceAll(
//     "([A-Z]+)([A-Z][a-z])",
//     "$1_$2"
//   ).replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

//   def cleanBlock(text: String): String = {
//     val aposPos = text.indexOf('"')
//     text
//       .substring(aposPos)
//       .split('\n')
//       .map(_.trim)
//       .map(_.drop(1).reverse.dropWhile(_ != '"').drop(1).reverse)
//       .mkString("\n")
//       .replace("\\n", "")
//       .replace("\\\"", "\"")
//       .replace("\\\\", "\\")
//   }

//   path.toJava.andTree.filter(_.getName.endsWith("Tests.java")).foreach { f =>
//     import better.files._
//     val code = f.toScala.contentAsString
//     if (code.contains(fragment)) {
//       val start      = code.indexOf(fragment)
//       val dirtyBlock = code.substring(start + fragment.length, code.indexOf(fragmentEnd, start) + 2)
//       val queryName  = f.getName.replace("BuilderTests.java", "")
//       val name       = snakeCaseTransformation(queryName)
// //        println(name)
//       for {
//         jsonBlock <- io.circe.parser.parse(cleanBlock(dirtyBlock))
//       } yield (outputPath / s"$name.json").writeText(jsonBlock.spaces2SortKeys)

//       val codeTest = s"""  it should "serialize and deserialize $queryName" in {
//                         |    val json = readResourceJSON("/${Constants.destPackage}/queries/$name.json")
//                         |    val oQuery = json.as[Query]
//                         |    oQuery.isRight should be(true)
//                         |    oQuery.right.get.isInstanceOf[$queryName] should be(true)
//                         |    val realQuery = oQuery.right.get.asInstanceOf[$queryName]
//                         |    val nJson=realQuery.asJson
//                         |    nJson.as[Query].right.get should be(realQuery)
//                         |  }
//                         |  """.stripMargin
//       println(codeTest)
//     }

//   }
// }
