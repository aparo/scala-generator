package io.megl.generator


import ammonite.ops._
import generator.ts.ParserContext
import org.scalablytyped.converter.internal._
import org.scalablytyped.converter.internal.ts.parser._

import scala.collection.mutable.ListBuffer

object OutputGenerator extends App {
  val specification = os.home / "projects" / "github" / "elasticsearch" / "elasticsearch-specification"
  val schemaFile = specification / "output" / "schema" / "schema.json"
  val typesFile = specification / "output" / "typescript" / "types.ts"

  val targetDir = (os.pwd / "elasticsearch-generated" / "src" / "main" / "scala")
  os.makeDir.all(targetDir)
  implicit val parserContext = new ParserContext()

  val infile=InFile(typesFile)
  val res = parseFile(infile)
  val namespace=""
  res match {
    case Left(value) =>
      println(value)
    case Right(value) =>
      //                     println(value)
      parserContext.parse(infile, namespace, value)
  }

  parserContext.scalaClasses
    .map(_._2.toCode)
    .groupBy(_.filename)
    .foreach {
      case (filename, codes) =>
        var targetFileDir = targetDir
        var finalName: String = "zio/" + filename.getOrElse("common/common.scala").replace(".", "/").replace("/scala", ".scala")
        val packg = finalName.split("/").dropRight(1).mkString(".")
        finalName.split('/').foreach {
          value =>
            if (value.endsWith(".scala")) {
              finalName = value
            } else {
              targetFileDir = targetFileDir / value
            }
        }
        os.makeDir.all(targetFileDir)
        val text = codes.flatMap(_.code).mkString("\n")
        var includes = new ListBuffer[String]
        includes ++= codes.flatMap(_.imports).toSet
        if (text.contains(": Chunk[")) includes += "import zio._"
        if (text.contains("OffsetDateTime") || text.contains("LocalDateTime")) includes += "import java.time._"
        val finalIncludes = includes.toSet.toList.sorted.mkString("\n")
        os.write.over(targetFileDir / finalName, s"package $packg\n$finalIncludes\n$text")
        println(s"Written ${targetFileDir / finalName}")

    }
  println("Completed")
}
