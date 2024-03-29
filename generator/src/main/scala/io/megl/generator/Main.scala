package io.megl.generator

import ammonite.ops._
import generator.ts.ParserContext
import org.scalablytyped.converter.internal._
import org.scalablytyped.converter.internal.ts.parser._

import scala.collection.mutable.ListBuffer
object Main extends App {
  val specification =
    os.home / "projects" / "github" / "elasticsearch" / "elasticsearch-specification" / "specification" / "ingest" / "delete_pipeline"
  val targetDir = os.pwd / "elasticsearch-generated" / "src" / "main" / "scala"
  os.makeDir.all(targetDir)
  implicit val parserContext = new ParserContext

  val tsFiles =
    (ls.rec ! specification)
      .filter(_.isFile)
      .filter(_.toString().endsWith(".ts"))
      .sorted
      .map(p => InFile(p))
    // tsFiles.foreach(p => println(p))

    tsFiles
//     .take(13)
      .foreach { infile =>
//             println(infile.toString)
        val res = parseFile(infile)
        var namespace = infile.path.segments.toList
          .dropWhile(_ != "specification")
          .drop(1)
          .head
        if (infile.path.segments.toList.last.endsWith("Request.ts")) namespace = namespace + ".requests"
      if (infile.path.segments.toList.last.endsWith("Response.ts")) namespace = namespace + ".responses"
      res match {
        case Left(value) =>
          println(value)
        case Right(value) =>
//                     println(value)
          parserContext.parse(infile, namespace, value)
      }

      }

  parserContext.scalaClasses
    .map(_._2.toCode)
    .groupBy(_.filename)
    .foreach { case (filename, codes) =>
      var targetFileDir = targetDir
      var finalName: String =
        "zio/" + filename.getOrElse("common/common.scala").replace(".", "/").replace("/scala", ".scala")
      val packg = finalName.split("/").dropRight(1).mkString(".")
      finalName.split('/').foreach { value =>
        if (value.endsWith(".scala")) {
          finalName = value
        } else {
          targetFileDir = targetFileDir / value
        }
      }
      os.makeDir.all(targetFileDir)
      val text     = codes.flatMap(_.code).mkString("\n")
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
