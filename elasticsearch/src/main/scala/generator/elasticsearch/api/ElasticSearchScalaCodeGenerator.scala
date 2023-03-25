/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import generator.PathUtils
import generator.elasticsearch.{Constants, DevConfig}
import generator.ts.Converters._
import generator.ts.{CodeData, ParserContext}
import os.Path
import zio.ZIO

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
class ElasticSearchScalaCodeGenerator(val devConfig: DevConfig) extends BaseCodeGenerator {
  implicit lazy val parserContext = new ParserContext

  lazy val elasticFiles =
    os.walk(devConfig.devESSourcePath, skip = { f => f.last.endsWith(".java") }).toList
  val destDir: os.Path = devConfig.devScalaAPIDestPath / "src" / "main" / "scala" / "zio" / "elasticsearch"
  if (!os.exists(destDir))
    os.makeDir.all(destDir)
  val requestResponse = new ListBuffer[(String, String)]
  var nativeFiltering = false

  def run(): ZIO[Any, Throwable, Unit] =
//    runMappings()
//    runTypes()
    runREST()

  def runREST(): ZIO[Any, Throwable, Unit] = {
    for {
      _ <- ZIO.logInfo("Parsing Classes")
      _ <- parseEntities
      _ <- ZIO.logInfo("Parsing REST")
      apisUnsorted <- ZIO.foreach(
        apiFiles,
        // .filter(_.last == "get.json")
      )(f => processFile(f))
      apis = apisUnsorted.flatten.sortBy(_.name)
//      esActions = elasticFiles
//        .filter(_.toString().contains("/action/"))
//        .filterNot(_.toString().contains("/post/"))
//        .filterNot(_.toString().contains("/rest/"))
//        .filter(_.last.endsWith("Action.java"))
//        .map(_.last.split('.').head)
//        .filterNot(_.startsWith("Abstract"))
//        .filterNot(_.startsWith("Node"))
//        .filterNot(_.endsWith("AsyncAction"))
//        .filterNot(_ == "Action")
//        .filterNot(_ == "GenericAction")
      currentActions = apis.flatMap(_.nativeAction)
//      missing        = esActions.toSet -- currentActions.map(_.split('.').last).toSet
      // generate Request
      requests <- ZIO.foreach(apis) {apiEntry => ZIO.attempt(apiEntry.generateRequest) }
        // generate Response
      responses <- ZIO.foreach(apis) {apiEntry =>ZIO.attempt(apiEntry.generateResponse) }
      extraAPIClasses = apis.flatMap(_.extraClassCodes)

//        zioAccessManagers <- generateApis(apis)
//      _ <- generateManagers()
////      _ <- ZIO.attempt(generateZioAccessors(zioAccessManagers))
//      _ <- generateClientActions(destDir, apis)
//      _ <- generateClientActionResolver(destDir, apis)
//      _ <- generateEnumeration()
// generating classes
      typesCodes = parserContext.scalaClasses
        .filterNot(_._1.endsWith("Response"))
        .filterNot(_._1.endsWith("Request"))
        .map(_._2.toCode)
      fileCodes=(requests ++ responses ++ typesCodes ++ extraAPIClasses)
        .groupBy(_.filename
//          .getOrElse("common/common.scala")
          .replace(".", "/")
          .replace("_types/ElasticSearch", "common")
          .replace("_spec_utils", "common")
          .replace("_types", "common")
        )
      _ <- generateTsClasses(fileCodes)
    } yield ()
  }

  def generateEnumeration(): ZIO[Any, Throwable, Unit] = for {
    _ <- ZIO.logDebug(s"Generating new files in ${destDir}")
    _ <- ZIO.attempt(
      PathUtils.saveScalaFile(
        (List(s"package ${Constants.namespaceName}.client") ++ extras.values).mkString("\n\n"),
        destDir / "enumerations.scala",
      ),
    )
  } yield ()

  def generateManagers(): ZIO[Any, Throwable, Unit] =
    ZIO.foreachDiscard(managers) { case (name, code) =>
      ZIO.attempt(
        PathUtils.saveScalaFile(
          elasticsearch.managers.txt
            .Manager(
              Constants.namespaceName,
              managersImports.getOrElse(name, Nil).distinct.sorted,
              name,
              name.toCamelUpper,
              code,
            )
            .toString(),
          destDir / name / s"${name.toCamelUpper}Manager.scala",
        )
      )
    }

  def generateApis(
      apis:                 Seq[APIEntry],
  ): ZIO[Any, Throwable, Map[String, String]] = ZIO.attempt {
    val zioAccessManagers = new mutable.HashMap[String, String]

    apis.foreach { apiEntry =>

      val client = apiEntry.scope
      requestResponse += s"zio.elasticsearch.$client.requests.${apiEntry.scalaRequest}" -> s"zio.elasticsearch.$client.responses.${apiEntry.scalaResponse}"
      val extra            = apiEntry.extra
      val implicitsList    = apiEntry.implicits
      val code             = apiEntry.getClientCalls
      val zioAccessMethods = apiEntry.getClientZIOAccessorsCalls
      val imports = List(
        s"zio.elasticsearch.$client.requests.${apiEntry.scalaRequest}",
        s"zio.elasticsearch.$client.responses.${apiEntry.scalaResponse}",
      )
      extras ++= extra
      if (managers.contains(client)) {
        managers += client -> (managers(client) + "\n\n" + code)
        zioAccessManagers += client -> (zioAccessManagers(client) + "\n\n" + zioAccessMethods)
        managersImports += client -> (managersImports(client) ++ imports)
        implicits += client -> (implicits(client) ++ implicitsList)
      } else {
        managers += client -> code
        zioAccessManagers += client -> zioAccessMethods
        managersImports += client -> imports
        implicits += client -> implicitsList
      }
    }
    zioAccessManagers.toMap

  }


  def generateClientActions(destDir: Path, apis: List[APIEntry])(implicit
      parserContext:                 ParserContext,
  ): ZIO[Any, Throwable, Unit] = ZIO.attempt {
    val filename = destDir / "client" / "ClientActions.scala"
    var toImport = Set.empty[String]
    requestResponse.foreach { case (req, res) =>
      toImport += req
      toImport += res
    }
    val generated =
      elasticsearch.client.txt
        .ClientActions(Constants.namespaceName, toImport.toList.sorted, requestResponse.toList)
        .toString()
    PathUtils.saveScalaFile(generated, filename)
  }

  def generateClientActionResolver(destDir: Path, apis: List[APIEntry])(implicit
      parserContext:                        ParserContext,
  ): ZIO[Any, Throwable, Unit] = ZIO.attempt {
    val filename = destDir / "client" / "ClientActionResolver.scala"
    var toImport = Set.empty[String]
    requestResponse.foreach { case (req, res) =>
      toImport += req
      toImport += res
    }
    val generated =
      elasticsearch.client.txt
        .ClientActionResolver(Constants.namespaceName, toImport.toList.sorted, requestResponse.toList)
        .toString()
    PathUtils.saveScalaFile(generated, filename)

  }

  def runMappings(): ZIO[Any, Throwable, Unit] =
    for {
      apis <- ZIO.foreach(mappingFiles)(processFile)
    } yield ()

  def generateZioAccessors(zioAccessManagers: mutable.HashMap[String, String]) =
    zioAccessManagers.foreach { case (name, code) =>
      val filename = destDir / name / s"${name.toCamelUpper}Accessors.scala"
      PathUtils.saveScalaFile(
        code.replace("%%SERVICE%%", s"${name.toCamelUpper}Service"),
        filename,
      )
    }

  def parseEntities: ZIO[Any, Throwable, Unit] =
    ZIO.foreachDiscard(
      tsFiles,
      // .filter(_.last == "get.json")
    ) { f =>
      import org.scalablytyped.converter.internal._
      import org.scalablytyped.converter.internal.ts.parser._
      println(s"Parsing: $f")
      parseFile(InFile(f)) match {
        case Left(value) =>
          ZIO.fail(new RuntimeException(value))
        case Right(value) =>
          var namespace = f.segments.toList
            .dropWhile(_ != "specification")
            .drop(1)
            .head
          if (f.segments.toList.last.endsWith("Request.ts")) namespace = namespace + ".requests"
          if (f.segments.toList.last.endsWith("Response.ts")) namespace = namespace + ".responses"

          ZIO.attempt(parserContext.parse(InFile(f), namespace, value))
      }
    }

  def generateTsClasses(fileCodes:Map[String, List[CodeData]])=
  ZIO.foreachDiscard(fileCodes) { case (filename, codes) =>
    var targetFileDir = destDir
    var finalName: String = filename.replace(".", "/").replace("/scala", ".scala")
    val packg = "zio.elasticsearch." + finalName.split('/').dropRight(1).mkString(".")
    finalName.split('/').foreach { value =>
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
    if (text.contains(" ElasticSearch ")) includes += "import zio.elasticsearch._"
    if (text.contains("Json")) includes += "import zio.json.ast._"
    if (text.contains("Chunk[")) includes += "import zio._"
    if (text.contains("OffsetDateTime") || text.contains("LocalDateTime")) includes += "import java.time._"
    val finalIncludes = includes.toSet.toList.sorted.mkString("\n")
//    os.write.over(targetFileDir / finalName, s"package $packg\n$finalIncludes\n$text")
    PathUtils.saveScalaFile(
      s"package $packg\n$finalIncludes\n$text",
      targetFileDir / finalName
    )
    ZIO.logInfo(s"Written ${targetFileDir / finalName}")
  }

}
