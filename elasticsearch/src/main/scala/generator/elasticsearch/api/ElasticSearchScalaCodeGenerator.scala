/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import generator.PathUtils
import generator.elasticsearch.{Constants, DevConfig}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import os.Path
import zio.ZIO
import generator.ts.Converters._
class ElasticSearchScalaCodeGenerator(val devConfig: DevConfig) extends BaseCodeGenerator {

  var nativeFiltering = false

  val destDir: os.Path = devConfig.devScalaAPIDestPath / "src"/"main"/"scala"/"zio"/"elasticsearch"
  if(!os.exists(destDir))
    os.makeDir.all(destDir)

  lazy val elasticFiles =
    os.walk(devConfig.devESSourcePath, skip = { f => f.last.endsWith(".java") }).toList

  val requestResponse = new ListBuffer[(String, String)]

  def run(): ZIO[Any, Throwable, Unit] =
//    runMappings()
    runREST()
  def runMappings(): ZIO[Any, Throwable, Unit] = {
    for {
      apis <- ZIO.foreach(mappingFiles)(processFile)
    } yield ()

  }
  def runREST(): ZIO[Any, Throwable, Unit] = {

    for {
      apisUnsorted <- ZIO.foreach(files
       // .filter(_.last == "get.json")
      )(processFile)
      apis=apisUnsorted.flatten.toList.sortBy(_.name)
      esActions = elasticFiles
        .filter(_.toString().contains("/action/"))
        .filterNot(_.toString().contains("/post/"))
        .filterNot(_.toString().contains("/rest/"))
        .filter(_.last.endsWith("Action.java"))
        .map(_.last.split('.').head)
        .filterNot(_.startsWith("Abstract"))
        .filterNot(_.startsWith("Node"))
        .filterNot(_.endsWith("AsyncAction"))
        .filterNot(_ == "Action")
        .filterNot(_ == "GenericAction")
      currentActions = apis.flatMap(_.nativeAction)
      missing = esActions.toSet -- currentActions.map(_.split('.').last).toSet
      zioAccessManagers=generateApis(apis)
      _ <- ZIO.attempt(generateManagers())
      _ <- ZIO.attempt(generateZioAccessors(zioAccessManagers))
      _ <- ZIO.attempt (generateClientActions (destDir, apis))
      _ <- ZIO.attempt (  generateClientActionResolver (destDir, apis))
      _ <- ZIO.attempt (  generateEnumeration())

    } yield ()


//    val actionToAdd = missing.flatMap { toAdd =>
//      elasticFiles
//        .filter(_.toString().contains("/action/"))
//        .find(f => f.toString().contains(s"$toAdd.java") && !f.toString().contains("Transport"))
//        .map { file =>
//          file.toString().split("/java/").last.replace(".java", "").replace("/", ".")
//        }
//    }
//    actionToAdd.toList.sorted.foreach{
//      actionName=>
//        println(s"""
//      "native_action":"$actionName",
//      "native_request":"${actionName.replace("Action", "Request")}",
//      "native_response":"${actionName.replace("Action", "Response")}",
//        """)
//    }

//    // generate ActionMagner -- no more used
//    val actionMTemplate = elasticsearch.client.txt.ActionMagnet
//    val actions         = apis.flatMap(_.nativeAction).toSet.toList.sorted
//    PathUtils.saveScalaFile(actionMTemplate.render(actions).toString(), destDir / "elasticsearch" / "client"/ "ActionMagnet.scala")






//
//    fw = new FileWriter(new File(destDir, "ImplicitsForManagers.scala"))
//    fw.write(s"package ${Constants.namespaceName}")
//    fw.write("\n\n")
//    fw.write("import io.circe._\n")
//    fw.write("package object client {")
//    implicits.foreach {
//      case (name, imps) =>
//        imps.foreach { name =>
//          val impName = name(0).toLower + name.substring(1) + "Fmt"
//          fw.write(s"    implicit val $impName = Json.format[$name]\n")
//        }
//    }
//    fw.write("\n\n}\n\n")
//    fw.close()

  }

  def generateEnumeration()={
    println(s"Generating new files in ${destDir}")
    PathUtils.saveScalaFile(
      (List(s"package ${Constants.namespaceName}.client") ++ extras.values).mkString("\n\n"),
      destDir / "enumerations.scala",
    )

  }

  def generateManagers()={
    managers.foreach { case (name, code) =>
      val filename = destDir / name / s"${name.toCamelUpper}Manager.scala"
      PathUtils.saveScalaFile(
        elasticsearch.managers.txt
          .Manager(Constants.namespaceName, (managersImports.getOrElse(name, Nil)).distinct.sorted, name, name.toCamelUpper, code)
          .toString(),
        filename,
      )
    }
  }

  def generateZioAccessors(zioAccessManagers: mutable.HashMap[String, String]) = {
    zioAccessManagers.foreach { case (name, code) =>
      val filename = destDir / name / s"${name.toCamelUpper}Accessors.scala"
      PathUtils.saveScalaFile(
        code.replace("%%SERVICE%%", s"${name.toCamelUpper}Service"),
        filename,
      )
    }  }

  def generateApis(apis:Seq[APIEntry])={
    val zioAccessManagers = new mutable.HashMap[String, String]

    apis.foreach { apiEntry =>
      // generate Request
      generateRequest(destDir, apiEntry)
      // generate Response
      PathUtils.saveScalaFile(
        apiEntry.responseClass,
        destDir / s"${apiEntry.responseFilename.split("\\.", 1).last.replace(".", "/")}.scala".split("/").drop(2),
      )
      val client = apiEntry.scope

//      requestResponse += apiEntry.scalaRequest -> apiEntry.scalaResponse
      requestResponse += s"zio.elasticsearch.$client.requests.${apiEntry.scalaRequest}" -> s"zio.elasticsearch.$client.responses.${apiEntry.scalaResponse}"
      val extra = apiEntry.extra
      val implicitsList = apiEntry.implicits
      val code = apiEntry.getClientCalls
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
    zioAccessManagers

  }

  def generateClientActions(destDir: Path, apis: List[APIEntry]): Unit = {
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

  def generateRequest(destDir: Path, api: APIEntry): Unit = {
    val ddir = api.scope match {
      case "client" =>
        destDir / "requests"
      case default =>
        destDir / default / "requests"
    }
    if(!os.exists(ddir))
      os.makeDir.all(ddir)

    val filename = ddir / s"${api.scalaRequest}.scala"

    val namespace = api.scope match {
      case "client" =>
        s"${Constants.namespaceName}.requests"
      case default =>
        s"${Constants.namespaceName}.$default.requests"
    }

    var imports = new ListBuffer[String]

    if (api.scope != "client")
      imports += s"${Constants.namespaceName}.requests.ActionRequest"
    api.extra.foreach { case (k, v) =>
      imports += s"${Constants.namespaceName}.$k"
    }
    val rq        = api.generateRequest.mkString
    val generated = elasticsearch.txt.ScalaRequest(namespace, imports.toSet.toList.sorted, rq).toString()
    PathUtils.saveScalaFile(generated, filename)

  }

  def generateClientActionResolver(destDir: Path, apis: List[APIEntry]): Unit = {
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
}
