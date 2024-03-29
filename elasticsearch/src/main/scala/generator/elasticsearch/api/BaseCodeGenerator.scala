/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import zio.json.JsoniterScalaCodec._

import scala.collection.mutable
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.elasticsearch.DevConfig
import generator.ts.ParserContext
import zio._
trait BaseCodeGenerator {

  def devConfig: DevConfig

  // DONE autoscaling ingest
  lazy val todoModule = "async_search"

  lazy val root = devConfig.devRestAPIPath / "api"

  val managersImports = new mutable.HashMap[String, List[String]]
  val managers        = new mutable.HashMap[String, String]
  val extras          = new mutable.HashMap[String, String]
  val implicits       = new mutable.HashMap[String, List[String]]
  lazy val apiFiles = os
    .walk(root)
    .filter(os.isFile(_, followLinks = false))
    .filter(_.last.endsWith(".json"))
//    .filter(f => f.last.startsWith(todoModule))
    .toList

  lazy val tsFiles = os
    .walk(devConfig.specAPIPath / "specification")
    .filter(os.isFile(_, followLinks = false))
    .filter(_.last.endsWith(".ts"))
//    .filterNot(_.toString.contains("_types")) // we skip for now
    .filterNot(_.toString.contains("mapping")) // we skip for now
    .filterNot(_.toString.contains("aggregations/pipeline.ts")) // we skip for now
//    .filter(f =>
//      f.toString.contains(todoModule) ||
//        f.toString.contains("Base.ts") || // requests
//        f.toString.contains("behaviors.ts"),
//    )
    .toList

  lazy val mappingFiles =
    os.walk(devConfig.devRestAPIPath / "mappings", skip = { f => !f.last.endsWith(".json") }).toList

  def run(): ZIO[Any, Throwable, Unit]

  lazy val FIX_API_NAMES:Map[String,String] = Map(
//    "ingest.simulate" -> "ingest.simulate_pipeline"
  )

  def fixAPIName(str:String):String=if(str.contains(".")){
    str
  } else s"common.$str"

  protected def processFile(
      name:                 os.Path,
  )(implicit parserContext: ParserContext): ZIO[Any, Throwable, Seq[ApiEntryTyped]] =
    if (name.baseName.startsWith("_")) ZIO.succeed(Nil)
    else {
      for {
        _ <- ZIO.debug(s"Processing $name")
        obj <- ZIO.attempt {
          readFromStream[Map[String, APIEntry]](name.getInputStream).map { v =>
            ApiEntryTyped.parse(v._2.copy(name = fixAPIName(v._1)))
          }
        }
      } yield obj.toSeq
    }

}
