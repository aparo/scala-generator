/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator.elasticsearch.api

import zio.json.JsoniterScalaCodec._
import scala.collection.mutable
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import generator.elasticsearch.DevConfig
import zio._
trait BaseCodeGenerator {

  def devConfig: DevConfig

  val root = devConfig.devRestAPIPath / "api"

  val managersImports = new mutable.HashMap[String, List[String]]
  val managers        = new mutable.HashMap[String, String]
  val extras          = new mutable.HashMap[String, String]
  val implicits       = new mutable.HashMap[String, List[String]]
  var files           = os.walk(root, skip = { f => !f.last.endsWith(".json") })
  lazy val mappingFiles =
    os.walk(devConfig.devRestAPIPath / "mappings", skip = { f => !f.last.endsWith(".json") }).toList

  def run(): ZIO[Any, Throwable, Unit]

  protected def processFile(name: os.Path): ZIO[Any, Throwable, Seq[APIEntry]] =
    if (name.baseName.startsWith("_")) ZIO.succeed(Nil)
    else {
      for {
        _ <- ZIO.debug(s"Processing $name")
        obj <- ZIO.attempt(readFromStream[Map[String, APIEntry]](name.getInputStream))
      } yield obj.map(v => v._2.copy(name = v._1)).toSeq
    }

}
