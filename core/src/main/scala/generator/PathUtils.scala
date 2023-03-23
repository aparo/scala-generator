/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator

import com.github.difflib.DiffUtils
import com.github.difflib.patch.DeltaType

import scala.jdk.CollectionConverters._
import scala.util.Try
import os.Path

/** Common functionalities for working with the paths
  */
object PathUtils {

  /** Find a file in this path of recurvicely in subpaths
    * @param startPath
    *   startPath
    * @param name
    *   name of the file
    * @return
    *   if the faile is found
    */
  def findFile(name: String, startPath: Path = os.pwd): Option[Path] = {
    val curr = startPath / name
    if (os.exists(curr))
      Some(curr)
    else
      (startPath / os.up) match {
        case os.root => None
        case value   => findFile(name, value)
      }
  }

  def recursivelyListFiles(f: java.io.File): Array[java.io.File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursivelyListFiles)
  }

  def getScala(paths: Array[String]) =
    paths
      .flatMap { dir =>
        recursivelyListFiles(new java.io.File(dir))
      }
      .filter(_.getName.endsWith(".scala"))
      .toList

  def expandFiles(list: List[Path]): List[Path] =
    expandFiles(
      list,
      _ => true,
      Set("target", "tmp", ".metals", ".bloop"),
    )

  def expandFiles(
      list:               List[Path],
      filter:             Path => Boolean,
      invalidDirectories: Set[String],
  ): List[Path] =
    list.flatMap { file =>
      if (os.isDir(file))
        os.walk(file)
          .toList
          .filterNot(f => invalidDirectories.exists(d => f.toString().contains(d)))
          .filter(f => os.isFile(f))
          .filter(filter)
      else if (os.isFile(file) && filter(file)) {
        Nil
      } else List(file).filter(filter)
    }

  def updateFileIfNeeded(file: Path, content: String): Unit = {
    if (os.exists(file)) {
      val oldContent = os.read(file)
      if (oldContent != content) {
        os.write.over(file, content, createFolders = true)
      }
    } else {
      os.write(file, content, createFolders = true)
    }
    ()
  }

  def save(file: Path, content: String, templateFlags: List[String] = Nil): Unit =
    if (os.exists(file) && templateFlags.nonEmpty) {
      val result = mergeTemplate(
        os.read.lines(file).toList,
        content.split('\n').toList,
        templateFlags = templateFlags,
      )
      PathUtils.updateFileIfNeeded(file, result.mkString("\n"))
    } else {
      PathUtils.updateFileIfNeeded(file, content)
    }

  def mergeTemplate(
      original:      List[String],
      generated:     List[String],
      templateFlags: List[String] = Nil,
  ): List[String] = {

    val patch  = DiffUtils.diff(original.asJava, generated.asJava)
    var result = original

    // simple output the computed patch to console
    val deltas  = patch.getDeltas.asScala.reverse.toList
    var nextpos = 0
    deltas.foreach { delta =>
      delta.getType match {
        case DeltaType.CHANGE =>
          val position = delta.getSource.getPosition
          result = result.take(position) ++ delta.getTarget.getLines.asScala ++ result.drop(position)
        case DeltaType.DELETE =>
          val position = delta.getSource.getPosition
          // check sources
          val lines      = delta.getSource.getLines.asScala.toList
          var inTemplate = false
          if (lines.nonEmpty) {
            val h = lines.head
            if (templateFlags.contains(h)) {
              inTemplate = true
              nextpos = position + lines.length
            }
          }
          if (position == nextpos + 1) {
            inTemplate = true
          }

          if (!inTemplate) {
            result = result.take(position) ++ result.drop(position + lines.length)
          } else {
            // if we are in template we skip removing
          }
        case DeltaType.INSERT =>
          val position = delta.getSource.getPosition
//          if(templateFlags.isEmpty){
          // all new code is inderted
          result = result.take(position) ++ delta.getTarget.getLines.asScala ++ result.drop(position)
//          }

        case DeltaType.EQUAL =>
      }
    }
    result
  }

  /** Save a given content in a file if the content is different
    * @param content
    *   the content to be saved
    * @param file
    *   the file to be saved
    */
  def saveScalaFile(content: String, file: Path): Unit = {
     val result = Try {
       org.scalafmt.Scalafmt.format(content).get
     }.toOption.getOrElse(content)
    PathUtils.updateFileIfNeeded(file, result)
//    logger.info(s"Generated $file")
  }

  /** Add the component of a namespace to a directory name returning the new build path.
    *
    * @param baseDir
    *   the base directory to be used
    * @param namespace
    *   a Scala/Java namespace
    * @returnthe
    *   new build path
    */
  def buildDirectoryFromNamespace(baseDir: Path, namespace: String): Path = {
    var result = baseDir
    namespace.split('.').foreach(n => result = result / n)
    result
  }

}
