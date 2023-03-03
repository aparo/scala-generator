/*
 * Copyright 2018-2022 Alberto Paro on Apache 2 License. All Rights Reserved.
 */

package generator

import java.io._
import java.nio.file.Paths

import scala.io.Source._
import scala.io.{Codec, Source}
import scala.language.implicitConversions

object FileUtils {

  /** A wrapper around file, allowing iteration either on direct children or on directory tree
    */
//  extension (file: File) {
  implicit class RichFile(file: File) {

    def children = new Iterable[File] {
      def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }

    def andTree: Iterable[File] =
      Seq(file) ++ children.flatMap(child => child.andTree)
  }

  def getCurrentDirectory = Paths.get("").toAbsolutePath.toString

  def searchFile(baseDir: String, fileToSearch: String): Option[File] =
    new File(baseDir).andTree.find(_.getName.equalsIgnoreCase(fileToSearch))

  /*
  import java.io._
  val data = Array("Five","strings","in","a","file!")
  printToFile(new File("example.txt"))(p => {
    data.foreach(p.println)
  })
   */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(f)
    try
      op(p)
    finally
      p.close()
  }

  def readResource(name: String, encoding: String = "utf8"): Option[String] =
    try {

      val source = scala.io.Source.fromInputStream(getClass.getResourceAsStream(name), encoding)
      val res    = source.mkString
      source.close
      Some(res)
    } catch {
      case ex: Throwable =>
        println(s"Unable to read resource $name: $ex")
        None
    }

//  def readResourceJSON(name: String, encoding: String = "utf8"): Option[Json] = {
//    readResource(name, encoding)(v => io.circe.parser.parse(v))
//  }

  implicit val codec: Codec = Codec.UTF8

  def readUtf8(filename: String): String = Source.fromFile(filename).getLines().mkString

  def writeData(file: File, data: String): Boolean =
    writeSafe(file) { writer =>
      writer.write(data)
    }

  def writeLines(file: File, lines: Iterable[String]): Boolean =
    writeSafe(file) { writer =>
      lines.foreach { line =>
        writer.write(line)
        writer.newLine()
      }
    }

  /** Method that write into a temporary files, then moves it to replace to original for 'safe' write
    *
    * @param file
    *   file to write to
    * @param writeFn
    *   wrapped function taking BufferedWriter as a parameter that actually writes to the file
    * @return
    *   true if success, false otherwise
    */
  private def writeSafe(file: File)(writeFn: (BufferedWriter) => Unit): Boolean = {
    val tmpFileName = s"${file.getPath}.tmp"
    try {
      val tmpFile = new File(tmpFileName)

      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(tmpFileName), "UTF-8"))
      try {
        writeFn(writer)
        writer.flush()
      } finally writer.close()

      val status = tmpFile.renameTo(file)
      if (!status) println(s"Unable to rename $tmpFile to $file")
      status
    } catch {
      case e: Exception =>
        println(s"Error when trying to write to ${file.getPath}. $e")
        false
    }
  }

  // def readLines[T](file: File)(readFn: (Iterator[String]) => T): T = {
  //   val source = fromFile(file)
  //   readFromSource(source)(readFn)
  // }

  // def readLinesGzip[T](file: File)(readFn: (Iterator[String]) => T): T = {
  //   val source = fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(file))))
  //   readFromSource(source)(readFn)
  // }

  // private def readFromSource[T](s: Source)(block: (Iterator[String]) => T): T = {
  //   val iteratorWithClosable = new Iterator[String] with Closable {
  //     val lines = s.getLines()

  //     override def hasNext: Boolean = lines.hasNext

  //     override def close(): Unit = s.close()

  //     override def next(): String = lines.next()
  //   }

  //   using(iteratorWithClosable)(block)
  // }

  // // Code extracted from File.Java to determine the tmpdir on the system.
  // def getTmpDirectory: File = new File(AccessController.doPrivileged(new GetPropertyAction("java.io.tmpdir")))

  // implicit def closingSource(source: Source) = new {
  //   val lines  = source.getLines()
  //   var isOpen = true

  //   def closeAfterGetLines() = new Iterator[String] {
  //     def hasNext = isOpen && hasNextAndCloseIfDone

  //     def next() = {
  //       val line = lines.next()
  //       hasNextAndCloseIfDone
  //       line
  //     }

  //     private def hasNextAndCloseIfDone =
  //       if (lines.hasNext) true
  //       else {
  //         source.close(); isOpen = false; false
  //       }
  //   }
  // }
}

/*
import io.utils.RichFile.toRichFile // this makes implicit toRichFile active
import java.io.File

object Test extends App {
  val root = new File("/home/user")
  for(f <- root.andTree) Console.println(f)

 // filtering comes for free
 for(f <- root.andTree; if f.getName.endsWith(".mp3")) Console.println(f)
}*
 * */
