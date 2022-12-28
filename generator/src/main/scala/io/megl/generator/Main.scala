// package io.megl.generator

// import ammonite.ops._
// import org.scalablytyped.converter.internal._
// import org.scalablytyped.converter.internal.ts.parser._
// object Main extends App {
//   val specification = os.home / "projects" / "github" / "elasticsearch" / "elasticsearch-specification" / "specification"
//   val tsFiles =
//     (ls.rec ! specification)
//       .filter(_.isFile)
//       .filter(_.toString().endsWith(".ts"))
//       .sorted
//       .map(p => InFile(p))
//   // tsFiles.foreach(p => println(p))

//     tsFiles
//     .take(13)
//     .foreach{
//         infile =>
//             println(infile.toString)
//             val res=parseFile(infile)
//             res match {
//                 case Left(value) =>
//                     println(value)
//                 case Right(value) =>
//                     println(value)
//                     Render.parse(value)
//             }

//     }

//   println("Completed")
// }
