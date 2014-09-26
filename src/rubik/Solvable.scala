package rubik

import scala.io.Source
import scala.util.{Try,Success,Failure}

/**
 * Starting point for the Rubik's Cube AI.
 * 
 * @author  John Paul Welsh
 */
object run {
  
  def main(args: Array[String]) {
    
    def attemptToReadFile(filename: String): Try[Array[String]] = {
      Try(Source.fromFile(filename).getLines.map(x => x.trim()).toArray)
    }
    
//    // Current path to the file is ../../initial.txt
//    val filename = args(0)
//    // Try: read in the text file, make it a List out of the lines of the file, trim each line, and make it an Array
//    // If it failed, print false, otherwise assign it to a variable
//    attemptToReadFile(filename) match {
//      case Failure(f)     => println(false)
//      case Success(lines) => {
//        if (lines.isEmpty) println(false)
//        else {
//          val cube = common.arrangeInput(lines)
//          val solvedCube = common.setSolvedCube
//          println(CubeValidator.isValidCube(lines, cube, solvedCube)) 
//        }
//      }
//    }
    
    // Current path to the file is ../../initial.txt
    var count = 0
    for (i <- 0 to 1999) {
      val filename = args(0) + "test" + i + ".txt"
      // Try: read in the text file, make it a List out of the lines of the file, trim each line, and make it an Array
      // If it failed, print false, otherwise assign it to a variable
      attemptToReadFile(filename) match {
        case Failure(f)     => println(false)
        case Success(lines) => {
          if (lines.isEmpty) println(false)
          else {
            val cube = common.arrangeInput(lines)
            val solvedCube = common.setSolvedCube
            //if (!(CubeValidator.isValidCube(lines, cube, solvedCube))) println("wrong at test " + i)
            if (!(CubeValidator.isValidCube(lines, cube, solvedCube))) count += 1
          }
        }
      }
    }
    //println(count)
  }
}