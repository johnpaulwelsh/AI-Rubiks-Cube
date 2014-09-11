package rubik

import scala.io.Source
import common._

/**
 * Starting point for the Rubik's Cube AI.
 * @author: John Paul Welsh
 */
object run {

  /**
   * File reading taken from http://alvinalexander.com/scala/scala-how-open-read-files-scala-examples
   */
  def main(args: Array[String]): Unit = {
    // ../../initial.txt
    val filename = args(0)
    val filelines = io.Source.fromFile(filename).getLines.toArray
    var cube = arrangeInput(filelines)
    val cubeValidator = new CubeValidator
    printCube(cube)
    println(cubeValidator.isValidCube(cube))
  }
}