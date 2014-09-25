package rubik

import scala.io.Source

/**
 * Starting point for the Rubik's Cube AI.
 * 
 * @author  John Paul Welsh
 */
object run {

  /**
   * File reading taken from http://alvinalexander.com/scala/scala-how-open-read-files-scala-examples
   */
  def main(args: Array[String]) {
    // Current path to the file is ../../initial.txt
    val filename = args(0)
    // Read in the text file, make it a List out of the lines of the file, trim each line, and make it an Array
    val filelines: Array[String] = io.Source.fromFile(filename).getLines.map(x => x.trim()).toArray
    // Put the input characters into a Cube
    var cube = common.arrangeInput(filelines)
    // Set up a prototype of a Cube in its solved state
    var solvedCube = common.setSolvedCube
    
    println("Valid color counts? " + CubeValidator.hasValidNumOfColors(filelines))
    println("Valid cube? " + CubeValidator.isValidCube(filelines, cube, solvedCube))
  }
}