package rubik

import common._
import scala.util.control.Breaks._

/**
 * Collection of functions that check whether the input Cube is a valid arrangement.
 * 
 * @author  John Paul Welsh
 */
object CubeValidator {
  
  /**
   * The primary function to check the three parity checks against the input.
   * 
   * @param input   the cube we are checking for validity
   * @param solved  the solved state of the cube to compare 'input' against
   * @return  true if all tests pass, false otherwise
   */
  def isValidCube(filelines: Array[String], input: Cube, solved: Cube): Boolean = {
    
    /**
     * Test 1 of 4: center cubes.
     * 
     * @param ls  the original input from the text file, representing a cube state
     * @return  true if the centers are right, false otherwise
     */
    def isValidCenters(): Boolean = {
      val centerList: List[Char] = List(filelines(1).charAt(1)) :::
                                   List(filelines(4).charAt(1)) :::
                                   List(filelines(4).charAt(4)) :::
                                   List(filelines(4).charAt(7)) :::
                                   List(filelines(7).charAt(1)) :::
                                   List(filelines(10).charAt(1))
      centerList == List('R', 'G', 'Y', 'B', 'O', 'W')
    }
    
    /**
     * Determines whether the input file contains exactly 9 of the 6 valid colors of a Rubik's Cube
     * (R for red, Y for yellow, G for green, B for blue, O for orange, and W for white).
     * 
     * @param input  the input file as an array of strings (each entry is one row of the file)
     * @return  true if every character in the input is one of those 6, and there are only 9 of each,
     *          false otherwise
     */
    def hasValidNumOfColors(): Boolean = {
    
	  def recurse(flatArray: Array[Char], centerList: List[Char]): Boolean = {
	    if (centerList.isEmpty) true
	    else if (flatArray.count(_ == centerList.head) != 9) false
	    else recurse(flatArray, centerList.tail)
	  }
	  
	  recurse(filelines.flatten, List('R', 'G', 'Y', 'B', 'O', 'W'))
    }
    
    /**
     * For each element e, check how many elements between e
     * and the end of the sequence are < e.
     * 
     * @param seq  the sequence of indices that may have inversions
     * @return  number of inversions in the sequence
     */
    def inversionCount(seq: Array[Int]): Int = {
      var count = 0
      
      for (i <- 0 until seq.length) {
        for (j <- i until seq.length) {
          if (seq(j) < seq(i)) count += 1
        }
      }
      count
    }
    
    /**
     * Maps the location that a cubie is (in seqNow) to the location
     * it ought to be (gotten from seqSolved).
     * 
     * @param seqNow     the sequence of cubies from the input
     * @param seqSolved  the sequence of cubies in the solved state
     * @param accum      an array showing, for each element in seqNow, what element
     *                   it belongs to in the solved state
     * @return  the accumulated array of position indicies
     */
    def getCubieIndices(seqNow: Array[Cubie], seqSolved: Array[Cubie], accum: Array[Int]): Array[Int] = {
      
      def matchingCubies(input: Cubie, solved: Cubie): Boolean = {
        if (solved.isEmpty) true
        else if (!(input.deep.contains(solved.head))) false
        else matchingCubies(input, solved.tail)
      }
      
      for (i <- 0 until seqSolved.length) {
        for (j <- 0 until seqNow.length) {
          if (matchingCubies(seqSolved(i), seqNow(j))) {
            accum(j) = i
          }
        }
      }
      accum
    }
    
    /**
     * Test 2 of 4: permutation parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidPermutationParity(): Boolean = {
      val seq1now  = getCubeCornerList(input)
      val seq1goal = getCubeCornerList(solved)
      val seq2now  = getCubeSideList(input)
      val seq2goal = getCubeSideList(solved)
      
      // The initialized arrays are filled with zeroes -- this is very important regarding
      // the if-elseif-else statement below
      val seq1Indices = getCubieIndices(seq1now, seq1goal, Array.ofDim(8))
      val seq2Indices = getCubieIndices(seq2now, seq2goal, Array.ofDim(12))
      
      val sumInversions = inversionCount(seq1Indices) + inversionCount(seq2Indices)
      
      // If there is more than one 0 in either sequence, then there was at least one "missing"
      // sticker on a cubie, whether because it was replaced with an extra valid color, or
      // any invalid color
      if      (seq1Indices.toList.count(_ == 0) > 1) false
      else if (seq2Indices.toList.count(_ == 0) > 1) false
      else     sumInversions % 2 == 0
    }
    
    /**
     * Determines the valid orientations of the given corner Cubie.
     * 
     * @param corner  the Cubie element that we want the orientations for
     * @return  an array of all legal orientations of the Cubie
     *          (original, clockwise turn, anticlockwise turn)
     */
    def getCornerOrientations(corner: Cubie): Array[Cubie] = {
      var orients: Array[Cubie] = Array.ofDim(3)
      
      // The first thing you check is whether R or O is on the y-axis. If it is, 'corner' goes in element 0
      if (corner(1) == 'R' || corner(1) == 'O') orients(0) = corner
      else {
        
      }
      
      orients(0) = Array(corner(0), corner(1), corner(2))
      orients(1) = Array(corner(2), corner(0), corner(1))
      orients(2) = Array(corner(1), corner(2), corner(0))
      orients
    }
    
    // THIS IS NEEDED FOR PERMUTATION TEST
//    def getSideOrientations(side: Cubie): Array[Cubie] = {
//      kljasdf;lkjsfd
//    }
    
    /**
     * Test 3 of 4: corner parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidCornerOrientationParity(): Boolean = {
      val inputCorners: Array[Cubie]  = getCubeCornerList(input)
      val solvedCorners: Array[Cubie] = getCubeCornerList(solved)
      var totalCornerParity = 0
      var invalidOrientationFlag = false
        
      // Loop through list of input Cubies
      for (cubie <- inputCorners) {
        // Make new blank orientation list
        var orientationList: Array[Cubie] = Array()
        // Find the corresponding Cubie in the list from the solved Cube, and get its orientation list
        println("cubie: " + cubie.deep)
        for (i <- 0 until solvedCorners.length) {
          val orients = getCornerOrientations(solvedCorners(i))
          if (orients.deep.contains(cubie.deep)) orientationList = orients
        }
        println(orientationList.deep)
        // Assign a parity number to the Cubie based on which entry of the orientation list it matches
        val parityNum = {
          if      (orientationList.isEmpty)              -1
          else if (cubie.deep == orientationList(0).deep) 0
          else if (cubie.deep == orientationList(1).deep) 1
          else if (cubie.deep == orientationList(2).deep) 2
//          else if (cubie.deep == orientationList(3).deep) 3
          else -1
        }
        
        if (parityNum == -1) invalidOrientationFlag = true
        totalCornerParity += parityNum
      }
      
      println("Total corner parity: " + totalCornerParity)
      
      if (invalidOrientationFlag) false
      else totalCornerParity % 3 == 0
    }
    
    /**
     * Test 4 of 4: side parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidSideOrientationParity(): Boolean = {      
      true
    }
    
    // Returns true when all tests pass, false if one or more fails
    println("Easy tests: " + (hasValidNumOfColors && isValidCenters))
    println("Permutation test: " + isValidPermutationParity)
    isValidPermutationParity
    //isValidCornerOrientationParity &&
    //isValidSideOrientationParity
  }
}