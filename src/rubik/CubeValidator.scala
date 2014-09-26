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
     * Test 1 of 5: center cubes.
     * 
     * @param ls  the original input from the text file, representing a cube state
     * @return  true if the centers are right, false otherwise
     */
    def hasValidCenters(): Boolean = {
      val centerList: List[Char] = List(filelines(1).charAt(1)) :::
                                   List(filelines(4).charAt(1)) :::
                                   List(filelines(4).charAt(4)) :::
                                   List(filelines(4).charAt(7)) :::
                                   List(filelines(7).charAt(1)) :::
                                   List(filelines(10).charAt(1))
      centerList == List('R', 'G', 'Y', 'B', 'O', 'W')
    }
    
    /**
     * Test 2 of 5: number of each color.
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
     * Test 3 of 5: permutation parity.
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
     * Gets the parity of a Cubie.
     * 
     * We only care about where the up-down color is (which axis it's currently on).
     * If it's up or down, we're good so give it a 0. If it isn't, how many clockwise 120-degree turns
     * does it take to fix it back to solved? Try again with a shifted Cubie and an incremented parity.
     * The shift direction is determined by complicated booleans that would be easier if I just sent in
     * the Cubie's index, but oh well.
     * 
     * @param corner      the input Cubie
     * @param isLeftSide  whether this Cubie is on the left side of the Cube (when yellow faces toward me and red faces up)
     * @param isOnFront   whether this Cubie is on the front side of the Cube
     * @param isTop       whether this Cubie is on the top side of the Cube
     * @return  the parity value for the Cubie
     */
    def getCornerParity(corner: Cubie, isLeftSide: Boolean, isOnFront: Boolean, isTop: Boolean): Int = {
      // This represents one clockwise turn, but is different depending on which side of the Cube
      // our Cubie is (from a fixed perspective of yellow in front, red on top)
      def shiftOnLeft(c: Cubie): Cubie  = Array(c(2), c(0), c(1))
      def shiftOnRight(c: Cubie): Cubie = Array(c(1), c(2), c(0))
      
      def recurse(corner: Cubie, parity: Int): Int = {
        if (corner(1) == 'R' || corner(1) == 'O') parity
        else if (isLeftSide  && isOnFront  && isTop) recurse(shiftOnLeft(corner), parity+1)
        else if (isLeftSide  && !isOnFront && isTop) recurse(shiftOnRight(corner), parity+1)
        else if (!isLeftSide && isOnFront  && isTop) recurse(shiftOnRight(corner), parity+1)
        else if (!isLeftSide && !isOnFront && isTop) recurse(shiftOnLeft(corner), parity+1)
        else if (isLeftSide  && isOnFront  && !isTop) recurse(shiftOnRight(corner), parity+1)
        else if (isLeftSide  && !isOnFront && !isTop) recurse(shiftOnLeft(corner), parity+1)
        else if (!isLeftSide && isOnFront  && !isTop) recurse(shiftOnLeft(corner), parity+1)
        else if (!isLeftSide && !isOnFront && !isTop) recurse(shiftOnRight(corner), parity+1)
        else -1
      }
      recurse(corner, 0)
    }
    
    /**
     * Test 4 of 5: corner parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidCornerOrientationParity(): Boolean = {
      val inputCorners: Array[Cubie]  = getCubeCornerList(input)
      val solvedCorners: Array[Cubie] = getCubeCornerList(solved)
      var totalCornerParity = 0
        
      for (inCubie <- inputCorners) {
        var orientationList: Array[Cubie] = Array()
                
        val isLeftSideCubie = (inputCorners.indexOf(inCubie) == 0 ||
                               inputCorners.indexOf(inCubie) == 2 ||
                               inputCorners.indexOf(inCubie) == 4 ||
                               inputCorners.indexOf(inCubie) == 6)
                               
        val isOnFrontOfCube = (inputCorners.indexOf(inCubie) == 2 ||
                               inputCorners.indexOf(inCubie) == 3 ||
                               inputCorners.indexOf(inCubie) == 6 ||
                               inputCorners.indexOf(inCubie) == 7)
                               
        val isOnTopOfCube   = (inputCorners.indexOf(inCubie) >= 0 &&
                               inputCorners.indexOf(inCubie) <= 3)
                
        totalCornerParity += getCornerParity(inCubie, isLeftSideCubie, isOnFrontOfCube, isOnTopOfCube)
      } // end outer 'for' loop
      
      // Returns whether the total parity is divisible by 3
      totalCornerParity % 3 == 0
    }
    
    // I DONT KNOW WHAT THIS NEEDS TO DO
    def getSideOrientations(side: Cubie): Array[Cubie] = {
      null
    }
    
    /**
     * Test 5 of 5: side parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidSideOrientationParity(): Boolean = {      
      true
    }
    
    // Returns true when all tests pass, false if one or more fails
    println("Valid # of colors: " + hasValidNumOfColors)
    println("Valid centers: " + hasValidCenters)
    println("Permutation test: " + isValidPermutationParity)
    println("Corner parity test: " + isValidCornerOrientationParity)
    isValidPermutationParity &&
    isValidCornerOrientationParity
    //isValidSideOrientationParity
  }
}