package rubik

import common._

/**
 * Collection of functions that check whether the input Cube is a valid arrangement.
 * 
 * @author  John Paul Welsh
 */
object CubeValidator {

//  def hasValidNumOfColors(ls: Array[String]): Boolean = {
//    var flag = true
//    for (i <- List("R", "Y", "G", "B", "O", "W")) {
//      val x = ls.flatten
//      println(ls.count(_.split("").contains(i)))
//      flag = ls.count(_.split("").contains(i)) == 9
//    }
//    flag
//  }
  
  /**
   * The primary function to check the three parity checks against the input.
   * 
   * @param input   the cube we are checking for validity
   * @param solved  the solved state of the cube to compare 'input' against
   * @return        true if all tests pass, false otherwise
   */
  def isValidCube(filelines: Array[String], input: Cube, solved: Cube): Boolean = {
    
    /**
     * Test 1 of 4: center cubes.
     * 
     * @param ls  the original input from the text file, representing a cube state
     * @return    true if the centers are right, false otherwise
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
     * Maps the location that a cubie is (in seqNow) to the location
     * it ought to be (gotten from seqSolved).
     * 
     * @param seqNow     the sequence of cubies from the input
     * @param seqSolved  the sequence of cubies in the solved state
     * @param accum      an array showing, for each element in seqNow, what element
     *                   it belongs to in the solved state
     * @return           the accumulated array of position indicies
     */
    def getCubieIndices(seqNow: Array[Cubie], seqSolved: Array[Cubie], accum: Array[Int]): Array[Int] = {
      // Loop through every element in the input list
      for (i <- 0 until seqNow.length) {
        // Loop through every element in the solved list
        for (j <- 0 until seqSolved.length) {
          // If we get a match, put the solved element's index into the list
          // at the index where the input element is
          if (seqNow(i).deep == seqSolved(j).deep) {
            //println(i + " => " + j)
            accum(j) = i
          }
        }
      }
      //printIndices(accum)
      accum
    }
    
    /**
     * For each element e, check how many elements between e
     * and the end of the sequence are < e.
     * 
     * @param seq  the sequence of indices that may have inversions
     * @return     number of inversions in the sequence
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
      
      // If there is more than one 0 in either sequence, then there was at least one "missing"
      // sticker on a cubie, whether because it was replaced with an extra valid color, or
      // any invalid color
      if (seq1Indices.toList.count(_ == 0) > 1)      false
      else if (seq2Indices.toList.count(_ == 0) > 1) false
      else inversionCount(seq1Indices) + inversionCount(seq2Indices) % 2 == 0
    }
    
    /**
     * Test 3 of 4: corner parity.
     * 
     * @return  true if passed the test, false otherwise
     */
    def isValidCornerOrientationParity(): Boolean = {
      true
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
    isValidCenters                 && isValidPermutationParity &&
    isValidCornerOrientationParity && isValidSideOrientationParity
  }
}