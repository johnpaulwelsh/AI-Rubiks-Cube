package rubik

import common._

/**
 * Collection of functions that check whether the input Cube is a valid arrangement.
 * 
 * @author: John Paul Welsh
 */
class CubeValidator {

  def isValidCenters(ls: Array[String]): Boolean = {
    val centerList: List[Char] = List(ls(1).charAt(1)) :::
                                 List(ls(4).charAt(1)) :::
                                 List(ls(4).charAt(4)) :::
                                 List(ls(4).charAt(7)) :::
                                 List(ls(7).charAt(1)) :::
                                 List(ls(10).charAt(1)).map(x => x.asInstanceOf[Char])                                 
    centerList == List('R', 'G', 'Y', 'B', 'O', 'W')
  }
  
  def isValidCube(c: Cube): Boolean = {
    
    def isValidPermutationParity(): Boolean = {
      true
    }
    
    def isValidCornerOrientationParity(): Boolean = {
      
      var flag = true
      val cornerList: List[Cubie] = Array( c(0), c(2), c(5), c(7), c(12), c(14), c(17), c(19) ).toList
      
      // For every cubie, its z-axis element (2) better be either R or O
//      for (cubie <- cornerList) {
//        flag = (cubie(2) != 'R' && cubie(2) != 'O')
//      }
      
      // Return the state of the flag at the end
      flag
    }
    
    def isValidSideOrientationParity(): Boolean = {
      
      var flag = true
      val sideList: List[Cubie] = Array( c(1),  c(3),  c(4),  c(6),  c(8),  c(9),
                                         c(10), c(11), c(13), c(15), c(16), c(18) ).toList
      true
    }
    
    // Returns true when all tests pass, false if one or more fails
    isValidPermutationParity && isValidCornerOrientationParity && isValidSideOrientationParity
  }
}