package rubik

import common._

class CubeValidator {

  def isValidCube(c: Cube): Boolean = {
    def isValidCenters(): Boolean = {
      c(0)(4) == 'R' &&
      c(1)(4) == 'G' &&
      c(2)(4) == 'Y' &&
      c(3)(4) == 'B' &&
      c(4)(4) == 'O' &&
      c(5)(4) == 'W'
    }
    
    def isValidCorners(): Boolean = true
    
    def isValidSides(): Boolean = true
    
    isValidCenters && isValidCorners && isValidSides
  }
}