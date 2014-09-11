package rubik

/**
 * Common utility functions that may be used at any time.
 * Must be imported into each file that wants to use them.
 */
object common {
  type Face = Array[Char]
  type Cube = Array[Face]

  /**
   * Transforms a cross-shaped input into a face-divided cube.
   */
  def arrangeInput(ls: Array[String]): Cube = {
    
    def fillSpots(ls2: Cube): Cube = {
      // Red
      ls2(0)(0) = ls(0).charAt(0)
      ls2(0)(1) = ls(0).charAt(1)
      ls2(0)(2) = ls(0).charAt(2)
      ls2(0)(3) = ls(1).charAt(0)
      ls2(0)(4) = ls(1).charAt(1)
      ls2(0)(5) = ls(1).charAt(2)
      ls2(0)(6) = ls(2).charAt(0)
      ls2(0)(7) = ls(2).charAt(1)
      ls2(0)(8) = ls(2).charAt(2)
      // Green
      ls2(1)(0) = ls(3).charAt(0)
      ls2(1)(1) = ls(3).charAt(1)
      ls2(1)(2) = ls(3).charAt(2)
      ls2(1)(3) = ls(4).charAt(0)
      ls2(1)(4) = ls(4).charAt(1)
      ls2(1)(5) = ls(4).charAt(2)
      ls2(1)(6) = ls(5).charAt(0)
      ls2(1)(7) = ls(5).charAt(1)
      ls2(1)(8) = ls(5).charAt(2)
      // Yellow
      ls2(2)(0) = ls(3).charAt(3)
      ls2(2)(1) = ls(3).charAt(4)
      ls2(2)(2) = ls(3).charAt(5)
      ls2(2)(3) = ls(4).charAt(3)
      ls2(2)(4) = ls(4).charAt(4)
      ls2(2)(5) = ls(4).charAt(5)
      ls2(2)(6) = ls(5).charAt(3)
      ls2(2)(7) = ls(5).charAt(4)
      ls2(2)(8) = ls(5).charAt(5)
      // Blue
      ls2(3)(0) = ls(3).charAt(6)
      ls2(3)(1) = ls(3).charAt(7)
      ls2(3)(2) = ls(3).charAt(8)
      ls2(3)(3) = ls(4).charAt(6)
      ls2(3)(4) = ls(4).charAt(7)
      ls2(3)(5) = ls(4).charAt(8)
      ls2(3)(6) = ls(5).charAt(6)
      ls2(3)(7) = ls(5).charAt(7)
      ls2(3)(8) = ls(5).charAt(8)
      // Orange
      ls2(4)(0) = ls(6).charAt(0)
      ls2(4)(1) = ls(6).charAt(1)
      ls2(4)(2) = ls(6).charAt(2)
      ls2(4)(3) = ls(7).charAt(0)
      ls2(4)(4) = ls(7).charAt(1)
      ls2(4)(5) = ls(7).charAt(2)
      ls2(4)(6) = ls(8).charAt(0)
      ls2(4)(7) = ls(8).charAt(1)
      ls2(4)(8) = ls(8).charAt(2)
      // White
      ls2(5)(0) = ls(9).charAt(0)
      ls2(5)(1) = ls(9).charAt(1)
      ls2(5)(2) = ls(9).charAt(2)
      ls2(5)(3) = ls(10).charAt(0)
      ls2(5)(4) = ls(10).charAt(1)
      ls2(5)(5) = ls(10).charAt(2)
      ls2(5)(6) = ls(11).charAt(0)
      ls2(5)(7) = ls(11).charAt(1)
      ls2(5)(8) = ls(11).charAt(2)
      
      ls2
    }

    var cube = Array.ofDim[Face](6)

    // put a face into each 
    for (i <- 0 to 5) { cube(i) = new Face(9) }
    
    fillSpots(cube)
  }
  
  /**
   * Prints the arranged cube in a face-divided and readable way.
   */
  def printCube(cube: Cube): Unit = {
    for (face <- cube) {
      for (elem <- face) {
        print(elem)
      }
      println()
    }
  }
}