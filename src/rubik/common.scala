package rubik

/**
 * Common utility functions that may be used at any time.
 * Can be imported into each file that wants to use them,
 * or called by common.function_name syntax.
 * 
 * @author  John Paul Welsh
 */
object common {
  type Cubie = Array[Char]
  type Cube = Array[Cubie]

  /**
   * Transforms a cross-shaped input into a cubie-divided cube.
   * 
   * The first element of a cubie is always the color resting on the x-axis,
   * and the second and third rest on the y- and z-axes, respectively.
   * If the cubie does not have a clor on a certain axis, that spot is filled
   * with a lowercase 'x'.
   */
  def arrangeInput(ls: Array[String]): Cube = {
    var cube = Array.ofDim[Cubie](20)
    
    // corner cubies first, since they actually hold 3 colors    
    //              x                y                z
    cube(0)  = Array(ls(3).charAt(0), ls(0).charAt(0), ls(11).charAt(0))
    cube(2)  = Array(ls(3).charAt(8), ls(0).charAt(2), ls(11).charAt(2))
    cube(5)  = Array(ls(3).charAt(2), ls(2).charAt(0), ls(3).charAt(3))
    cube(7)  = Array(ls(3).charAt(6), ls(2).charAt(2), ls(3).charAt(5))
    
    cube(12) = Array(ls(5).charAt(0), ls(8).charAt(0), ls(9).charAt(0))
    cube(14) = Array(ls(5).charAt(8), ls(8).charAt(2), ls(9).charAt(2))
    cube(17) = Array(ls(5).charAt(2), ls(6).charAt(0), ls(5).charAt(3))
    cube(19) = Array(ls(5).charAt(6), ls(6).charAt(2), ls(5).charAt(5))
    
    // side cubies (with x's in the nonexistent spots)
    //              x                 y                z
    cube(1) = Array('x',             ls(0).charAt(1), ls(11).charAt(1))
    cube(3) = Array(ls(3).charAt(1), ls(1).charAt(0), 'x')
    cube(4) = Array(ls(3).charAt(7), ls(1).charAt(2), 'x')
    cube(6) = Array('x',             ls(2).charAt(1), ls(3).charAt(4))
    
    cube(8)  = Array(ls(4).charAt(0), 'x', ls(10).charAt(0))
    cube(9)  = Array(ls(4).charAt(8), 'x', ls(10).charAt(2))
    cube(10) = Array(ls(4).charAt(2), 'x', ls(4).charAt(3))
    cube(11) = Array(ls(4).charAt(6), 'x', ls(4).charAt(5))
    
    cube(13) = Array('x',             ls(8).charAt(1), ls(9).charAt(1))
    cube(15) = Array(ls(5).charAt(1), ls(7).charAt(0), 'x')
    cube(16) = Array(ls(5).charAt(7), ls(7).charAt(2), 'x')
    cube(18) = Array('x',             ls(6).charAt(1), ls(5).charAt(4))
    
    // Return the arranged Cube
    cube
  }
  
  /**
   * Fills a cube instance to represent the solved state.
   */
  def setSolvedCube(): Cube = {
    var solvedCube = Array.ofDim[Cubie](20)

    // Corner cubies
    //                      x    y    z
    solvedCube(0)  = Array('G', 'R', 'W')
    solvedCube(2)  = Array('B', 'R', 'W')
    solvedCube(5)  = Array('G', 'R', 'Y')
    solvedCube(7)  = Array('B', 'R', 'Y')
    
    solvedCube(12) = Array('G', 'O', 'W')
    solvedCube(14) = Array('B', 'O', 'W')
    solvedCube(17) = Array('G', 'O', 'Y')
    solvedCube(19) = Array('B', 'O', 'Y')
    
    // Side cubies
    solvedCube(1)  = Array('x', 'R', 'W')
    solvedCube(3)  = Array('G', 'R', 'x')
    solvedCube(4)  = Array('B', 'R', 'x')
    solvedCube(6)  = Array('x', 'R', 'Y')
    
    solvedCube(8)  = Array('G', 'x', 'W')
    solvedCube(9)  = Array('B', 'x', 'W')
    solvedCube(10) = Array('G', 'x', 'Y')
    solvedCube(11) = Array('B', 'x', 'Y')
    
    solvedCube(13) = Array('x', 'O', 'W')
    solvedCube(15) = Array('G', 'O', 'x')
    solvedCube(16) = Array('B', 'O', 'x')
    solvedCube(18) = Array('x', 'O', 'Y')
    
    // Return the finished Cube
    solvedCube
  }
  
  /**
   * An array of all the corner cubies in the given Cube.
   */
  def getCubeCornerList(c: Cube): Array[Cubie] = Array(c(0), c(2), c(5), c(7), c(12), c(14), c(17), c(19))
  
  /**
   * An array of all the corner cubies in the given Cube.
   */
  def getCubeSideList(c: Cube): Array[Cubie] = Array(c(1), c(3), c(4), c(6), c(8), c(9), c(10), c(11), c(13), c(15), c(16), c(18))
  
  /**
   * Prints the given index list.
   */
  def printIndices(ls: Array[Int]) {
    for (elem <- ls) print(elem + ", ")
    println()
  }
  
  /**
   * Prints the given sequence of cubies.
   */
  def printSeq(seq: Array[Cubie]) {
    for (c <- seq) println(c(0) + ", " + c(1) + ", " + c(2))
  }
}