package rubik

/**
 * Common utility functions that may be used at any time.
 * Must be imported into each file that wants to use them.
 * 
 * @author: John Paul Welsh
 */
object common {
  type Cubie = Array[Char]
  type Cube = Array[Cubie]

  /**
   * Transforms a cross-shaped input into a cubie-divided cube.
   */
  def arrangeInput(ls: Array[String]): Cube = {
    var cube = Array.ofDim[Cubie](20)    
    
    // do shit here
    
    cube
  }
  
  /**
   * Fills a cube instance to represent the solved state.
   * 
   * The first element of a cubie is always the color resting on the x-axis,
   * and the second and third rest on the y- and z-axes, respectively.
   * If the cubie does not have a clor on a certain axis, that spot is filled
   * with a lowercase 'x'.
   */
  def setSolvedCube(): Cube = {
    var solvedCube = Array.ofDim[Cubie](20)

    // Corner cubies
    //                      x-   y-   z- axes
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
    
    // Return back the finished Cube
    solvedCube
  }
  
  /**
   * Prints the given Cube in a readable way.
   */
  def printCube(cube: Cube) {
    println("Not even close baby")
  }
}