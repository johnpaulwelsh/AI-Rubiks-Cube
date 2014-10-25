/**
 * Common utility functions that may be used at any time.
 * Can be imported into each file that wants to use them,
 * or called by common.function_name syntax.
 *
 * @author  John Paul Welsh
 */
object common {

  /*
   *
   * Type aliases for use in making a Rubik's Cube.
   *
   */

  type Cubie = Array[Char]
  type Cube = Array[Cubie]

  /*
   *
   * "Getter" and "setter" functions for Rubik's Cube.
   *
   */

  /**
   * Transforms a cross-shaped input into a cubie-divided cube.
   * The first element of a cubie is always the color resting on the x-axis,
   * and the second and third rest on the y- and z-axes, respectively.
   * If the cubie does not have a color on a certain axis, that spot is filled
   * with a lowercase 'x'.
   *
   * @param ls  an array of Strings, each one representing a row of the input
   * @return  a Cube object
   */
  def arrangeInput(ls: Array[String]): Cube = {
    val cube = Array.ofDim[Cubie](20)

    // corner cubies first, since they actually hold 3 colors    
    //               x                y                z
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
   *
   * @return  a solved Rubik's Cube
   */
  def setSolvedCube(): Cube = {
    val solvedCube = Array.ofDim[Cubie](20)

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
   *
   * @param c  a cube
   * @return  an array of the corner cubies
   */
  def getCubeCornerList(c: Cube): Array[Cubie] = Array(c(0), c(2), c(5), c(7), c(12), c(14), c(17), c(19))

  /**
   * An array of all the side cubies in the given Cube.
   *
   * @param c  a cube
   * @return   an array of the side cubies
   */
  def getCubeSideList(c: Cube): Array[Cubie] = Array(c(1), c(3), c(4), c(6), c(8), c(9), c(10), c(11), c(13), c(15), c(16), c(18))

  /**
   * An array of the first 6 side cubies in the given cube.
   *
   * @param c  a cube
   * @return   an array of the first 6 side cubies
   */
  def getFirstHalfOfCubeSides(c: Cube): Array[Cubie] = Array(c(1), c(3), c(4), c(6), c(8), c(9))

  /**
   * An array of the last 6 side cubies in the given cube.
   *
   * @param c  a cube
   * @return   an array of the first 6 side cubies
   */
  def getSecondHalfOfCubeSides(c: Cube): Array[Cubie] = Array(c(10), c(11), c(13), c(15), c(16), c(18))




  def getOrientationOfCorner(c: Cubie): Int = {
    0
  }




  def getOrientationOfSide(c: Cubie): Int = {
    0
  }

  /*
   *
   * Functions for making legal moves on a cube.
   *
   */

  /**
   * Makes an 'up' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_U(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(2), cubie(1), cubie(0))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(0))
      cube(0) = orient(cube(5))
      cube(5) = orient(cube(7))
      cube(7) = orient(cube(2))
      cube(2) = tempCorner

      val tempSide = orient(cube(1))
      cube(1) = orient(cube(3))
      cube(3) = orient(cube(6))
      cube(6) = orient(cube(4))
      cube(4) = tempSide
    }
    cube
  }

  /**
   * Makes a 'down' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_D(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(2), cubie(1), cubie(0))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(17))
      cube(17) = orient(cube(12))
      cube(12) = orient(cube(14))
      cube(14) = orient(cube(19))
      cube(19) = tempCorner

      val tempSide = orient(cube(18))
      cube(18) = orient(cube(15))
      cube(15) = orient(cube(13))
      cube(13) = orient(cube(16))
      cube(16) = tempSide
    }
    cube
  }

  /**
   * Makes a 'left' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_L(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(0), cubie(2), cubie(1))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(0))
      cube(0) = orient(cube(12))
      cube(12) = orient(cube(17))
      cube(17) = orient(cube(5))
      cube(5) = tempCorner

      val tempSide = orient(cube(3))
      cube(3) = orient(cube(8))
      cube(8) = orient(cube(15))
      cube(15) = orient(cube(10))
      cube(10) = tempSide
    }
    cube
  }

  /**
   * Makes a 'right' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_R(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(0), cubie(2), cubie(1))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(7))
      cube(7) = orient(cube(19))
      cube(19) = orient(cube(14))
      cube(14) = orient(cube(2))
      cube(2) = tempCorner

      val tempSide = orient(cube(4))
      cube(4) = orient(cube(11))
      cube(11) = orient(cube(16))
      cube(16) = orient(cube(9))
      cube(9) = tempSide
    }
    cube
  }

  /**
   * Makes a 'front' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_F(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(1), cubie(0), cubie(2))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(5))
      cube(5) = orient(cube(17))
      cube(17) = orient(cube(19))
      cube(19) = orient(cube(7))
      cube(7) = tempCorner

      val tempSide = orient(cube(6))
      cube(6) = orient(cube(10))
      cube(10) = orient(cube(18))
      cube(18) = orient(cube(11))
      cube(11) = tempSide
    }
    cube
  }

  /**
   * Makes a 'back' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_B(cube: Cube, turn: Int): Cube = {

    def orient(cubie: Cubie): Cubie = Array(cubie(1), cubie(0), cubie(2))

    for (i <- 0 until turn) {
      val tempCorner = orient(cube(2))
      cube(2) = orient(cube(14))
      cube(14) = orient(cube(12))
      cube(12) = orient(cube(0))
      cube(0) = tempCorner

      val tempSide = orient(cube(1))
      cube(1) = orient(cube(9))
      cube(9) = orient(cube(13))
      cube(13) = orient(cube(8))
      cube(8) = tempSide
    }
    cube
  }

  /*
   *
   * Utility functions, general purpose.
   *
   */

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }
}